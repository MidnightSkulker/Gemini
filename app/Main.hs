{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Main where

import Lib
import Debug.Trace
-- import Web.Scotty
import Web.Scotty.Trans
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Data.Aeson as Aeson
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Monad.Reader
-- import qualified Control.Monad.Trans.State as ST
import Text.Blaze.Html hiding (text) -- Conflicts with Scotty

import qualified Data.ByteString as B
-- import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.String
import Data.Default.Class
import Data.Time.Clock
import Coins
import State
import Assoc

-- Why 'ReaderT (TVar AppState)' rather than 'StateT AppState'?
-- With a state transformer, 'runActionToIO' (below) would have
-- to provide the state to _every action_, and save the resulting
-- state, using an MVar. This means actions would be blocking,
-- effectively meaning only one request could be serviced at a time.
-- The 'ReaderT' solution means only actions that actually modify
-- the state need to block/retry.
--
-- Also note: your monad must be an instance of 'MonadIO' for
-- Scotty to use it.
newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

-- Scotty's monads are layered on top of our custom monad.
-- We define this synonym for lift in order to be explicit
-- about when we are operating at the 'WebM' layer.
webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- Some helpers to make this feel more like a state monad.
gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

-- Our internal state
instance Default AppState where
    def = initAppState

-- Read a file into a Text string
readLazyByteStringFile :: String -> IO L.Text
readLazyByteStringFile fileName = do
  str :: String <- readFile fileName
  return (L.pack str)

-- Serve an HTML
serveHtml :: ActionT L.Text WebM ()
serveHtml = do
  r <- request
  let rawpath :: String = C.unpack (rawPathInfo r)
      path :: String = "html" ++ rawpath ++ ".html"
  liftIO (putStrLn (("serveHtml --> ") ++ path))
  setHeader "Content-Type" "text/html"
  file path

-- type ScottyST = ST.StateT String ScottyM ()
-- scottyST :: ScottyST -> IO () = liftM scotty

main :: IO ()
main = do
    sync <- newTVarIO def
        -- 'runActionToIO' is called once per action.
    let runActionToIO m = runReaderT (runWebM m) sync

    scottyT 3000 runActionToIO app

-- This app doesn't use raise/rescue, so the exception
-- type is ambiguous. We can fix it by putting a type
-- annotation just about anywhere. In this case, we'll
-- just do it on the entire app.
app :: ScottyT L.Text WebM ()
app = do
  middleware logStdoutDev
  -- Test out getting the root.
  get "/" $ do
      c <- webM $ gets tickCount
      text $ fromString $ show c
  get "/plusone" $ do
    webM $ modify $ \ st -> st { tickCount = tickCount st + 1 }
    redirect "/"
  -- Server other HTML files
  get (regex "^/(.*).html$") $ do
    setHeader "Content-Type" "text/html"
    path <- param "0"
    file ("html/" ++ path) -- a la Sinatra
  -- Get css files from assets/stylesheets
  get "/assets/stylesheets/:css" $ do
    setHeader "Content-Type" "text/css"
    -- Sinatra style route and capture.
    css <- param "css"
    file ("assets/stylesheets/" ++ css)
  -- Get html files without a .html suffix on the link
  -- I was having trouble getting apache to serve /pout-jersey,
  -- pout-jersey/api, and so on.
  get "/pout-jersey" $ do serveHtml
  get "/pout-jersey/api" $ do serveHtml
  get "/pout-jersey/create" $ do
    ps <- params
    text (L.pack ("Under construction\n" ++ (getParameter ps "address")))
  get "/pout-jersey/addresses/:addr" $ do
    setHeader "Content-Type" "application/json"
    addr :: String <- param "addr"
    let addrstr :: String = "addesses/" ++ addr
    text (L.pack addrstr)
  notFound $ do
    r <- request
    text "There is no such route.\n"
