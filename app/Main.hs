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
import Text.Blaze hiding (text)
import Text.Blaze.Html hiding (text) -- Conflicts with Scotty
import Text.Blaze.Html.Renderer.String

import qualified Data.ByteString as B
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.String
import Data.Default.Class
import Data.Time.Clock
import Coins
import State
import Address
import Assoc
import HomePage

-- Why 'ReaderT (TVar AppState)' rather than 'StateT AppState'?
-- With a state transformer, 'runActionToIO' (below) would have
-- to provide the state to _every action_, and save the resulting
-- state, using an MVar. This means actions would be blocking,
-- meaning only one request could be serviced at a time.
-- The 'ReaderT' solution means only actions that actually modify
-- the state need to block/retry.
--
-- Scotty requires the monad to be an instance of MonadIO
newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

-- Scotty's monads are layered on top of our custom monad.
-- We define this synonym for lift in order to be explicit
-- about when we are operating at the 'WebM' layer.
webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- Get the State
gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

-- Modify the State
modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

-- Our internal state for the jobcoin demo
instance Default AppState where
    def = initAppState

-- Serve an HTML file
serveHtml :: ActionT L.Text WebM ()
serveHtml = do
  r <- request
  let rawpath :: String = C.unpack (rawPathInfo r)
      path :: String = "html" ++ rawpath ++ ".html"
  -- liftIO (putStrLn (("serveHtml --> ") ++ path))
  setHeader "Content-Type" "text/html"
  file path

-- Get the account value for the specified user/address
getLedgerValue :: Address -> ActionT L.Text WebM Amount
getLedgerValue addr = do
  l <- webM $ gets appLedger
  return (getValue addr l)

-- Set up to call the web server.
main :: IO ()
main = do
  putStrLn (renderHtml (homePage "Peter White" emptyLedger))
  sync <- newTVarIO def
  -- 'runActionToIO' is called once per action.
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyT 3000 runActionToIO app

-- A debugging print out
debugit :: String -> ActionT L.Text WebM ()
debugit = liftIO . putStrLn

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
  get "/plusone" $ do -- TODO: Remove
    webM $ modify $ \ st -> st { tickCount = tickCount st + 1 }
    t <- webM $ gets appLedger
    liftIO (putStrLn (("...... tickCount = ") ++ show t))
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
  get "/pout-jersey" $ do -- do serveHtml
    ledger <- webM $ gets appLedger
    html (L.pack (renderHtml (homePage "Peter White" ledger)))
  get "/pout-jersey/api" $ serveHtml
  post "/pout-jersey/create" $ do
    ps <- params
    let (Just addr) = ps Assoc.! "address" -- TODO: Dangerous
    webM $ modify $ \ st -> add50 (L.unpack addr) st
    value <- webM $ gets (getAppValue (L.unpack addr))
    redirect "/pout-jersey"
  get "/pout-jersey/addresses/:addr" $ do
    setHeader "Content-Type" "application/json"
    addr :: String <- param "addr"
    value <- webM $ gets (getAppValue addr)
    let response :: String = addr ++ " has " ++ show value ++ " jobcoins"
    text (L.pack response)
  notFound $ do
    text "There is no such route.\n"
