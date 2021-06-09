{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Lib
import Debug.Trace
import Web.Scotty
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Data.Aeson as Aeson
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as ST
import Text.Blaze.Html hiding (text) -- Conflicts with Scotty

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Time.Clock
import Coins

-- Read a file into a Text string
readLazyByteStringFile :: String -> IO L.Text
readLazyByteStringFile fileName = do
  str :: String <- readFile fileName
  return (L.pack str)

-- Serve an HTML
serveHtml :: ActionM ()
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
  homeHtml :: L.Text <- readLazyByteStringFile "html/pout-jersey.html"
  scotty 3000 $ do
    middleware logStdoutDev
    -- Get other html files
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
    get "/pout-jersey/addresses/:addr" $ do
      setHeader "Content-Type" "application/json"
      addr :: String <- param "addr"
      let addrstr :: String = "addesses/" ++ addr
      text (L.pack addrstr)
    notFound $ do
      r <- request
      text "There is no such route.\n"
    -- Handle request to get information on a user/addresses
-- main :: IO ()
-- main = someFunc
