{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Lib
import Debug.Trace
import Web.Scotty
import Network.HTTP.Types
import Network.Wai
import Data.Aeson as Aeson
import Control.Monad
import Control.Monad.IO.Class
import Text.Blaze.Html hiding (text) -- Conflicts with Scotty

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Time.Clock
import Coins

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

main :: IO ()
main = do
  -- TODO: Should protect this with exception handling
  homeHtml :: L.Text <- readLazyByteStringFile "html/pout-jersey.html"
  scotty 3000 $ do
    -- Get other html files
    get (regex "^/(.*).html$") $ do
      setHeader "Content-Type" "text/html"
      path <- param "0"
      trace ("/html/" ++ path) (file ("html/" ++ path)) -- a la Sinatra
    -- Get css files from assets/stylesheets
    get "/assets/stylesheets/:css" $ do
      setHeader "Content-Type" "text/css"
      -- Sinatra style route and capture.
      css <- param "css"
      trace ("/assets/stylesheets/" ++ css)
            (file ("assets/stylesheets/" ++ css))
    -- Get html files without a .html suffix on the link
    -- I was having trouble getting apache to serve /pout-jersey,
    -- pout-jersey/api, and so on.
    get "/pout-jersey" $ do trace "Jobcoin home page\n" serveHtml
    get "/pout-jersey/api" $ do trace "API home page\n" serveHtml
    get "/pout-jersey/addresses/:addr" $ do
      setHeader "Content-Type" "application/json"
      addr :: String <- param "addr"
      let addrstr :: String = "addesses/" ++ addr
      text (L.pack addrstr)
    notFound $ do
      r <- request
      trace ("No such route\n" ++ show r) (text "There is no such route.\n")
    -- Handle request to get information on a user/addresses
-- main :: IO ()
-- main = someFunc
