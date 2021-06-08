{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Lib
import Debug.Trace
import Web.Scotty
import Network.HTTP.Types
import Network.Wai
import Data.Aeson
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Char8 as C

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
    get "/pout-jersey" $ do trace "Jobcoin home page\n" serveHtml
    get "/" $ do                         -- handle GET request on "/" URL
      trace "GET request!\n" (text "This was a GET request!\n") -- send 'text/plain' response
      ps <- params
      trace "NULL check\n" (text (if null ps then "NULL parameters\n" else (fst (head ps))))
    -- named parameters:
    get "/askfor/:word\n" $ do
      w <- param "word"
      html $ mconcat ["<h1>You asked for ", w, ", you got it!</h1>" ]
    -- unnamed parameters from a query string or a form:
    post "/submit" $ do  -- e.g. http://server.com/submit?name=somename
      name <- param "name"
      text name
    -- match a route regardless of the method
  --  trace "ANY /all" (matchAny "/all") $ do
  --    text "matches all methods"
        -- handler for when there is no matched route
        -- (this should be the last handler because it matches all routes)
    notFound $ do
      r <- request
      trace ("No such route\n" ++ show r) (text "There is no such route.\n")

-- main :: IO ()
-- main = someFunc
