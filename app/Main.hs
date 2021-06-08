{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Lib
import Debug.Trace
import Web.Scotty
import Network.HTTP.Types
import Data.Aeson
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

readLazyByteStringFile :: String -> IO L.Text
readLazyByteStringFile fileName = do
  str :: String <- readFile fileName
  return (L.pack str)

main :: IO ()
main = do
  -- TODO: Should protect this with exception handling
  homeHtml :: L.Text <- readLazyByteStringFile "html/pout-jersey.html"
  mainCss :: L.Text <- readLazyByteStringFile "assets/stylesheets/main.css"
  scotty 3000 $ do
    -- The jobcoin homepage
    get "/pout-jersey" $ do
      r <- request
      trace "Jobcoin home page\n" (html homeHtml)
    -- Get css files from assets/stylesheets
    get "/assets/stylesheets/:css" $ do
      setHeader "Content-Type" "text/css"
      -- Sinatra style route and capture.
      css <- param "css"
      trace ("/assets/stylesheets/" ++ css)
            (file ("assets/stylesheets/" ++ css))
    get "/" $ do                         -- handle GET request on "/" URL
      trace "GET request!\n" (text "This was a GET request!\n") -- send 'text/plain' response
      ps <- params
      trace "NULL check\n" (text (if null ps then "NULL parameters\n" else (fst (head ps))))
    delete "/" $ do
      trace "DELETE request!\n" (html "This was a DELETE request!\n")  -- send 'text/html' response
    post "/" $ do
      trace "POST request!\n" (text "This was a POST request!\n")
    put "/" $ do
      trace "PUT request!\n" (text "This was a PUT request!\n")
    get "/one" $ do
      -- status status302  -- Respond with HTTP 302 status code
      -- setHeader "Location" "http://lonliest-number"
      trace "GET/one request!\n" (text "This was a GET/one request!\n")     -- send 'text/plain' response
  --  post "/set-headers" $ do
  --    trace "POST /set_headers location 302\n" (status status302)  -- Respond with HTTP 302 status code
  --    setHeader "Location" "http://www.google.com.au"
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
