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

main :: IO ()
main = do
  -- TODO: Should protect this with exception handling
  poutJerseyHtmlStr :: String <- readFile "html/pout-jersey.html"
  let poutJerseyHtmlText :: L.Text = L.pack poutJerseyHtmlStr
  putStrLn (take 100 poutJerseyHtmlStr)
  scotty 3000 $ do
    -- The jobcoin homepage
    get "/pout-jersey" $ do
      trace "Jobcoin home page\n" (html poutJerseyHtmlText)
    get "/" $ do                         -- handle GET request on "/" URL
      trace "GET request!\n" (text "This was a GET request!\n") -- send 'text/plain' response
      ps <- params
      text (if null ps then "NULL\n" else (fst (head ps)))
  --  delete "/" $ do
  --    trace "DELETE request!\n" (html "This was a DELETE request!\n")  -- send 'text/html' response
  --  post "/" $ do
  --    trace "POST request!\n" (text "This was a POST request!\n")
  --  put "/" $ do
  --    trace "PUT request!\n" (text "This was a PUT request!\n")
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
      text "There is no such route.\n"

-- main :: IO ()
-- main = someFunc
