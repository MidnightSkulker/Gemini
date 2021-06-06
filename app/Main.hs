{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Debug.Trace
import Web.Scotty
import Network.HTTP.Types
import Data.Aeson

main = scotty 3000 $ do
  get "/" $ do                         -- handle GET request on "/" URL
    trace "GET request!\n" (text "This was a GET request!") -- send 'text/plain' response
  delete "/" $ do
    trace "DELETE request!\n" (html "This was a DELETE request!")  -- send 'text/html' response
  post "/" $ do
    trace "POST request!\n" (text "This was a POST request!")
  put "/" $ do
    trace "PUT request!\n" (text "This was a PUT request!")
    -- set a header:
  get "/one" $ do
    -- status status302  -- Respond with HTTP 302 status code
    -- setHeader "Location" "http://lonliest-number"
    trace "GET/one request!\n" (text "This was a GET/one request!")     -- send 'text/plain' response
  post "/set-headers" $ do
    trace "POST /set_headers location 302" (status status302)  -- Respond with HTTP 302 status code
    setHeader "Location" "http://www.google.com.au"
  -- named parameters:
  trace "GET /askfor/:word" (get "/askfor/:word") $ do
    w <- param "word"
    html $ mconcat ["<h1>You asked for ", w, ", you got it!</h1>" ]
  -- unnamed parameters from a query string or a form:
  trace "POST /submit" (post "/submit") $ do  -- e.g. http://server.com/submit?name=somename
    name <- param "name"
    text name
  -- match a route regardless of the method
  trace "ANY /all" (matchAny "/all") $ do
    text "matches all methods"
    -- handler for when there is no matched route
    -- (this should be the last handler because it matches all routes)
  notFound $ do
    text "there is no such route."

-- main :: IO ()
-- main = someFunc
