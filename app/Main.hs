{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Debug.Trace
import Web.Scotty
import Network.HTTP.Types

main = scotty 3000 $ do
  get "/" $ do                         -- handle GET request on "/" URL
    trace "GET request!" (text "This was a GET request!")     -- send 'text/plain' response
  delete "/" $ do
    html "This was a DELETE request!"  -- send 'text/html' response
  post "/" $ do
    text "This was a POST request!"
  put "/" $ do
    text "This was a PUT request!"

-- main :: IO ()
-- main = someFunc
