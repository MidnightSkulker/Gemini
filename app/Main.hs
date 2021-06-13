{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Main where

import Lib
import Web.Scotty.Trans
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types.Status
import WebM
import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Reader
import Text.Blaze hiding (text)
import Text.Blaze.Html hiding (text) -- Conflicts with Scotty
import Text.Blaze.Html.Renderer.String

import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.String
import Data.Default.Class
import Data.Time.Clock
import Coins
import State
import Address
import Assoc
import Log
import HomePage

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
  -- putStrLn (renderHtml (homePage "Peter White" emptyLedger emptyLog))
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

  get "/" $ do
    redirect "/pout-jersey"

  -- Serve other HTML files
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
    -- TODO: Possible time window here?
    -- We are getting two items from the global state
    -- (each in an atomic manner)
    -- It is possible for another API call to happen between these two.
    -- I will need to generalize the series of actions, and make the
    -- series of actions atomically.
    -- FIXED! Created a webM combinator that gets both parts at once.
    (ledger, log) <- webM $ gets2 appLedger appLog
    errMsgs <- webM $ gets lastErrors
    when (not (null errMsgs)) $ webM $ modify $ \ st -> removeErrors st
    html (L.pack (renderHtml (homePage "Peter White" errMsgs ledger log)))

  get "/pout-jersey/api" $ serveHtml

  post "/pout-jersey/send" $ do
    ps <- params
    -- Check the validity of the send call, and return a list of
    -- error messages. An empty list means there was no error.
    let paramErrors :: String -> String -> String -> [String]
        paramErrors fromAddr toAddr amountStr =
          let ret1 :: [String] = if not (isFloat amountStr)
                                 then ["The amount is not a valid floating point number"]
                                 else []
              ret2 :: [String] = if null fromAddr
                                 then ret1 ++ ["The sender address cannot be null"]
                                 else ret1
              ret3 :: [String] = if null toAddr
                                 then ret2 ++ ["The receiver address cannot be null"]
                                 else ret2
          in ret3
        validParams :: String -> String -> String -> Bool
        validParams fromAddr toAddr amountStr = paramErrors fromAddr toAddr amountStr == []
        params :: String = show ps
    -- Fetch the parameters
    currentTime :: UTCTime <- liftIO getCurrentTime
    fromAddr :: String <- param "fromAddress"
    toAddr :: String <- param "toAddress"
    amountStr :: String <- param "amount"
    -- When there are not errors, do the send transaction.
    if (validParams fromAddr toAddr amountStr) then do
      -- We know the amountStr is a valid float at this point, so
      -- the conversion to float should not cause an error.
      let amount :: Float = read amountStr
      -- TODO: The following code does an access to the app state,
      -- followed by three modifications the to the app state. To
      -- make this safe(r), we need to encode this action down at
      -- the Software Transactional Memory (STM) level, and then
      -- perform the whole sequence atomically.
      --
      -- Find out how much the sender has.
      value <- webM $ gets (getAppValue fromAddr)
      -- Enter the transaction into the log
      webM $ do
        -- Find out if the sender has sufficient funds
        if amount <= value then do
          -- Add the transaction to the log
          modify $ \ st -> addAppSendTransaction currentTime fromAddr toAddr amount st
          -- Transfer the funds from sender to receiver
          modify $ \ st -> appAddValue fromAddr (-amount) st
          modify $ \ st -> appAddValue toAddr amount st
        else return ()
    else do
      status status403
      webM $ modify $ \ st -> setErrors (paramErrors fromAddr toAddr amountStr) st
    redirect "/pout-jersey"

  post "/pout-jersey/create" $ do
    ps <- params
    let (Just addr) = ps Assoc.! "address" -- TODO: Dangerous
        addrStr :: String = L.unpack addr
    currentTime :: UTCTime <- liftIO getCurrentTime
    -- Make sure address is not null
    if (not (null addrStr)) then do
      -- Create the funds
      webM $ modify $ \ st -> appAddValue addrStr 50.0 st
      webM $ modify $ \ st -> addAppCreateTransaction currentTime addrStr 50.0 st
    else do
      status status403
      webM $ modify $ \ st -> setErrors ["Null address for create request"] st

    redirect "/pout-jersey"

  get "/pout-jersey/addresses/:addr" $ do
    setHeader "Content-Type" "application/json"
    addr :: String <- param "addr"
    amount <- webM $ gets (getAppValue addr)
    entries <- webM $ gets (getAppTransactions addr)
    -- json :: ToJSON a => a -> ActionM ()
    json ( TransactionReport { balance = amount, transactions = entries } )

  notFound $ do
    r <- request
    let path :: C.ByteString = rawPathInfo r
    text (L.pack ("There is no such route: " ++ (C.unpack path) ++ "\n"))
