{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
module State (
  AppState(..),
  initAppState,
  getAppValue,
  appAddValue,
  addAppSendTransaction,
  addAppCreateTransaction,
  getAppTransactions,
  getAllAppTransactions,
  removeErrors,
  setError,
  setErrors ) where

import Log
import Address
import Coins
import Debug.Trace
import Data.Aeson
import Data.Time.Clock
import GHC.Generics

-- The global state for the Server
data AppState = AppState
  { appLog :: Log,
    appLedger :: Ledger,
    lastErrors :: [String] -- Most recent error messages
  } deriving (Show, Generic, ToJSON)

initAppState = AppState {
  appLog = emptyLog,
  appLedger = emptyLedger,
  -- Last error that occured
  lastErrors = [] }

-- remove the last error message
removeErrors :: AppState -> AppState
removeErrors appState = appState { lastErrors = [] }

-- Add error messages
setErrors :: [String] -> AppState -> AppState
setErrors errorMessages appState = appState { lastErrors = errorMessages }

-- Add one error message
setError :: String -> AppState -> AppState
setError errorMessage appState = setErrors [errorMessage] appState

-- Add coins to a ledger
appAddValue :: Address -> Amount ->AppState -> AppState
appAddValue addr amount appState = appState { appLedger = addValue amount addr (appLedger appState)}

getAppValue :: Address -> AppState -> Amount
getAppValue addr appState = getValue addr (appLedger appState)

addAppSendTransaction :: UTCTime -> Address -> Address -> Amount -> AppState -> AppState
addAppSendTransaction currentTime transFromAddr transToAddr transAmount appState =
  appState { appLog = addSendTransaction currentTime transFromAddr transToAddr transAmount (appLog appState) }

addAppCreateTransaction :: UTCTime -> Address -> Amount -> AppState -> AppState
addAppCreateTransaction currentTime transToAddr transAmount appState =
  appState { appLog = addCreateTransaction currentTime transToAddr transAmount (appLog appState) }

getAppTransactions :: Address -> AppState -> [Entry]
getAppTransactions add appState = getTransactions add (appLog appState)

getAllAppTransactions :: Address -> AppState -> [Entry]
getAllAppTransactions addr appState = getAllTransactions addr (appLog appState)
