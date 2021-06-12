{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
module State (
  AppState(..),
  initAppState,
  getAppValue,
  appAddValue,
  addAppTransaction,
  removeError,
  addError ) where

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
    lastError :: String -- Most recent error
  } deriving (Show, Generic, ToJSON)

initAppState = AppState {
  appLog = emptyLog,
  appLedger = emptyLedger,
  -- Last error that occured
  lastError = "" }

-- remove the last error message
removeError :: AppState -> AppState
removeError appState = appState { lastError = "" }

-- Add an error message
addError :: String -> AppState -> AppState
addError s appState = appState { lastError = s }

-- Add coins to a ledger
appAddValue :: Address -> Amount ->AppState -> AppState
appAddValue addr amount appState = appState { appLedger = addValue amount addr (appLedger appState)}

getAppValue :: Address -> AppState -> Amount
getAppValue addr appState = getValue addr (appLedger appState)

addAppTransaction :: UTCTime -> Address -> Address -> Amount -> AppState -> AppState
addAppTransaction currentTime transFromAddr transToAddr transAmount appState =
  appState { appLog = addTransaction currentTime transFromAddr transToAddr transAmount (appLog appState) }
