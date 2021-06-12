{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
module State (
  AppState(..),
  initAppState,
  getAppValue,
  appAddValue,
  addAppTransaction) where

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
    tickCount :: Int -- For trying out ideas
  } deriving (Show, Generic, ToJSON)

initAppState = AppState {
  appLog = emptyLog,
  appLedger = emptyLedger,
  tickCount = 0 }

-- Add coins to a ledger
appAddValue :: Address -> Amount ->AppState -> AppState
appAddValue addr amount appState = appState { appLedger = addValue amount addr (appLedger appState)}

getAppValue :: Address -> AppState -> Amount
getAppValue addr appState = getValue addr (appLedger appState)

addAppTransaction :: UTCTime -> Address -> Address -> Amount -> AppState -> AppState
addAppTransaction currentTime transFromAddr transToAddr transAmount appState =
  appState { appLog = addTransaction currentTime transFromAddr transToAddr transAmount (appLog appState) }
