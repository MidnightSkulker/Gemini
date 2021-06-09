{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Log where

import Data.Time.Clock
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

import Address
import Coins

data Entry = Entry {
  from :: Address,
  to :: Address,
  amount :: Amount,
  datetime :: UniversalTime
  } deriving (Generic, Show, ToJSON)

-- Only thing compiler cannot derive for ToJSON instance
instance ToJSON UniversalTime where
  toJSON = String . T.pack . show

-- The log is a list of log entries
-- Normally a log would be done using some logging facility
-- such as log4j, I wanted to show some skills coding business
-- logic and HTML formatting here for the coding challenge.
data Log = Log {entries :: [Entry]} deriving (Generic, Show, ToJSON)
