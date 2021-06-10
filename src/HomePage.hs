{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module HomePage where

import Prelude hiding (div, head)
import Address
import Data.Aeson
import GHC.Generics
-- Imports for formatting HTML
import Text.Blaze.Html
import Text.Blaze.XHtml1.FrameSet
import Text.Blaze.Html5.Attributes hiding (title, form)
import Data.Text hiding (length, head)
import qualified Data.Map as Map

-- Building the home page with combinators, to make it easier
-- to insert the ledger and the transaction log.
homePage :: String -> Html
homePage titleStr = do
  docType
  html (toHtml titleStr)
  head $ do
    title "Jobcoin"
    link ! rel "stylesheet" ! media "screen" ! href "//cdnjs.cloudflare.com/ajax/libs/semantic-ui/0.16.1/css/semantic.min.css"
    link ! rel "stylesheet" ! media "screen" ! href "/assets/stylesheets/main.css"
    h1 ! class_ "ui header" $ text ("Jobcoin Viewer for Peter D. White")
    a ! href "/pout-jersey/api" $ text("API docs")
    p ""
    body $ do
      div ! class_ "ui page grid" $ do
        div ! class_ "column" $ do
          div ! class_ "row" $ do
            form ! class_ "ui form segment" ! action "/pout-jersey/send" ! method "POST" $ do
              h3 ! class_ "ui header" $ text("Send Jobcoins")

              text "Under construction"
