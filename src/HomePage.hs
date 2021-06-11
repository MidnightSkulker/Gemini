{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module HomePage where

import Prelude hiding (div, head, id)
import Address
import Coins
import Log
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
homePage :: String -> Ledger -> Log -> Html
homePage titleStr ledger log = do
  docType
  html $ do
    head $ do
      title "Jobcoin"
      link ! rel "stylesheet" ! media "screen" ! href "//cdnjs.cloudflare.com/ajax/libs/semantic-ui/0.16.1/css/semantic.min.css"
      link ! rel "stylesheet" ! media "screen" ! href "/assets/stylesheets/main.css"
    body $ do
      div ! class_ "ui page grid" $ do
        div ! class_ "column" $ do
          div ! class_ "row" $ do
            h1 ! class_ "ui header" $ text ("Jobcoin Viewer for Peter D. White")
            a ! href "/pout-jersey/api" $ text("API docs")
            p ""
            div ! class_ "ui two column grid" $ do
              div ! class_ "row" $ do
                div ! class_ "column" $ do
                  form ! class_ "ui form segment" ! action "/pout-jersey/send" ! method "POST" $ do
                    h3 ! class_ "ui header" $ text("Send Jobcoins")
                    div ! class_ "field  " ! id "fromAddress_field" $ do
                      input ! type_ "text" ! id "fromAddress" ! name "fromAddress" ! value "" ! placeholder "From Address"
                    div ! class_ "field  " ! id "toAddress_field" $ do
                      input ! type_ "text" ! id "toAddress" ! name "toAddress" ! value "" ! placeholder "To Address"
                    div ! class_ "field  " ! id "amount_field" $ do
                      input ! type_ "text" ! id "amount" ! name "amount" ! value "" ! placeholder "Amount"
                    button ! class_ "ui secondary button" ! type_ "submit" $ text "Send Coins"
                div ! class_ "column" $ do
                  form ! action "/pout-jersey/create" ! method "POST" ! method "POST" ! class_ "ui form segment" $ do
                    h3 ! class_ "ui header" $ "Create 50 New Jobcoins"
                    div ! class_ "field  " ! id "address_field" $ do
                      input ! type_ "text" ! id "address" ! name "address" ! value "" ! placeholder"Address"
                    button ! type_ "submit" ! class_ "ui secondary button" $ text "Create 50 Coins"
            ledger2html ledger
            transactionsHtml log
