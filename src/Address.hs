{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Address where

import Text.Blaze.Html
import Text.Blaze.XHtml1.FrameSet
import Text.Blaze.Html5.Attributes
import Data.Text

type Address = String

-- Format a table row with one address / balance pair
oneRow :: Text -> Text -> Html
oneRow addr amount = do
  tr $ do
    td (a (text (pack("href=\"/pout-jersey/addresse/\"") `append` addr)))
    td (text amount)

-- Format the table header
tableHeader :: Text -> Text -> Html
tableHeader col1hdr col2hdr = do
  thead $ do
    tr $ do
      th (text col1hdr)
      th (text col2hdr)

table2col :: Text -> (a -> b) -> (a -> c) -> [a] -> Html
table2col addr col1 col2 items = do
  table ! class_ "ui collapsing table segment" $ do
    tableHeader "Address" "Balance"
    tbody $ do
      oneRow "Alice" "20"
      tfoot $ do
        tr $ do
          td ! colspan "2" $ "4 Addresses" -- TODO: Change number

--             <tr>
--                 <td><a href="/pout-jersey/addresses/Alice">Alice</a></td>
--                 <td>39.5</td>
--             </tr>

--     <tfoot>
--         <tr>
--             <td colspan="2">4 Addresses</td>
--         </tr>
--     </tfoot>
