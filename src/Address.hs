{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Address where

import Text.Blaze.Html
import Text.Blaze.XHtml1.FrameSet
import Text.Blaze.Html5.Attributes
import Data.Text

type Address = String


--             <tr>
--                 <td><a href="/pout-jersey/addresses/Alice">Alice</a></td>
--                 <td>39.5</td>
--             </tr>

--     <tfoot>
--         <tr>
--             <td colspan="2">4 Addresses</td>
--         </tr>
--     </tfoot>
