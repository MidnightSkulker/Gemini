module Assoc (AssociationList, (!), getParameter) where

type AssociationList a b = [(a,b)]

(!) :: (Eq a) => AssociationList a b -> a -> Maybe b
(!) [] x = Nothing
(!) ((a,b):abs) x | a == x = Just b
(!) ((a,b):abs) x = abs ! x

getParameter :: (Eq a, Show a, Show b) => AssociationList a b -> a -> String
getParameter abs a =
  case abs ! a of
    Nothing -> "Parameter " ++ show a ++ " not found"
    Just b -> "Parameter " ++ show a ++ " = " ++ show b
