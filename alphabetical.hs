module Alphabet where

isAlphabetical :: String -> Bool
isAlphabetical [] = True
isAlphabetical (_:[]) = True
isAlphabetical (x:y:[]) = x <= y
isAlphabetical (x:y:xs) = (x <= y) && isAlphabetical (y:xs)
