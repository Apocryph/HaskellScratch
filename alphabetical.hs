isAlphabetical :: String -> Bool
isAlphabetical [] = True
isAlphabetical (x:[]) = True
isAlphabetical (x:y:[]) = x <= y
isAlphabetical (x:y:xs) = (x <= y) && isAlphabetical (y:xs)
