module PE1 where

isDiv :: Integer -> Bool
isDiv x
    | x `mod` 3 == 0    = True
    | x `mod` 5 == 0    = True
    | otherwise         = False

sumOfMultiples :: Integer
sumOfMultiples = sum [x | x <- [1..999], isDiv x]

sumOfMultiples' :: Integer
sumOfMultiples' = sum [x | x <- [1..999], (x `mod` 3 == 0) || (x `mod` 5 == 0)]
