module Scratch where

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long.  The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter' :: String -> String
firstLetter' "" = "Empty string, whoops!"
firstLetter' wholeWord@(x:_) = "The first letter of " ++ wholeWord ++ " is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
 | bmi <= skinny = "You're underweight, eat more!"
 | bmi <= normal = "Looking Good!"
 | bmi <= overweight = "You're overweight.  Let's work out together!"
 | otherwise   = "You're obese.  Go see a doctor."
 where bmi = weight / height ^ 2
       (skinny, normal, overweight) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
 | a <= b    = b
 | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
 | a == b    = EQ
 | a <= b    = LT
 | otherwise = GT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
 where (f:_) = firstname
       (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
 where bmi weight height = weight / height ^ 2
