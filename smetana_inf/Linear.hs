{-# LANGUAGE NamedFieldPuns #-}

divisibleBy :: Integer -> Integer -> Bool
x `divisibleBy` y = mod x y == 0

(/?) :: Integer -> Integer -> Maybe Integer
_ /? 0 = Nothing
x /? y | x `divisibleBy` y = Just $ x `div` y
       | otherwise         = Nothing

data Linear = Linear {
    slope :: Integer,
    intercept :: Integer
}

constant :: Integer -> Linear
constant x = Linear { slope=0, intercept=x }

evaluate :: Linear -> Integer -> Integer
evaluate (Linear { slope, intercept }) x = x * slope + intercept

solve :: Linear -> Integer -> Maybe Integer
solve (Linear { slope, intercept }) y = (y - intercept) /? slope

data Command = GoToN Linear Linear
             | GoTo Integer Integer
             | SwapN Linear Linear Linear
             | Swap Integer Integer Integer
             | OutputN Linear Linear
             | Output Integer Integer
             | HaltN Linear

type SwapList = [(Integer, Integer)]