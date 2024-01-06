-- Sum
sumL :: Int -> Int -> Int
sumL = \x -> \y -> x + y

main :: IO ()
main = do
  print $ sumL 10 2
  print $ multiplicationL 10 2
  print $ exponentiation 10 2

-- Multiplication
multiplicationL :: Int -> Int -> Int
multiplicationL = \x -> \y -> x * y

-- Exponenciación

exponentiation :: Int -> Int -> Int
exponentiation = \x -> \y -> x ^ y