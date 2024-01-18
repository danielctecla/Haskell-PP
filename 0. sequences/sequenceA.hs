-- Function to generate a sequence of numbers multiples of 3
generateSequence :: Int -> [Int]
generateSequence limit = takeMultipleOf3 0 limit
    where
        takeMultipleOf3 current max
            | current > max = []
            | otherwise = current : takeMultipleOf3 (current + 3) max

main :: IO ()
main = do
    putStrLn "Enter the limit: "
    input <- getLine
    let limit = read input
    putStrLn (show (generateSequence limit))
