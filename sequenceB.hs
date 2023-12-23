-- sequence of numbers that starts from 5 and ends at the limit

generateSequence :: Int -> [Int]
generateSequence limit = takeSequence 5 limit
    where
        takeSequence current max
            | current > max = []
            | otherwise = current : takeSequence (current * 3 + 1) max

main :: IO ()
main = do
    putStrLn "Enter the limit:"
    input <- getLine
    let limitInt = read input
    putStrLn(show(generateSequence limitInt))