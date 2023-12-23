generateSequence :: Int -> [Int]
generateSequence list = sequence (-1) 1 list
    where
        sequence current level max
            | level > max = []
            | current > 0 = current : sequence (-1*(level + 1)) (level+1) max
            | current == 0 = current : sequence level level max
            | otherwise = current : sequence 0 level max

main :: IO ()
main = do
    putStrLn "Enter the limit: "
    input <- getLine
    let limit = read input
    putStrLn (show (generateSequence limit))