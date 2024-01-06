nQueens :: Int -> [[(Int, Int)]]
nQueens n = placeQueens n n

placeQueens :: Int -> Int -> [[(Int, Int)]]
placeQueens _ 0 = [[]]
placeQueens size n = [pos:positions | positions <- placeQueens size (n-1), pos <- positionsFor size n, isValid pos positions]

positionsFor :: Int -> Int -> [(Int, Int)]
positionsFor n row = [(row, m) | m <- [1..n]]

isValid :: (Int, Int) -> [(Int, Int)] -> Bool
isValid (x, y) positions = not $ any (\(px, py) -> px == x || py == y || abs (px - x) == abs (py - y)) positions

printBoard :: Int -> [(Int, Int)] -> IO ()
printBoard n positions = sequence_ [putStrLn [if (i, j) `elem` positions then 'Q' else '.' | j <- [1..n]] | i <- [1..n]]

main :: IO ()
main = do
    n <- readLn :: IO Int
    let solutions = nQueens n
    putStrLn $ "Found " ++ show (length solutions) ++ " solutions"
    mapM_ (printBoard n) solutions