-- Quicksort Haskell

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort list = quicksort left_sorted ++ pivot_list ++ quicksort right_sorted
    where
        pivot :: Int
        pivot = head list
        left_sorted :: [Int]
        left_sorted = filter (< pivot) list
        pivot_list :: [Int]
        pivot_list = filter (== pivot) list
        right_sorted :: [Int]
        right_sorted = filter (> pivot) list

main :: IO ()
main = do
    putStrLn "Quicksort Haskell"
    putStrLn "Before sorting:"
    let list = [10, 2, 5, 3, 1, 6, 7, 4, 2, 3, 4, 8, 9]
    print $ list
    putStrLn "After sorting:"
    print $ quicksort list
