-- mergeSort

mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort comparator arr = merge comparator (mergeSort comparator firstHalf) (mergeSort comparator secondHalf)
    where
        (firstHalf, secondHalf) = splitAt (length arr `div` 2) arr

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ arr [] = arr
merge _ [] arr = arr
merge comparator (x:xr) (y:yr)
    | comparator x y = x : merge comparator xr (y:yr)
    | otherwise = y : merge comparator (x:xr) yr

main :: IO ()
main = do
    let unsorted = [1, 5, 2, 4, 3, 6, 7, 9, 8]
    putStrLn $ "Unsorted list: " ++ show unsorted
    let sorted = mergeSort (<) unsorted
    putStrLn $ "Sorted list: " ++ show sorted
