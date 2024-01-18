solveHanoi :: Integer -> String -> String -> String -> [(String, String)]
solveHanoi 0 _ _ _ = [] -- No moves needed for zero disks.
solveHanoi disks start temp end =
    solveHanoi (disks - 1) start end temp ++
    [(start, end)] ++
    solveHanoi (disks - 1) temp start end

main :: IO ()
main = do
    putStrLn "Enter disk count for Hanoi Towers:"
    diskCountInput <- getLine
    let diskCount = read diskCountInput :: Integer
    mapM_ (print . showMove) (solveHanoi diskCount "A" "B" "C")

showMove :: (String, String) -> String
showMove (a, b) = "Move from " ++ a ++ " to " ++ b
