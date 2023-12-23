-- Function return the factorial of a number
factorial :: Integer -> Integer
factorial n = product [1..n]

main :: IO ()
main = do
    putStrLn "Enter a number: "
    n <- getLine
    let num = read n :: Integer
    putStrLn ("Factorial of " ++ n ++ " is " ++ show (factorial num))