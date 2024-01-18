push :: Char -> [Char] -> [Char]
push x stack = x : stack

pop :: [Char] -> (Char, [Char])
pop (x:xs) = (x, xs)

reverseUsingStack :: String -> String
reverseUsingStack str = reverseHelper str []
  where
    reverseHelper [] stack = stack
    reverseHelper (x:xs) stack = reverseHelper xs (push x stack)

isPalindrome :: String -> Bool
isPalindrome word = word == reverseUsingStack word

main :: IO ()
main = do
    putStrLn "Enter a word:"
    word <- getLine
    putStrLn $ if isPalindrome word
               then "The word is a palindrome."
               else "The word is not a palindrome."
