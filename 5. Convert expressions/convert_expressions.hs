isOperand :: Char -> Bool
isOperand c = c `elem` ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

infixToPostfix :: String -> String
infixToPostfix exp = process [] [] (words exp)
  where
    process out stack [] = unwords (reverse out ++ stack)
    process out stack (x:xs)
      | isOperand (head x) = process (x:out) stack xs
      | otherwise = process out (x:stack) xs

postfixToPrefix :: String -> String
postfixToPrefix exp = head (foldl processToken [] (words exp))
  where
    processToken stack token
      | isOperand (head token) = token : stack
      | otherwise =
          let (right:rightStack) = stack
              (left:leftStack) = rightStack
          in (token ++ " " ++ left ++ " " ++ right) : leftStack

main :: IO ()
main = do
    putStrLn "Enter an infix expression:"
    infixExpr <- getLine
    let postfixExpr = infixToPostfix infixExpr
    let prefixExpr = postfixToPrefix postfixExpr
    putStrLn $ "Postfix Expression: " ++ postfixExpr
    putStrLn $ "Prefix Expression: "  ++ prefixExpr
