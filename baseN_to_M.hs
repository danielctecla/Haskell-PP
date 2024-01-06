main :: IO ()
main = do
    putStrLn "Insert your number: "
    number <- getLine
    putStrLn "Insert number's base: "
    base_m <- getLine
    putStrLn "Insert the base you want to convert to: "
    base_n <- getLine
    let result = convert_to_decimal number (read base_m :: Int)
    let new_result = convert_from_decimal result (read base_n :: Int)
    putStrLn $ "The result in decimal is: " ++ show new_result


convert_to_decimal :: String -> Int -> Int
convert_to_decimal number base = convert_to_decimal' (reverse number) base 0 0
  where
    convert_to_decimal' [] _ _ result = result
    convert_to_decimal' (d:ds) base i result
      | d >= '0' && d <= '9' = convert_to_decimal' ds base (i+1) (result + (fromEnum d - fromEnum '0') * base ^ i)
      | otherwise = convert_to_decimal' ds base (i+1) (result + (fromEnum d - fromEnum 'A' + 10) * base ^ i)

convert_from_decimal :: Int -> Int -> String
convert_from_decimal number base = reverse $ convert_from_decimal' number base
    where
        convert_from_decimal' 0 _ = []
        convert_from_decimal' number base
            | remainder < 10 = (toEnum (remainder + fromEnum '0')) : convert_from_decimal' quotient base
            | otherwise = (toEnum (remainder - 10 + fromEnum 'A')) : convert_from_decimal' quotient base
            where
                (quotient, remainder) = divMod number base