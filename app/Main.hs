--
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- Main.hs
 --

type Parser a = String -> Either String (a, String)

parseChar :: Char -> Parser Char
parseChar _ "" = Left ("No characters")
parseChar c (x:xs) | (x == c) = Right (c, xs)
                   | otherwise = Left ("No characters")

parseAnyChar :: String -> Parser Char
parseAnyChar _ "" = Left ("No characters")
parseAnyChar "" _ = Left ("No characters")
parseAnyChar (x:xs) (y:ys) | (parseChar x (y:ys)) == Left "No characters" = parseAnyChar xs (y:ys)
                        | otherwise = Right (x, ys)

main :: IO ()
main = do
    putStrLn $ "Exo 1.2.1: " ++ show (parseChar 'a' "abcd")
    putStrLn $ "Exo 1.2.2: " ++ show (parseAnyChar "bca" "abcd")