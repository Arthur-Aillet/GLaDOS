--
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- Main.hs
 --

type Parser a = String -> Either String (a, String)

parseChar :: Char -> Parser Char
parseChar _ "" = Left ("No character")
parseChar c (x:xs) | (x == c) = Right (c, xs)
                   | otherwise = parseChar c xs

main :: IO ()
main = do
    putStrLn $ "Exo 1.2.1: " ++ show (parseChar 'a' "abcd")