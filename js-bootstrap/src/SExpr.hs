module SExpr where

import Data.Maybe (fromMaybe)
import Data.Foldable (foldMap')

data SExpr = Integer Int
            | Symbol String
            | SList [SExpr]
            deriving Show

x = Integer 5
i x = Integer x
l x = SList x
s x = Symbol x

-- ex02:

getSymbol :: SExpr -> Maybe String
getSymbol (Symbol x) = Just x
getSymbol _ = Nothing


getInteger :: SExpr -> Maybe Int
getInteger (Integer x) = Just x
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList x) = Just x
getList _ = Nothing

-- ex03:

long :: [a] -> Int -> Bool
long [] n = n >= 0
long (x:xs) n = if n > 0 then long xs (n - 1) else True

printTree :: SExpr -> String
printTree (SList []) = "a SList"
printTree (SList [x]) = printTree (SList []) ++ " with " ++ printTree' x
printTree (SList [x,xs]) = printTree (SList [x]) ++ " followed by " ++ printTree' xs
printTree (SList (x:xs:xss)) = printTree (SList [x,xs]) ++ foldMap (\y -> ',':' ':printTree' y) xss
printTree x = printTree' x

printTree' :: SExpr -> String
printTree' (SList x) = "(" ++ printTree (SList x) ++ ")"
printTree' (Symbol x) = "a Symbol '" ++ x ++ "'"
printTree' (Integer x) = "a Number " ++ show x
-- printTree' _ = ""
