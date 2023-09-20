module Main (main) where

-- Symbolic Expressions

-- Exo 1

data SExpr  = SInt Int
            | SSym String
            | SList [SExpr]
            deriving (Show, Eq)


-- SList [SSym "define", SSym "x", SInt 5]
-- SSym "x"
-- SList [SSym "if", SList [SSym ">", SSym "x", SInt 4], SInt 1, SInt 0]
-- SList [SSym "define", SSym "y", SList [SSym "+", SInt 5, SSym "x"]]

-- Exo 2

getSymbol :: SExpr -> Maybe String
getSymbol (SSym s) = Just s
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SInt i) = Just i
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList l) = Just l
getList _ = Nothing

-- Exo 3

printInsideList :: [SExpr] -> Maybe String
printInsideList [] = Nothing
printInsideList [x] = printTree x
printInsideList (x:xs) = case (printTree x, printInsideList xs) of
                        (Just strX, Just strXS) -> Just (strX ++ "," ++ strXS)
                        _ -> Nothing


printTree :: SExpr -> Maybe String
printTree (SSym s) = Just ("a Symbol " ++ s)
printTree (SInt i) = Just ("a Number " ++ show i)
printTree (SList l) = fmap ("a List with "  ++) (printInsideList l)


-- Abstract Syntax Tree

-- Exo 1

data Ast = Define { name :: String, val :: Ast }
         | AInt { intValue :: Int }
         | ASym { symbolName :: String }
         | ABool { boolValue :: Bool }
         deriving Show

-- Exo 2

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInt s) = Just $ AInt s
sexprToAST (SSym s) = Just $ ASym s
sexprToAST (SList [SSym "define", SSym var, valExpr]) =
    case sexprToAST valExpr of
        Just exprVal -> Just $ Define var exprVal
        _            -> Nothing
sexprToAST _ = Nothing

main :: IO ()
main = do
    let expr1 = SSym "define"
        expr2 = SInt 5
        expr3 = SSym "x"
        expr5 = SSym "+"
        expr6 = SSym "y"
        expr7 = SList [expr5, expr2, expr3]
        expr4 = SList [expr1, expr6, expr2]
    putStrLn $ "Symbol 1: " ++ show (getSymbol expr1)
    putStrLn $ "Symbol 2: " ++ show (getInteger expr2)
    putStrLn $ "Symbol 3: " ++ show (getList expr3)
    putStrLn $ "Exo 3:" ++ show (printTree expr4)
    putStrLn $ "Exo 3 AST:" ++ show (sexprToAST expr4)
