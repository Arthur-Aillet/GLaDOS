module AST where

import SExpr (SExpr (Integer, Symbol, SList))


-- data Operation a where
--     OSymbol :: Show a => String -> Operation a
--     OP :: Show a => (a -> a -> a) -> Operation a

data Operation a = OSymbol String
                | OP (a -> a -> a)
            -- deriving Show

instance (Show a) => Show (Operation a) where
    show (OSymbol x) = "OSymbol " ++ x
    show (OP func) = "() () -> ()"

data Ast = ASymbol String
        | Define String Ast
        | Number Int
        | Call (String) (Ast, Ast)
            deriving Show

stringToOP :: String -> Operation Int
stringToOP "+" = OP (+)
stringToOP "*" = OP (*)
stringToOP "-" = OP (-)
-- stringToOP "/" = OP (/)
stringToOP x = OSymbol x


sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SList [x]) = sexprToAST x
sexprToAST (SList [Symbol "define",Symbol name,s]) = case mexpr of
        Just expr -> Just $ Define name expr
        Nothing -> Nothing
    where mexpr = sexprToAST s
sexprToAST (SList [Symbol name, x,y]) = case (mexpr1, mexpr2) of
    (Just expr1, Just expr2) -> Just $ Call (name) (expr1, expr2)
    _ -> Nothing
    where
        mexpr1 = sexprToAST x
        mexpr2 = sexprToAST y
sexprToAST (Integer x) = Just $ Number x
sexprToAST (Symbol x) = Just $ ASymbol x
sexprToAST (SList _) = Nothing

evalAST :: Ast -> Maybe Ast
-- evalAST (Define name x) = evalAST x
evalAST (Call name (x, y)) = case stringToOP name of
    (OP f) -> case (evalAST x, evalAST y) of
        (Just (Number xx), Just (Number yy)) -> Just $ Number $ f xx yy
        _ -> Nothing
    _ -> Nothing
evalAST x = Just x
