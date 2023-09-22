module Main (main) where

-- Symbolic Expressions

-- Exo 1

data SExpr
  = SInt Int
  | SSym String
  | SList [SExpr]
  deriving (Show, Eq)

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
printInsideList (x : xs) = case (printTree x, printInsideList xs) of
  (Just strX, Just strXS) -> Just (strX ++ "," ++ strXS)
  _ -> Nothing

printTree :: SExpr -> Maybe String
printTree (SSym s) = Just ("a Symbol " ++ s)
printTree (SInt i) = Just ("a Number " ++ show i)
printTree (SList l) = fmap ("a List with " ++) (printInsideList l)

-- Abstract Syntax Tree

-- Exo 1

data Ast
  = Define {name :: String, val :: Ast}
  | Call {name :: String, x :: Ast, y :: Ast}
  | AInt {intValue :: Int}
  | ASym {symbolName :: String}
  | ABool {boolValue :: Bool}
  deriving (Show)

-- Exo 2

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SList [SSym "define", SSym var, valExpr]) =
  case sexprToAST valExpr of
    Just exprVal -> Just $ Define var exprVal
    _ -> Nothing
sexprToAST (SList [SSym name, x, y]) =
  case (sexprToAST x, sexprToAST y) of
    (Just x, Just y) -> Just $ Call name x y
    _ -> Nothing
sexprToAST (SInt s) = Just $ AInt s
sexprToAST (SSym s) = Just $ ASym s
sexprToAST _ = Nothing

data Function a = Operator (a -> a -> a) | OtherSymbol String

instance (Show a) => Show (Function a) where
    show (OtherSymbol x) = "OtherSymbol " ++ x
    show (Operator func) = "() () -> ()"

symbolToOperator :: String -> Function Int
symbolToOperator "+" = Operator (+)
symbolToOperator "-" = Operator (-)
symbolToOperator "*" = Operator (*)
symbolToOperator x = OtherSymbol x

evalAST :: Ast -> Maybe Ast
evalAST (Call name x y) = case symbolToOperator name of
    (Operator f) -> case (evalAST x, evalAST y) of
        (Just (AInt xx), Just (AInt yy)) -> Just $ AInt $ f xx yy
        _ -> Nothing
    _ -> Nothing
evalAST x = Just x

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
