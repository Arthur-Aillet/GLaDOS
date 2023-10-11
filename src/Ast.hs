{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST
-}

module Ast
  ( Atom (AtomI, AtomF),
    Ast (Symbol, AAtom, Define, Truth, Lambda, Func, Call, Builtin, If, Error, Null),
    evalAST,
    displayAST,
    execCallDistribute,
    execCall,
    execBuiltins,
    Context,
    builtinPredicates,
    Predicates (..),
    emptyContext,
    isBuiltin,
    expectAtom,
    binOp,
    builtinDiv,
    builtinMod,
  )
where

import Data.HashMap.Lazy (HashMap, empty, insert, (!?))

data Atom
  = AtomI Int
  | AtomF Float
  deriving (Show)

-- \| Truth Bool

expectAtom :: (Context, Ast) -> Ast
expectAtom (_, AAtom i) = AAtom i
expectAtom (_, Truth t) = Truth t
expectAtom (_, Symbol sym) = Error ("Symbol '" ++ sym ++ "' is not bound")
expectAtom (_, Error string) = Error string
expectAtom (_, x) = Error ("Expected Atom but got: " ++ show x)

instance Num Atom where
  (+) (AtomI a) (AtomI b) = AtomI (a + b)
  (+) (AtomF a) (AtomI b) = AtomF (a + toEnum b)
  (+) (AtomI a) (AtomF b) = AtomF (toEnum a + b)
  (+) (AtomF a) (AtomF b) = AtomF (a + b)

  (*) (AtomI a) (AtomI b) = AtomI (a * b)
  (*) (AtomF a) (AtomI b) = AtomF (a * toEnum b)
  (*) (AtomI a) (AtomF b) = AtomF (toEnum a * b)
  (*) (AtomF a) (AtomF b) = AtomF (a * b)

  abs (AtomI x) = AtomI $ abs x
  abs (AtomF x) = AtomF $ abs x

  negate (AtomI x) = AtomI $ negate x
  negate (AtomF x) = AtomF $ negate x

  signum (AtomI x) = AtomI $ signum x
  signum (AtomF x) = AtomF $ signum x

  fromInteger x = AtomI $ fromEnum x

instance Eq Atom where
  (==) (AtomI a) (AtomI b) = a == b
  (==) (AtomF a) (AtomI b) = a == toEnum b
  (==) (AtomI a) (AtomF b) = toEnum a == b
  (==) (AtomF a) (AtomF b) = a == b

instance Ord Atom where
  (<=) (AtomI a) (AtomI b) = a <= b
  (<=) (AtomF a) (AtomI b) = a <= toEnum b
  (<=) (AtomI a) (AtomF b) = toEnum a <= b
  (<=) (AtomF a) (AtomF b) = a <= b

atomDiv :: Atom -> Atom -> Atom
atomDiv (AtomI a) (AtomI b) = AtomI (a `div` b)
atomDiv (AtomF a) (AtomI b) = AtomF (a / toEnum b)
atomDiv (AtomI a) (AtomF b) = AtomF (toEnum a / b)
atomDiv (AtomF a) (AtomF b) = AtomF (a / b)

data Ast
  = Error String -- error type with string detail
  | Null -- No-Op or resolved expression leaving no value
  | Symbol String -- Variable that must be bound
  | Define String Ast -- bind an expression to a variable
  | AAtom Atom -- Single known value of type a
  | Truth Bool -- Single known boolean value
  | Lambda [String] Ast -- expression with local bindings
  | Func String [String] Ast -- named expression with local bindings ?? TODO: verify that this shouldn't just be a Define-Lambda pair
  | Call Ast [Ast] -- call to be exectuted or fail immediately
  | Builtin String [Ast] -- builtin (binary?) operator
  | If Ast Ast Ast -- branching condition
  deriving (Show, Eq)

type Context = (HashMap String Ast, Int)

incrDepth :: Context -> Context
incrDepth (ctx, depth) = (ctx, depth + 1)

emptyContext :: Context
emptyContext = (empty, 0)

displayAST :: Ast -> IO ()
displayAST (Error s) = putStrLn ("Error: " ++ s)
displayAST Null = return ()
displayAST (AAtom (AtomI i)) = print i
displayAST (AAtom (AtomF f)) = print f
displayAST (Truth True) = putStrLn "#t"
displayAST (Truth False) = putStrLn "#f"
displayAST (Lambda _ _) = putStrLn "#<procedure>"
displayAST (Func name _ _) = putStrLn $ "#<procedure " ++ name ++ ">"
displayAST (Builtin name _) = putStrLn $ "#<procedure " ++ name ++ ">"
displayAST (Symbol x) =
  if isBuiltin x
    then putStrLn $ "#<procedure " ++ x ++ ">"
    else putStrLn $ "#<symbol " ++ show x ++ ">"
displayAST (x) = putStrLn $ "#inevaluable (" ++ show x ++ ")"

execCallDistribute :: Context -> [String] -> [Ast] -> Maybe Context
execCallDistribute ctx [] [] = Just ctx
execCallDistribute (ctx, depth) (s : ss) (x : xs) =
  case execCallDistribute (ctx, depth) ss xs of
    Just (next, _) -> case evalAST (ctx, depth) x of
      (_, y) -> Just (insert s y next, depth + 1)
    Nothing -> Nothing
execCallDistribute _ _ _ = Nothing

execCallFunc :: String -> Context -> [String] -> [Ast] -> Ast -> Ast
execCallFunc name ctx2 binds args expr =
  case execCallDistribute ctx2 binds args of
    Just jLocalCtx -> snd (evalAST jLocalCtx expr)
    Nothing -> Error ("incorrect args to function '" ++ name ++ "'")

execCallLambda :: Context -> [String] -> [Ast] -> Ast -> Ast
execCallLambda ctx2 binds args expr =
  case execCallDistribute ctx2 binds args of
    Just jLocalCtx -> snd (evalAST jLocalCtx expr)
    Nothing -> Error "Incorrect args to lambda"

execCallSymbol :: Context -> String -> [Ast] -> Ast
execCallSymbol ctx sym args
  | isBuiltin sym = execBuiltins ctx sym args
  | otherwise = Error ("Symbol '" ++ sym ++ "' is not bound")

execCall :: Context -> Ast -> [Ast] -> (Context, Ast)
execCall ctx call args =
  ( ctx,
    case evalAST ctx call of
      (ctx2, Func name binds expr) -> execCallFunc name ctx2 binds args expr
      (ctx2, Lambda binds expr) -> execCallLambda ctx2 binds args expr
      (_, Symbol sym) -> execCallSymbol ctx sym args
      (_, Error x) -> Error x
      _ -> Error "call to non-procedure"
  )

execBuiltins :: Context -> String -> [Ast] -> Ast
execBuiltins ctx "==" xs = builtinPredicates Eq ctx xs
execBuiltins ctx "eq?" xs = builtinPredicates Eq ctx xs
execBuiltins ctx "<" xs = builtinPredicates Lt ctx xs
execBuiltins ctx "<=" xs = builtinPredicates LEt ctx xs
execBuiltins ctx ">" xs = builtinPredicates Gt ctx xs
execBuiltins ctx ">=" xs = builtinPredicates GEt ctx xs
execBuiltins ctx "!=" xs = builtinPredicates NEq ctx xs
execBuiltins ctx "+" xs = binOp (+) ctx xs
execBuiltins ctx "-" xs = binOp (-) ctx xs
execBuiltins ctx "*" xs = binOp (*) ctx xs
execBuiltins ctx "div" xs = builtinDiv ctx xs
execBuiltins ctx "/" xs = builtinDiv ctx xs
execBuiltins ctx "mod" xs = builtinMod ctx xs
execBuiltins _ call _ = Error ("Symbol '" ++ call ++ "' is not bound")

isBuiltin :: String -> Bool
isBuiltin "<" = True
isBuiltin ">" = True
isBuiltin "<=" = True
isBuiltin ">=" = True
isBuiltin "==" = True
isBuiltin "!=" = True
isBuiltin "eq?" = True
isBuiltin "+" = True
isBuiltin "-" = True
isBuiltin "*" = True
isBuiltin "div" = True
isBuiltin "/" = True
isBuiltin "mod" = True
isBuiltin _ = False

evalAST :: Context -> Ast -> (Context, Ast)
evalAST ctx (Error msg) = (incrDepth ctx, Error msg)
evalAST ctx Null = (incrDepth ctx, Error "Expression has no value")
evalAST ctx (Symbol sym) = case fst ctx !? sym of
  Just jast -> (incrDepth ctx, jast)
  Nothing ->
    if isBuiltin sym
      then (incrDepth ctx, Symbol sym)
      else (incrDepth ctx, Error ("Symbol '" ++ sym ++ "' is not bound"))
evalAST (ctx, 0) (Define name x) = case val of
  Error err -> ((ctx, 1), Error err)
  val2 -> ((insert name val2 ctx, 1), Null)
  where
    val = expectAtom (evalAST (ctx, 1) x)
evalAST (ctx, depth) (Define name _) =
  ((ctx, depth + 1), Error $ "Define '" ++ name ++ "' at depth " ++ show depth)
evalAST ctx (AAtom i) = (incrDepth ctx, AAtom i)
evalAST ctx (Truth t) = (incrDepth ctx, Truth t)
-- lambda and func go to the default state of no expansion at this state
evalAST ctx (Call expr args) = execCall ctx expr args
evalAST ctx (Builtin name args) = (incrDepth ctx, execBuiltins ctx name args)
evalAST ctx (If _if _then _else) =
  case expectAtom (evalAST (incrDepth ctx) _if) of
    Error err -> (incrDepth ctx, Error err)
    Truth False -> evalAST (incrDepth ctx) _else
    _ -> evalAST (incrDepth ctx) _then
evalAST ctx x = (incrDepth ctx, x)

binOp :: (Atom -> Atom -> Atom) -> Context -> [Ast] -> Ast
binOp _ _ [] = AAtom 0
binOp _ _ [Error a] = Error a
binOp _ _ [AAtom a] = AAtom a
binOp op ctx (a : b : s) = binOp op ctx (this : s)
  where
    this = case (expectAtom (evalAST ctx a), expectAtom (evalAST ctx b)) of
      (AAtom ia, AAtom ib) -> AAtom (op ia ib)
      (Error x, _) -> Error x
      (_, Error x) -> Error x
      (x, _) -> x
binOp _ _ [_] = Error "Bad number of args to binary operand"

builtinEq :: Context -> [Ast] -> Ast
builtinEq ctx [a, b] =
  case (expectAtom (evalAST ctx a), expectAtom (evalAST ctx b)) of
    (AAtom ia, AAtom ib) -> Truth (ia == ib)
    (Error x, _) -> Error x
    (_, Error x) -> Error x
    (x, _) -> x
builtinEq _ _ = Error "Bad number of args to eq?"

builtinLt :: Context -> [Ast] -> Ast
builtinLt ctx [a, b] =
  case (expectAtom (evalAST ctx a), expectAtom (evalAST ctx b)) of
    (AAtom ia, AAtom ib) -> Truth (ia < ib)
    (Error x, _) -> Error x
    (_, Error x) -> Error x
    (x, _) -> x
builtinLt _ _ = Error "Bad number of args to <"

divByZero :: Atom -> Atom -> Ast
divByZero _ 0 = Error "Division by zero is denied"
divByZero ia ib = AAtom (atomDiv ia ib)

builtinDiv :: Context -> [Ast] -> Ast
builtinDiv _ [] = AAtom 0
builtinDiv _ [AAtom a] = AAtom a
builtinDiv ctx (a : b : s) = case this of
  Error x -> Error x
  _ -> builtinDiv ctx (this : s)
  where
    this = case (expectAtom (evalAST ctx a), expectAtom (evalAST ctx b)) of
      (AAtom ia, AAtom ib) -> divByZero ia ib
      (Error x, _) -> Error x
      (_, Error x) -> Error x
      (x, _) -> x
builtinDiv _ _ = Error "Bad number of args to div"

modByZero :: Int -> Int -> Ast
modByZero _ 0 = Error "Modulo by zero is denied"
modByZero ia ib = AAtom (AtomI (ia `mod` ib))

builtinMod :: Context -> [Ast] -> Ast
builtinMod ctx [a, b] =
  case (expectAtom (evalAST ctx a), expectAtom (evalAST ctx b)) of
    (AAtom (AtomI ia), AAtom (AtomI ib)) -> modByZero ia ib
    (AAtom (AtomF _), _) -> Error "float mod"
    (_, AAtom (AtomF _)) -> Error "float mod"
    (Error x, _) -> Error x
    (_, Error x) -> Error x
    _ -> Error "mod of non-integer"
builtinMod _ _ = Error "Bad number of args to mod"

data Predicates
  = Eq
  | Lt
  | LEt
  | Gt
  | GEt
  | NEq
  deriving (Eq, Show)

swPredicates :: (Ord a) => Predicates -> (a -> a -> Bool)
swPredicates Eq = (==)
swPredicates Lt = (<)
swPredicates LEt = (<=)
swPredicates Gt = (>)
swPredicates GEt = (>=)
swPredicates NEq = (/=)

builtinPredicates :: Predicates -> Context -> [Ast] -> Ast
builtinPredicates predi ctx [a, b] =
  case (expectAtom (evalAST ctx a), expectAtom (evalAST ctx b)) of
    (AAtom ia, AAtom ib) -> Truth (op ia ib)
    (Error x, _) -> Error x
    (_, Error x) -> Error x
    (x, _) -> x
  where
    op = swPredicates predi
builtinPredicates predi _ _ =
  Error ("Bad number of args to predicate " ++ show predi)
