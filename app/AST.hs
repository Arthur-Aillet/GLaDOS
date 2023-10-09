{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST
-}

module AST
  ( Ast (Symbol, Define, Atom, Truth, Lambda, Func, Call, Builtin, If),
    evalAST,
    displayAST,
    Context,
    emptyContext,
  )
where

import Data.HashMap.Lazy (HashMap, empty, insert, (!?))

data Ast
  = Error String -- error type with string detail
  | Null -- No-Op or resolved expression leaving no value
  | Symbol String -- Variable that must be bound
  | Define String Ast -- bind an expression to a variable
  | Atom Int -- Single known value
  | Truth Bool -- Single known boolean value
  | Lambda [String] Ast -- expression with local bindings
  | Func String [String] Ast -- named expression with local bindings ?? TODO: verify that this shouldn't just be a Define-Lambda pair
  | Call Ast [Ast] -- call to be exectuted or fail immediately
  | Builtin String [Ast] -- builtin (binary?) operator
  | If Ast Ast Ast -- branching condition
  deriving (Show)

type Context = (HashMap String Ast, Int)

incrDepth :: Context -> Context
incrDepth (ctx, depth) = (ctx, depth + 1)

emptyContext :: Context
emptyContext = (empty, 0)

displayAST :: Ast -> IO ()
displayAST (Error s) = putStrLn ("evaluation error: " ++ s)
displayAST (Null) = return ()
displayAST (Atom i) = print i
displayAST (Truth True) = putStrLn "#t"
displayAST (Truth False) = putStrLn "#f"
displayAST (Lambda _ _) = putStrLn "#<procedure>"
displayAST (Func name _ _) = putStrLn $ "#<procedure " ++ name ++ ">"
displayAST (Builtin name _) = putStrLn $ "#<procedure " ++ name ++ ">"
displayAST (_) = putStrLn "#inevaluable"

execCallDistribute :: Context -> [String] -> [Ast] -> Maybe Context
execCallDistribute ctx [] [] = Just ctx
execCallDistribute (ctx, depth) (s : ss) (x : xs) = case execCallDistribute (ctx, depth) ss xs of
  Just (next, depth2) -> case evalAST (ctx, depth) x of
    (_, y) -> Just (insert s y next, depth + 1)
  Nothing -> Nothing
execCallDistribute _ _ _ = Nothing

execCall :: Context -> Ast -> [Ast] -> (Context, Ast)
execCall ctx call args =
  ( ctx,
    case evalAST ctx call of
      (ctx2, Lambda binds expr) -> case execCallDistribute ctx2 binds args of
        Just jLocalCtx -> snd (evalAST jLocalCtx expr)
        Nothing -> Error "incorrect args to lambda"
      (_, Symbol sym) ->
        if isBuiltin sym
          then execBuiltins ctx sym args
          else Error ("call to non-procedure symbol: " ++ sym)
      (_, Error x) -> Error x
      _ -> Error "call to non-procedure"
  )

execBuiltins :: Context -> String -> [Ast] -> Ast
execBuiltins ctx "<" xs = builtinLt ctx xs
execBuiltins ctx "eq?" xs = builtinEq ctx xs
execBuiltins ctx "+" xs = binOp (+) ctx xs
execBuiltins ctx "-" xs = binOp (-) ctx xs
execBuiltins ctx "*" xs = binOp (*) ctx xs
execBuiltins ctx "div" xs = builtinDiv ctx xs
execBuiltins ctx "mod" xs = builtinMod ctx xs
execBuiltins _ call _ = Error ("unimplemented builtin: " ++ call)

isBuiltin :: String -> Bool
isBuiltin "<" = True
isBuiltin "eq?" = True
isBuiltin "+" = True
isBuiltin "-" = True
isBuiltin "*" = True
isBuiltin "div" = True
isBuiltin "mod" = True
isBuiltin _ = False

evalAST :: Context -> Ast -> (Context, Ast)
evalAST ctx (Error msg) = (incrDepth ctx, Error msg)
evalAST ctx Null = (incrDepth ctx, Error "expression has no value")
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
evalAST ctx (Atom i) = (incrDepth ctx, Atom i)
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

expectAtom :: (Context, Ast) -> Ast
expectAtom (_, Atom i) = Atom i
expectAtom (_, Truth t) = Truth t
expectAtom (_, Symbol sym) = Error ("Symbol '" ++ sym ++ "' is not bound")
expectAtom (_, Error string) = Error string
expectAtom (_, x) = Error ("expected Atom but got: " ++ show x)

binOp :: (Int -> Int -> Int) -> Context -> [Ast] -> Ast
binOp op ctx [a, b] =
  case expectAtom (evalAST ctx a) of
    Atom ia -> case expectAtom (evalAST ctx b) of
      Atom ib -> Atom (op ia ib)
      x -> x
    x -> x
binOp _ _ _ = Error "Bad number of args to binary operand"

builtinEq :: Context -> [Ast] -> Ast
builtinEq ctx [a, b] = case expectAtom (evalAST ctx a) of
  Atom ia -> case expectAtom (evalAST ctx b) of
    Atom ib -> Truth (ia == ib)
    x -> x
  x -> x
builtinEq _ _ = Error "Bad number of args to eq?"

builtinLt :: Context -> [Ast] -> Ast
builtinLt ctx [a, b] = case expectAtom (evalAST ctx a) of
  Atom ia -> case expectAtom (evalAST ctx b) of
    Atom ib -> Truth (ia < ib)
    x -> x
  x -> x
builtinLt _ _ = Error "Bad number of args to <"

builtinDiv :: Context -> [Ast] -> Ast
builtinDiv ctx [a, b] = case expectAtom (evalAST ctx a) of
  Atom ia -> case expectAtom (evalAST ctx b) of
    Atom ib ->
      if ib == 0
        then Error "division by zero"
        else Atom (ia `div` ib)
    x -> x
  x -> x
builtinDiv _ _ = Error "Bad number of args to div"

builtinMod :: Context -> [Ast] -> Ast
builtinMod ctx [a, b] = case expectAtom (evalAST ctx a) of
  Atom ia -> case expectAtom (evalAST ctx b) of
    Atom ib ->
      if ib == 0
        then Error "modulo by zero"
        else Atom (ia `mod` ib)
    x -> x
  x -> x
builtinMod _ _ = Error "Bad number of args to mod"
