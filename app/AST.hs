module AST (Ast, evalAST) where

import Data.HashMap.Lazy (HashMap, insert, (!?))

data Ast = Error String             -- error type with string detail
        | Null                      -- No-Op or resolved expression leaving no value
        | Symbol String             -- Variable that must be bound
        | Define String Ast         -- bind an expression to a variable
        | Atom Int                  -- Single known value
        | Truth Bool                -- Single known boolean value
        | Lambda [String] Ast       -- expression with local bindings
        | Func String [String] Ast  -- named expression with local bindings ?? TODO: verify that this shouldn't just be a Define-Lambda pair
        | Call Ast [Ast]            -- call to be exectuted or fail immediately
        | Builtin String [Ast]      -- builtin (binary?) operator
        | If Ast Ast Ast            -- branching condition
            deriving Show


type Context = (HashMap String Ast)

execCallDistribute :: Context -> [String] -> [Ast] -> Maybe Context
execCallDistribute ctx [] [] = Just ctx
execCallDistribute ctx (s:ss) (x:xs) = case execCallDistribute ctx ss xs of
    Just next -> Just $ insert s x next
    Nothing -> Nothing
execCallDistribute _ _ _ = Nothing


execCall :: Context -> Ast -> [Ast] -> (Context, Ast)
execCall ctx call args = (ctx, case evalAST ctx call of
        (ctx2, Lambda bindings expr) -> case execCallDistribute ctx2 bindings args of
            Just jLocalCtx -> snd (evalAST jLocalCtx expr)
            Nothing ->  Error "incorrect number or args to lambda"
        _ ->  Error "call to non-procedure"
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

evalAST :: Context -> Ast -> (Context, Ast)
evalAST ctx (Error msg) = (ctx, Error msg)
evalAST ctx (Null) = (ctx, Error "expression has no value")
evalAST ctx (Symbol sym) = case ctx !? sym of
    Just jast -> evalAST ctx jast
    Nothing -> (ctx, Error ("Symbol '" ++ sym ++ "' is not bound"))
evalAST ctx (Define name x) = (insert name val ctx2, Null)
    where (ctx2, val) = evalAST ctx x
evalAST ctx (Atom i) = (ctx, Atom i)
evalAST ctx (Truth t) = (ctx, Truth t)
-- lambda and func go to the default state of no expansion at this state
evalAST ctx (Call expr args) = execCall ctx expr args
evalAST ctx (Builtin name args) = (ctx, execBuiltins ctx name args)
evalAST ctx (If _if _then _else) = case expectAtom (evalAST ctx _if) of
    Truth False -> evalAST ctx _else
    _ -> evalAST ctx _then
evalAST ctx x = (ctx, x)

expectAtom :: (Context, Ast) -> Ast
expectAtom (_, Atom i) = Atom i
expectAtom (_, Truth t) = Truth t
expectAtom (_, Symbol sym) = Error ("Symbol '" ++ sym ++ "' is not bound")
expectAtom (_, Error string) = Error string
expectAtom (_, x) = Error ("expected Atom but got: " ++ show x)

binOp :: (Int -> Int -> Int) -> Context -> [Ast] -> Ast
binOp op ctx [a, b] = case expectAtom (evalAST ctx a) of
    Atom ia -> case expectAtom (evalAST ctx b) of
        Atom ib -> Atom (op ia ib)
        x -> x
    x -> x
binOp _ _ _ = Error "Bad number of args to eq?"

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
        Atom ib -> if ib == 0
            then Error "division by zero"
            else Atom (ia `div` ib)
        x -> x
    x -> x
builtinDiv _ _ = Error "Bad number of args to <"

builtinMod :: Context -> [Ast] -> Ast
builtinMod ctx [a, b] = case expectAtom (evalAST ctx a) of
    Atom ia -> case expectAtom (evalAST ctx b) of
        Atom ib -> if ib == 0
            then Error "modulo by zero"
            else Atom (ia `mod` ib)
        x -> x
    x -> x
builtinMod _ _ = Error "Bad number of args to <"
