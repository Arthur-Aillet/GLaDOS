module AST where

import Data.HashMap.Lazy (HashMap, insert, empty, (!?))

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

execCall :: Context -> Ast -> [Ast] -> (Context, Ast)
execCall ctx call args = (ctx, Error "unimplemented")

execBuiltins :: Context -> String -> [Ast] -> Ast
execBuiltins ctx call args = Error "unimplemented"

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
    Truth True -> evalAST ctx _then
    Truth False -> evalAST ctx _else
    x -> (ctx, Error $ "Condition '" ++ show _if ++ "' is invalid")
evalAST ctx x = (ctx, x)

expectAtom :: (Context, Ast) -> Ast
expectAtom (ctx, Atom i) = Atom i
expectAtom (ctx, Truth t) = Truth t
expectAtom (ctx, Symbol sym) = Error ("Symbol '" ++ sym ++ "' is not bound")
expectAtom (ctx, Error string) = Error string
expectAtom (ctx, x) = Error ("expected Atom but got: " ++ show x)

eqQ :: Context -> [Ast] -> Ast
eqQ ctx [a, b] = case expectAtom (evalAST ctx a) of
    Atom ia -> case expectAtom (evalAST ctx b) of
        Atom ib -> Truth (ia == ib)
        x -> x
    x -> x
eqQ _ _ = Error "Bad number of args to eq?"

lt :: Context -> [Ast] -> Ast
lt ctx [a, b] = case expectAtom (evalAST ctx a) of
    Atom ia -> case expectAtom (evalAST ctx b) of
        Atom ib -> Truth (ia < ib)
        x -> x
    x -> x
lt _ _ = Error "Bad number of args to <"
