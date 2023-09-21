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
        | Builtin String Ast Ast    -- builtin binary operator
        | If Ast Ast Ast            -- branching condition
            deriving Show


type Context = (HashMap String Ast)

evalAST :: Context -> Ast -> (Context, Ast)
evalAST hmap (Define name x) = (insert name val hmap2, Null)
    where (hmap2, val) = evalAST hmap x
evalAST ctx x = (ctx, x)

expectAtom :: (Context, Ast) -> Ast
expectAtom (ctx, Atom i) = Atom i
expectAtom (ctx, Truth t) = Truth t
expectAtom (ctx, Symbol string) = Error ("Symbol " ++ string ++ " is not bound")
expectAtom (ctx, Error string) = Error string
expectAtom (ctx, x) = Error ("expected Atom but got:" ++ show x)

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
lt _ _ = Error "Bad number of args to eq?"
