module AST where

import Data.HashMap.Lazy (HashMap, insert, empty, (!?))

-- import GHC.Generics (Generic)
-- import Data.Hashable
-- import Data.HashSet (HashSet, insert, empty, member)
-- data HashNode k v = HashNode k v
--     deriving (Show, Generic)

-- instance (Hashable k) => Hashable (HashNode k v) where
--     hash (HashNode key _) = hash key
--     hashWithSalt x (HashNode key _) = hashWithSalt x key

-- instance (Eq k) => Eq (HashNode k v) where
--     (==) (HashNode key1 _) (HashNode key2 _) = key1 == key2
-- type  HashMap k v = (HashSet (HashNode k v))

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
-- valAST (Call name (x, y)) = case stringToOP name of
--     (OP f) -> case (valAST x, valAST y) of
--         (Just (Number xx), Just (Number yy)) -> Just $ Number $ f xx yy
--         _ -> Error "unspecified"
--     _ -> Error "unspecified"
evalAST ctx x = (ctx, x)


-- doAst :: [Ast] -> Maybe [Int]
-- doAst [] = Just []
-- doAst (x:xs) = case res of
--                 [] -> Nothing
--                 _ -> Just res
--     where res =  foldr    (\y ys ->
--                             case valAST y of
--                                 (Atom jy) -> jy:ys
--                                 _ -> []
--                         )
--                     x xs

-- add :: [Ast] -> Ast
-- add [a, b] = case (valAST a, valAST b) of
--     (Just (Atom ja), Just (Atom jb)) -> Atom (a + b)
--     _ -> Error
-- add _ = Error


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
