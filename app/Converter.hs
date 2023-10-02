{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Converter
-}

module Converter (sexprToAST) where

import AST (Ast (Atom, Call, Define, If, Lambda, Symbol))
import SParser (SExpr (SInt, SList, SSym))

convertArgsContinuous :: [SExpr] -> Maybe [Ast]
convertArgsContinuous (x : xs) = case convertArgsContinuous xs of
  (Just ys) -> case sexprToAST x of
    Just y -> Just (y : ys)
    Nothing -> Nothing
  Nothing -> Nothing
convertArgsContinuous [] = Just []

convertSymbols :: [SExpr] -> Maybe [String]
convertSymbols (SSym sym : xs) = case convertSymbols xs of
  (Just ys) -> Just (sym : ys)
  Nothing -> Nothing
convertSymbols [] = Just []
convertSymbols _ = Nothing

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SList [x]) = sexprToAST x
sexprToAST (SList [SSym "define", SSym name, s]) = case mexpr of
  Just expr -> Just $ Define name expr
  Nothing -> Nothing
  where
    mexpr = sexprToAST s
sexprToAST (SList [SSym "define", SList (SSym name : args), expr]) =
  case convertSymbols args of
    (Just jArgs) -> case sexprToAST expr of
      (Just jExpr) -> Just (Define name (Func name jArgs jExpr))
      Nothing -> Nothing
    _ -> Nothing
sexprToAST (SList [SSym "if", _if, _then, _else]) =
    case (sexprToAST _if, sexprToAST _then, sexprToAST _else) of
        (Just jIf, Just jThen, Just jElse) -> Just (If jIf jThen jElse)
        _ -> Nothing

sexprToAST (SList [SSym "lambda", SList args, expr]) =
    case convertSymbols args of
      (Just jArgs) -> case sexprToAST expr of
        (Just jExpr) -> Just (Lambda jArgs jExpr)
        Nothing -> Nothing
      _ -> Nothing

sexprToAST (SList (SSym name:args)) = case convertArgsContinuous args of
    (Just jArgs) -> Just (Call (Symbol name) jArgs)
    _ -> Nothing
sexprToAST (SList (SSym name : args)) = case convertArgsContinuous args of
  (Just jArgs) -> Just (Call (Symbol name) jArgs)
  _ -> Nothing
sexprToAST (SInt x) = Just $ Atom x
sexprToAST (SSym x) = Just $ Symbol x
sexprToAST (SList _) = Nothing
sexprToAST _ = Nothing
