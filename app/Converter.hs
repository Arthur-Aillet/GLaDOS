--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Converter
--

module Converter (sexprToAST) where

import AST (Ast (Symbol, Atom, Call, Define))
import SParser (SExpr (SInt, SSym, SList))

convertArgsContinuous :: [SExpr] -> Maybe [Ast]
convertArgsContinuous (x:xs) = case convertArgsContinuous xs of
    (Just ys) -> case sexprToAST x of
        Just y -> Just (y:ys)
        Nothing -> Nothing
    Nothing -> Nothing
convertArgsContinuous [] = Just []
convertArgsContinuous _ = Nothing


sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SList [x]) = sexprToAST x
sexprToAST (SList [SSym "define",SSym name,s]) = case mexpr of
        Just expr -> Just $ Define name expr
        Nothing -> Nothing
    where mexpr = sexprToAST s
sexprToAST (SList (SSym name:args)) = case convertArgsContinuous args of
    (Just jArgs) -> Just (Call (Symbol name) jArgs)
    _ -> Nothing
sexprToAST (SInt x) = Just $ Atom x
sexprToAST (SSym x) = Just $ Symbol x
sexprToAST (SList _) = Nothing
