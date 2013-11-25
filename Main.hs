module Main where

import Control.Monad

import Text.Parsec.Prim

import Parser
import Syntax
import Types



typeCheck :: String -> [Maybe Type]
typeCheck s = case parse parseProgram "" s of
                Left err -> fail (show err)
                Right ps -> fst $ flip unwrapT ([0..], []) $ forM ps $ \p -> case p of
                                                                               Definition _ -> return Nothing
                                                                               Expression e -> typeOfExpr e emptyEnv

main :: IO ()
main = liftM typeCheck getContents >>= print . show
