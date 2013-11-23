module Main where

import Control.Monad

import Text.Parsec.Prim

import Parser
import Syntax
import Types

typeCheck :: String -> [Paragraph]
typeCheck s = case parse parseProgram "" s of
                Left err -> fail (show err)
                Right ps -> ps

main :: IO ()
main = liftM typeCheck getContents >>= print . show
