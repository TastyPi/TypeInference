-- | Main entry point to the application.
module Main where

import GeomLab.Parser

import Control.Interpreter

-- | The main entry point.
main :: IO ()
main = interpreter parseProgram "" (empty :: Context ShowString IntVar)
