{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad

import Control.Interpreter
import Control.Unification.IntVar

import Data.Map

import GeomLab.Parser

import Utils.Showable

main :: IO ()
main = void $ runIntBindingT $ runInterpreter $ interpreter parseProgram "" (empty :: Context ShowString IntVar)