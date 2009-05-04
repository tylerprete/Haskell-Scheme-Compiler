-----------------------------------------------------------------------------
-- |
-- Module      : Language.Scheme.R5RS.Parser.ParserUtils
-- Copyright   : (c) Tyler Prete 
-- License     : BSD-style
-- Maintainer  : psyonic@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Helper Utilities for implementing the R5RS Parser
-- See: <http://people.csail.mit.edu/jaffer/r5rs_9.html#SEC72>
-----------------------------------------------------------------------------

module Language.Scheme.R5RS.Parser.ParserUtils
where
import Language.Scheme.R5RS.Syntax.Expression
import Data.Maybe

makeFormalList xs (Just d)	= DottedVarList xs d
makeFormalList xs Nothing	= VarList xs

makeList xs (Just d)	= DottedList xs d
makeList xs Nothing	= List xs

makeVector (List xs) = Vector xs
makeVector _ = error "Bad input to makeVector -- Not a List()"
