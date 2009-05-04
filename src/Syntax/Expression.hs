-----------------------------------------------------------------------------
-- |
-- Module      : Language.Scheme.R5RS.Syntax.Expression
-- Copyright   : (c) Tyler Prete 
-- License     : BSD-style
-- Maintainer  : psyonic@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Expression defined for use by Parser and compiler
-- See: <http://people.csail.mit.edu/jaffer/r5rs_9.html#SEC72>
-----------------------------------------------------------------------------

module Language.Scheme.R5RS.Syntax.Expression
	(Exp(..), Formals(..), Var(..),
	makeFormalList, makeList, makeVector)	
where
import qualified Language.Scheme.R5RS.Parser.Token as Token
import Data.Maybe

data Var = Var String
	deriving Show

data Formals	= SingleVar Var
		| VarList [Var]
		| DottedVarList [Var] Var
	deriving Show

data Exp	= Ref Var
		-- Literals (Values)
		| Boolean Bool
		| Number Token.Number
		| Character Char
		| String String
		| Symbol String
		| List [Exp]
		| DottedList [Exp] Exp
		| Vector [Exp]

		| App Exp [Exp]
		| Lambda Formals Exp
		| If Exp Exp Exp
		| SetBang Var Exp
	deriving Show

makeFormalList xs (Just d)	= DottedVarList xs d
makeFormalList xs Nothing	= VarList xs

makeList xs (Just d)	= DottedList xs d
makeList xs Nothing	= List xs

makeVector (List xs) = Vector xs
makeVector _ = error "Bad input to makeVector -- Not a List()"
