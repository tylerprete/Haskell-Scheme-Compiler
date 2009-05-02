{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Scheme.R5RS.Parser.Token
-- Copyright   : (c) Tyler Prete 
-- License     : BSD-style
-- Maintainer  : psyonic@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Lexical tokens for the Scheme R5RS lexer.
-- See: <http://people.csail.mit.edu/jaffer/r5rs_9.html#SEC72>
-----------------------------------------------------------------------------

module Language.Scheme.R5RS.Parser.Token ( Number (..), Token (..) )
where
--import Data.Complex -- for Complex Float

-- Number Constructors (Base 10 assumed)
data Number
	= Real Float
--	| Complex Complex Float
	| Integer Int -- Integer Integer (Using Int as a simplification for now)
	deriving (Eq, Show, Ord)

-- | Lexical tokens.
data Token
	-- Identifier
	= Identifier !String
	| Boolean Bool
	| Number Number
	| Character Char
	| String !String
	| LeftParen
	| RightParen
	| PoundLeftParen
	| Tick
	| BackTick
	| Comma
	| CommaAt
	| Dot
	deriving (Eq, Show, Ord)
