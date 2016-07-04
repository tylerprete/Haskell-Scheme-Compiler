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

module Language.Scheme.R5RS.Parser.Token ( Token (..) )
where
import Language.Scheme.R5RS.Syntax.Expression (Number)
--import Data.Complex -- for Complex Float

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
