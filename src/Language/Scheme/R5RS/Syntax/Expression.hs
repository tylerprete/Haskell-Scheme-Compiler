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
    (Exp(..), Formals(..), Var(..), Number(..))
where

-- Number Constructors (Base 10 assumed)
data Number
    = Real Float
--  | Complex Complex Float
    | Integer Int -- Integer Integer (Using Int as a simplification for now)
    deriving (Eq, Show, Ord)

data Var = Var String
    deriving Show

data Formals    = SingleVar Var
        | VarList [Var]
        | DottedVarList [Var] Var
    deriving Show

data Exp    = Ref Var
        -- Literals (Values)
        | Boolean Bool
        | Number Number
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
        | CallCC Exp
    deriving Show
