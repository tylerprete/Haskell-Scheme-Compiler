-----------------------------------------------------------------------------
-- |
-- Module      : Language.Scheme.R5RS.Syntax.PrettyPrint
-- Copyright   : (c) Tyler Prete 
-- License     : BSD-style
-- Maintainer  : psyonic@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Pretty Print for AST Expressions
-- See: <http://people.csail.mit.edu/jaffer/r5rs_9.html#SEC72>
-----------------------------------------------------------------------------

module Language.Scheme.R5RS.Syntax.PrettyPrint (pp)
where
import Language.Scheme.R5RS.Syntax.Expression

class PrettyPrint a where
  pp	:: a -> String

instance PrettyPrint Number where
  pp (Real f) = show f
  pp (Integer i) = show i

instance PrettyPrint Var where
  pp (Var s) = s

instance PrettyPrint Formals where
  pp (SingleVar v) = pp v
  pp (VarList xs) = ppList xs
  pp (DottedVarList xs v) = ppDottedList xs v

instance PrettyPrint Exp where
  pp (Ref v) = pp v
  pp (Boolean b) = if b then "#t" else "#f"
  pp (Number n) = pp n
  pp (Character c) = show c
  pp (String s) = "\"" ++ s ++ "\""
  pp (Symbol s) = s
  pp (List xs) = ppList xs
  pp (DottedList xs e) = ppDottedList xs e
  pp (Vector xs) = "#(" ++ (unwords $ map pp xs) ++ ")"
  pp (App op args) = "(" ++ pp op ++ " " ++ (unwords $ map pp args) ++ ")"
  pp (Lambda f e) = "(lambda " ++ pp f ++ " " ++ pp e ++ ")"
  pp (If c t f) = "(if " ++ pp c ++ " " ++ pp t ++ " " ++ pp f ++ ")"
  pp (SetBang v e) = "(set! " ++ pp v ++ " " ++ pp e ++ ")"	 
  pp (CallCC e) = "(call/cc " ++ pp e ++ ")"

-- Helper Functions
ppList xs = "(" ++ (unwords $ map pp xs) ++ ")"
ppDottedList xs v = "(" ++ (unwords $ map pp xs) ++ " . " ++ pp v ++ ")"
