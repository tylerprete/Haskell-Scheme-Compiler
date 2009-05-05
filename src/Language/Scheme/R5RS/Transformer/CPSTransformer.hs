-----------------------------------------------------------------------------
-- |
-- Module      : Language.Scheme.R5RS.Transformer.CPSTransformer
-- Copyright   : (c) Tyler Prete 
-- License     : BSD-style
-- Maintainer  : psyonic@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Transforms AST to AST in CPS form
-- See: <http://people.csail.mit.edu/jaffer/r5rs_9.html#SEC72>
-----------------------------------------------------------------------------

module Language.Scheme.R5RS.Transformer.CPSTransformer
where
import Language.Scheme.R5RS.Syntax.Expression

import Data.Supply -- Used for generating unique values

fresh :: (Num a) => [Supply a] -> String -> ([Supply a], (String, Var, Exp))
fresh (x:xs) prefix = (xs, (s,v,r))
	where
		n = supplyValue x
		s = prefix ++ "$" ++ (show n)
		v = Var s
		r = Ref v
		{-
 			n <- return $ supplyValue x
			s <- return $ prefix ++ "$" ++ (show n)
			v <- return $ Var s
			r <- return $ Ref v
			return (xs, (s,v,r))
			-}

smarter_m :: (Num a) => [Supply a] -> Exp -> ([Supply a], Exp)
smarter_m xs e@(Ref v) = (xs,e)

--smarter_m (Lambda (SingleVar v) body) = Lambda (
smarter_m xs (Lambda (VarList vs) body) = 
	(xs2, (Lambda (VarList $ vs ++ [vk]) newbody))
	where
		(xs1, (k, vk, rk)) = fresh xs "k"
		(xs2, newbody) = smarter_t xs body rk
smarter_m xs (Lambda _ body) = error "Lambda Parameter type not yet implemented."

smarter_m xs e | isAtomNotLambda e = (xs,e)

smarter_m xs e = error "smarter_m called with not-atomic Exp"

smarter_t :: (Num a) => [Supply a] -> Exp -> Exp -> ([Supply a], Exp)
smarter_t xs e q | isAtomNotLambda e = (xs, App q [e])
smarter_t xs e@(Lambda _ _) q = (xs1, App q [e1]) where
	(xs1,e1) = smarter_m xs e

-- This can be broken down into 4 cases to generate smaller code, but for now
-- I'm using the general case to get things working
smarter_t xs (If c t f) q = (xs4, App body [q]) where
	(xs1, (t_, vt_, rt_)) = fresh xs "tmp"
	(xs2, (f_, vf_, rf_)) = fresh xs1 "retval"
	(xs3, newt) = smarter_t xs2 t rt_
	(xs4, newf) = smarter_t xs3 f rt_
	newif = If rf_ newt newf
	innerlam = Lambda (VarList [vf_]) newif
	(xs5, innerbody) = smarter_t xs4 c innerlam
	body = Lambda (VarList [vt_]) innerbody

-- This can also be broken down into 4 cases to generate smaller code, but for now
-- I'm using the general case to get things working
{- smarter_t xs (App f exps) q = (xs9, e9) where
	(xs1, (f_, vf_, rf_)) = fresh xs "retval"
	newExps = map (\e -> if (not isAtom(e))
				then let ((xs2, (e_,ve_,re_)) = fresh xs1 "retval" in
-}

smarter_t _ _ _ = error "Not yet implemented"




{-
-- Number Constructors (Base 10 assumed)
data Number
	= Real Float
--	| Complex Complex Float
	| Integer Int -- Integer Integer (Using Int as a simplification for now)
	deriving (Eq, Show, Ord)

data Var = Var String
	deriving Show

data Formals	= SingleVar Var
		| VarList [Var]
		| DottedVarList [Var] Var
	deriving Show

data Exp	= Ref Var -- Literals (Values)
		| Boolean Bool
		| Number Number
		| Character Char
		| String String
		| Symbol String
		| List [Exp]
		| DottedList [Exp] Exp
		| Vector [Exp]
		| Lambda Formals Exp

		| App Exp [Exp]
		| If Exp Exp Exp
		| SetBang Var Exp
	deriving Show
-}

-- Helper functions
isAtom :: Exp -> Bool
-- True for all Value types
isAtom (Ref _) = True
isAtom (Boolean _) = True
isAtom (Number _) = True
isAtom (String _) = True
isAtom (Symbol _) = True
isAtom (List _) = True
isAtom (DottedList _ _) = True
isAtom (Vector _) = True
isAtom (Lambda _ _) = True
-- False for other types
isAtom _ = False

isAtomNotLambda :: Exp -> Bool
isAtomNotLambda (Lambda _ _) = False
isAtomNotLambda e = isAtom e
