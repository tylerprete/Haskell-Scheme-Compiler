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

fresh :: (Num a, Show a) => Supply a -> String -> (String, Var, Exp)
fresh sup prefix = (s,v,r)
    where
        n = supplyValue sup
        s = prefix ++ "$" ++ (show n)
        v = Var s
        r = Ref v

smarter_m s (Lambda (VarList vs) body) = (Lambda (VarList $ vk:vs) newbody)
    where
        (s1,s2) = (split2 s)
        (k, vk, rk) = fresh s1 "k"
        newbody = smarter_t s2 body rk

smarter_m s (Lambda (SingleVar v) body) = 
    (Lambda (DottedVarList [vk] v) newbody)
    where
        (s1,s2) = (split2 s)
        (k, vk, rk) = fresh s1 "k"
        newbody = smarter_t s2 body rk

smarter_m s (Lambda (DottedVarList vs v) body) = 
    (Lambda (DottedVarList (vk:vs) v) newbody)
    where
        (s1,s2) = (split2 s)
        (k, vk, rk) = fresh s1 "k"
        newbody = smarter_t s2 body rk

--smarter_m s (CallCC e) = error "Don't think this should be called..."
smarter_m s (CallCC _) = body where
    makeNames str = (str, Var str, Ref $ Var str)
    (f_, vf_, rf_) = makeNames "f"
    (k_, vk_, rk_) = makeNames "k"
    (v_, vv_, rv_) = makeNames "v"
    (k0_,vk0_,rk0_)= makeNames "k0"
    innerbody = App rf_ [rk_, Lambda    (VarList [vk0_, vv_])
                        (App rk_ [rv_])]
    body = Lambda (VarList [vk_,vf_]) innerbody

smarter_m _ e | isAtom e = e

smarter_m _ _ = error "smarter_m called with not-atomic Exp"

-- convcallcc is the lambda body that callcc becomes
convcallcc = body where
    makeNames str = (str, Var str, Ref $ Var str)
    (f_, vf_, rf_) = makeNames "f"
    (k_, vk_, rk_) = makeNames "k"
    (v_, vv_, rv_) = makeNames "v"
    (k0_,vk0_,rk0_)= makeNames "k0"
    innerbody = App rf_ [rk_, Lambda    (VarList [vk0_, vv_])
                        (App rk_ [rv_])]
    body = Lambda (VarList [vk_,vf_]) innerbody

-- Smarter_t applies continuation to expressions, converting arguments to cps
-- as necessary

smarter_t :: (Num a, Show a) => Supply a -> Exp -> Exp -> Exp
smarter_t s e@(Lambda _ _) q = App q [smarter_m s e]

smarter_t s e@(CallCC e2) q = body where
    (s1,s2,s3) = split3 s
    (_,ve2,re2) = fresh s1 "tmp"
    body = smarter_t s2 e2 $ Lambda (VarList [ve2]) $ App q [App convcallcc [re2]]
    

smarter_t _ e q | isAtom e = App q [e]

smarter_t s (SetBang v e2) q@(Ref _) | isAtom e2 = newSB where
    newSB = App q $ [SetBang v (smarter_m s e2)]

smarter_t s e@(SetBang v e2) q@(Ref _) = newSB where
    (sret,s1,s2) = (split3 s)
    (_, vf_, rf_) = fresh sret "retval"
    newSB = smarter_t s1 e2 $ Lambda (VarList [vf_]) (smarter_t s2 (SetBang v rf_) q)

smarter_t s e@(SetBang v e2) q | isAtom e2 = App newSB [q] where
    (st,s1) = split2 s
    (_, vt_, rt_) = fresh st "tmp"
    newSB = Lambda  (VarList [vt_]) $
            App rt_ $ [SetBang v (smarter_m s1 e2)]

smarter_t s e@(SetBang v e2) q = App newSB [q] where
    (st:sret:s1:s2:_) = split s
    (_, vt_, rt_) = fresh st "tmp"
    (_, vf_, rf_) = fresh sret "retval"
    inSB = smarter_t s1 e2 $ Lambda (VarList [vf_]) 
                    (App rt_ [SetBang v rf_])
    newSB = Lambda  (VarList [vt_]) inSB

smarter_t s (If c t f) q@(Ref _) | isAtom c = newIf where
    (s1,s2,s3) = (split3 s)
    newIf = If (smarter_m s1 c) (smarter_t s2 t q) (smarter_t s3 f q)

smarter_t s (If c t f) q@(Ref _) = e where
    (sret:s1:s2:s3:_) = split s
    (_, vf_, rf_) = fresh sret "retval"
    e = smarter_t s1 c (Lambda (VarList [vf_])
                (If rf_ (smarter_t s2 t q) (smarter_t s3 f q)))

smarter_t s (If c t f) q | isAtom c = App body [q] where
    (sret:s1:s2:s3:_) = split s
    (t_, vt_, rt_) = fresh sret "tmp"
    newif = If (smarter_m s1 c) (smarter_t s2 t rt_) (smarter_t s3 f rt_)
    body = Lambda (VarList [vt_]) newif

smarter_t s (If c t f) q = App body [q] where
    (s1:s2:s3:s4:s5:_) = split s
    (t_, vt_, rt_) = fresh s1 "tmp"
    (f_, vf_, rf_) = fresh s2 "retval"
    newt = smarter_t s3 t rt_
    newf = smarter_t s4 f rt_
    newif = If rf_ newt newf
    innerlam = Lambda (VarList [vf_]) newif
    innerbody = smarter_t s5 c innerlam
    body = Lambda (VarList [vt_]) innerbody

smarter_t s (App f exps) q | isAtom f && all isAtom exps = e where
    s1:xs = split s
    e = App (smarter_m s1 f) $ q:(map (\(e,sp) -> smarter_m sp e) (zip exps xs))

smarter_t s (App f exps) q | isAtom f = fst res where
    (s1:s2:xs) = (split s)
    smartf = smarter_m s1 f
    expSupplys = zip exps xs
    newExps = map (\(e,sp) -> if (not $ isAtom e)
                then let (e_,ve_,re_) = fresh sp "retval" in
                     (re_,True)
                else (smarter_m sp e, False)) expSupplys
    nvTest (Ref _, True) = True
    nvTest (_,_) = False
    newVars = map (\(Ref v, _) -> v) (filter nvTest newExps)
    newExps2 = map fst newExps
    k = App smartf $ q:newExps2
    startVals = (k, reverse newVars)
    foldFunc (e, _) v | isAtom e = v
    foldFunc (e, sp) (e2, v:vs) = (smarter_t sp e (Lambda (VarList [v]) e2), vs)
    res = foldr foldFunc startVals (zip exps (split s2))

smarter_t s (App f exps) q | all isAtom exps = 
    smarter_t s2 f (Lambda (VarList [vf_]) (App rf_ $ q:exps)) where
    (s1,s2) = (split2 s)
    (_, vf_, rf_) = fresh s1 "retval"

smarter_t s (App f exps) q = smarter_t s3 f (Lambda (VarList [vf_]) (fst res)) where
    (s1:s2:s3:xs) = (split s)
    (_, vf_, rf_) = fresh s1 "retval"
    expSupplys = zip exps xs
    newExps = map (\(e,sp) -> if (not $ isAtom e)
                then let (e_,ve_,re_) = fresh sp "retval" in
                     (re_,True)
                else (smarter_m sp e, False)) expSupplys
    nvTest (Ref _, True) = True
    nvTest (_,_) = False
    newVars = map (\(Ref v, _) -> v) (filter nvTest newExps)
    newExps2 = map fst newExps
    k = App rf_ $ q:newExps2
    startVals = (k, reverse newVars)
    foldFunc (e, _) v | isAtom e = v
    foldFunc (e, sp) (e2, v:vs) = (smarter_t sp e (Lambda (VarList [v]) e2), vs)
    res = foldr foldFunc startVals (zip exps (split s2))

smarter_t _ _ _ = error "Not yet implemented"

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
isAtom (CallCC _) = True
-- False for other types
isAtom _ = False
