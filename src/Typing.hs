{-# LANGUAGE NamedFieldPuns #-}
module Typing where
import Monads.EvalEnv (EvalState, check, modifyEnv, EvalEnv (..), EvalError (..), Ty(..), throwOneError, getEnv)
import Lexing (FITerm, Term (TmLit, TmAbs, TmApp, TmVar, TmAs, TmIfElse), Ground (GBool, GInt), FI)
import Control.Monad.Except (throwError)

putBinder :: (String, Ty) -> EvalState FITerm ()
putBinder s = modifyEnv $
    \EvalEnv{binders, typeSigs} -> EvalEnv{binders = s:binders, typeSigs}

lookupBinder :: FI -> String -> EvalState FITerm Ty
lookupBinder fi s = do
    binders <- binders <$> getEnv
    case lookup s binders of
        Just t  -> return t
        Nothing -> throwError [UnboundVariable fi s]

typeof' :: FITerm -> EvalState FITerm Ty
typeof' (_, TmLit (GBool _)) = return $ Si "Bool"

typeof' (_, TmLit (GInt _))  = return $ Si "Int"

typeof' (_, TmAbs v ty t) = putBinder (v, ty) >> (Abs ty <$> typeof' t)

typeof' (fi, TmVar s) = lookupBinder fi s

typeof' (fi, TmApp f p) = do
    fT <- typeof' f
    pT <- typeof' p
    case fT of
        Abs pT' rT -> 
            if   pT == pT' 
            then return rT
            else throwError [BadTyped fi pT' pT]
        _ -> throwError [BadTyped fi fT $ Si "Function Type"]

typeof' (fi, TmIfElse t1 t2 t3) = do
    ty1 <- typeof' t1
    ty2 <- typeof' t2
    ty3 <- typeof' t3
    if   ty1 /= Si "Bool" 
    then throwError [BadTyped fi (Si "Bool" ) ty1]
    else 
        if   ty2 == ty3
        then return ty2
        else throwError [BadTyped fi ty2 ty3]


typeof' (fi, TmAs t ty) = do
    tT <- typeof' t
    if   tT == ty
    then return ty
    else throwError [BadTyped fi tT ty]

typeof' (fi, _) = throwError [UndefinedBehavior fi]

typeof :: EvalState FITerm Ty
typeof = check >>= typeof'