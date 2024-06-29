module Context(indexProc) where
import Lexing (Term(TmAbs, TmAbsC, TmAs, TmAsC, TmApp, TmProd, TmIfElse, TmProj), FITerm, TyTerm (..), FI)
import Utils.EvalEnv (EvalState, Ty (..), flow)
import Typing (lookupType)

{-
# Contexting:
    - Scan all the symbol definitions into EvalEnv
    - Index all the explicit symbols in AST
    - Throw errors on undefined symbols
-}

tyTermToTy :: FI -> TyTerm -> EvalState FITerm Ty
tyTermToTy fi (TmSiT s) = Si <$> lookupType fi s

tyTermToTy fi (TmProdT t1 t2) = Prod 
    <$> tyTermToTy fi t1 
    <*> tyTermToTy fi t2

tyTermToTy fi (TmAbsT t1 t2) = Abs 
    <$> tyTermToTy fi t1 
    <*> tyTermToTy fi t2

indexProc' :: FITerm -> EvalState FITerm FITerm
indexProc' (fi, TmAbs s tyT t) = do
    ty <- tyTermToTy fi tyT
    t' <- indexProc' t
    return (fi, TmAbsC s ty t')

indexProc' (fi, TmAs t tyT) = do
    ty <- tyTermToTy fi tyT
    t' <- indexProc' t
    return (fi, TmAsC t' ty)

indexProc' (fi, TmApp t1 t2) = do
    t1' <- indexProc' t1
    t2' <- indexProc' t2
    return (fi, TmApp t1' t2')

indexProc' (fi, TmProd t1 t2) = do
    t1' <- indexProc' t1
    t2' <- indexProc' t2
    return (fi, TmProd t1' t2')

indexProc' (fi, TmProj t i) = do
    t' <- indexProc' t
    return (fi, TmProj t' i)

indexProc' (fi, TmIfElse t1 t2 t3) = do
    t1' <- indexProc' t1
    t2' <- indexProc' t2
    t3' <- indexProc' t3
    return (fi, TmIfElse t1' t2' t3')

indexProc' t = pure t

indexProc :: EvalState FITerm ()
indexProc = flow indexProc'