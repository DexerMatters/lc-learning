module Desugar(desugarProc) where
import Lexing
import Utils.EvalEnv

desugarProc' :: FITerm -> EvalState FITerm FITerm
desugarProc' (fi, TmAbs s ty t) = do
    t' <- desugarProc' t
    return (fi, TmAbs s ty t')

desugarProc' (fi, TmAs t ty) = do
    t' <- desugarProc' t
    return (fi, TmAs t' ty)

desugarProc' (fi, TmApp t1 t2) = do
    t1' <- desugarProc' t1
    t2' <- desugarProc' t2
    return (fi, TmApp t1' t2')

desugarProc' (fi, TmProd t1 t2) = do
    t1' <- desugarProc' t1
    t2' <- desugarProc' t2
    return (fi, TmProd t1' t2')

desugarProc' (fi, TmIfElse t1 t2 t3) = do
    t1' <- desugarProc' t1
    t2' <- desugarProc' t2
    t3' <- desugarProc' t3
    return (fi, TmIfElse t1' t2' t3')

desugarProc' (fi, TmLetIn s ty t1 t2) = do
    t1' <- desugarProc' t1
    t2' <- desugarProc' t2
    return (fi, TmApp (fi, TmAbs s ty t2') t1')

desugarProc' t = pure t

desugarProc :: EvalState FITerm ()
desugarProc = flow desugarProc'