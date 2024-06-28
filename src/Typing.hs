{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Typing(typeof, lookupType, resetBinder) where
import Utils.EvalEnv (EvalState, check, modifyEnv, EvalEnv (..), EvalError (..), Ty(..), getEnv, next, refresh)
import Lexing (FITerm, Term (..), Ground (GBool, GInt), FI)
import Control.Monad.Except (throwError)
import Data.List (elemIndex)
import Subtyping (subs, notSubs)
import Control.Monad.RWS (modify)
import Control.Applicative (Alternative(many))


putBinder :: (String, Ty) -> EvalState FITerm ()
putBinder s = modifyEnv $
    \EvalEnv{binders, ..} -> EvalEnv{binders = s:binders, ..}

lookupBinder :: FI -> String -> EvalState FITerm Ty
lookupBinder fi s = do
    binders <- binders <$> getEnv
    case lookup s binders of
        Just t  -> return t
        Nothing -> throwError [UnboundVariable fi s]

resetBinder :: EvalState FITerm ()
resetBinder = modifyEnv $ 
    \EvalEnv{..} -> EvalEnv{binders = [], ..} 

lookupType :: FI -> String -> EvalState FITerm Int
lookupType fi s = do
    tys <- typeSigs <$> getEnv
    maybe 
        (throwError [UndefinedType fi s])
        return
        (elemIndex s tys)

returnSi :: FI -> String -> EvalState FITerm Ty
returnSi fi s = Si <$> lookupType fi s

typeof' :: FITerm -> EvalState FITerm Ty
typeof' (fi, TmLit (GBool _)) = returnSi fi "Bool"

typeof' (fi, TmLit (GInt _))  = returnSi fi "Int"

typeof' (_, TmAbsC "_" ty t) = Abs ty <$> typeof' t -- wildcard

typeof' (_, TmAbsC v ty t) = putBinder (v, ty) >> (Abs ty <$> typeof' t)

typeof' (fi, TmVar s) = lookupBinder fi s

typeof' (fi, TmApp f p) = do
    fT <- typeof' f
    pT <- typeof' p -- Type of input parameter
    case fT of
        Abs pT' rT -> 
            subs pT pT'
                (return rT)
                (throwError [BadTyped fi pT' pT])
        _ -> throwError [BadTypedS fi fT "Function Type"]

typeof' (_, TmProd t1 t2) = Prod <$> typeof' t1 <*> typeof' t2

typeof' (fi, TmIfElse t1 t2 t3) = do
    ty1 <- typeof' t1
    ty2 <- typeof' t2
    ty3 <- typeof' t3
    b   <- returnSi fi "Bool"
    notSubs ty1 b
        (throwError [BadTyped fi b ty1])
        $ do 
            subs ty2 ty3 
                (return ty3)
                (subs ty3 ty2
                    (return ty2)
                    (throwError [BadTyped fi ty2 ty3])
                )

typeof' (fi, TmAsC t ty) = do
    tT <- typeof' t
    subs tT ty
        (return ty)
        (throwError [BadTyped fi tT ty])

typeof' (fi, _) = throwError [UndefinedBehavior fi]

typeof :: EvalState FITerm [Ty]
typeof = many ((next >>= typeof') <* resetBinder) <* refresh
