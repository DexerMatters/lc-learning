{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Typing(typeof, lookupType, resetBinder, putBinders) where
import Utils.EvalEnv (EvalState, modifyEnv, EvalEnv (..), EvalError (..), Ty(..), getEnv, next, refresh, isEmpty)
import Lexing (FITerm, Term (..), Ground (GBool, GInt), FI, PtTerm (..))
import Control.Monad.Except (throwError)
import Data.List (elemIndex)
import Subtyping (subs, notSubs)
import Data.Bool (bool)
import Control.Monad (liftM2, join)
import GHC.Arr
import Data.Map (member, keys)
import qualified Data.Map ((!))

typeof' :: FITerm -> EvalState FITerm Ty
typeof' (fi, TmLit (GBool _)) = returnSi fi "Bool"

typeof' (fi, TmLit (GInt _))  = returnSi fi "Int"

typeof' (fi, TmAbsC v ty t) =
        dePattern fi v ty
    >>= putBinders
    >> (Abs ty <$> typeof' t)

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

typeof' (_, TmTuple arr) = Tuple <$> typeof' `mapM` arr

typeof' (fi, TmProj t i) = typeof' t  >>= \case
    (Tuple arr) ->
        if   inRange (bounds arr) i
        then return (arr ! i)
        else throwError [ProjOutOfBound fi (length arr - 1) i]
    d -> throwError [BadTypedS fi d "Tuple Type"]

typeof' (_, TmRecord m) = Record <$> typeof' `mapM` m

typeof' (fi, TmRcdProj t l) = typeof' t  >>= \case
    (Record m) ->
        if   member l m
        then return (m Data.Map.! l)
        else throwError [LabelNotFound fi l]
    d -> throwError [BadTypedS fi d "Record Type"]

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
typeof = (<* refresh) $ do
    isEmpty >>= bool
        (liftM2 (:) (next >>= typeof') typeof)
        (return [])

putBinders :: [(String, Ty)] -> EvalState FITerm ()
putBinders s = modifyEnv $
    \EvalEnv{binders, ..} -> EvalEnv{binders = s ++ binders, ..}

dePattern :: FI -> PtTerm -> Ty -> EvalState FITerm [(String, Ty)]
dePattern _ (TmSiP s) ty | s /= "_"  = return [(s, ty)]
                         | otherwise = return []

dePattern fi (TmTupleP ts) (Tuple tys)
    | length ts /= length tys = throwError [PatternBadMatched fi]
    | otherwise = join <$> mapM f (
        do
            i <- indices ts
            return (ts ! i, tys ! i))
    where f (ts', tys') = dePattern fi ts' tys'

dePattern fi (TmRecordP tm) (Record tym)
    | length tm /= length tym = throwError [PatternBadMatched fi]
    | otherwise = join <$> mapM f (
        do
            l <- keys tm
            return (tm Data.Map.! l, tym Data.Map.! l))
    where f (ts', tys') = dePattern fi ts' tys'

dePattern fi _ _ = throwError [PatternBadMatched fi]

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