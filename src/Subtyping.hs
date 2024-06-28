{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Subtyping(isSubOf, makeSubGraph, putSubEdge, subs, notSubs) where
import Utils.EvalEnv hiding (fromJust)
import Data.Graph (buildG, path)
import Data.Maybe (fromJust)

putSubEdge :: (Int, Int) -> EvalState t ()
putSubEdge s = modifyEnv $
    \EvalEnv{subEdges, ..} -> EvalEnv{subEdges = s:subEdges, ..}

makeSubGraph :: EvalState t ()
makeSubGraph = modifyEnv $
    \EvalEnv{subEdges,typeSigs, ..} -> EvalEnv {
          subGraph = Just $ buildG (0, length typeSigs - 1) subEdges
        , ..
    }

isSubOf :: EvalEnv -> Ty -> Ty -> Bool
isSubOf _ a b | a == b = True -- Invariance

-- For covariance of singletons
isSubOf env (Si a) (Si b) = path (fromJust (subGraph env)) b a

isSubOf env (Abs a as) (Abs b bs) = (&&)
    (isSubOf env b a)   -- Contravariance for parameter type
    (isSubOf env as bs) -- Covariance for return type

isSubOf env (Prod a as) (Prod b bs) = (&&)
    (isSubOf env a b)
    (isSubOf env as bs)

isSubOf _ _ _ = False

subs :: Ty -> Ty -> EvalState t a -> EvalState t a -> EvalState t a
subs t1 t2 e1 e2 = do
    env <- getEnv
    if   isSubOf env t1 t2
    then e1
    else e2

notSubs :: Ty -> Ty -> EvalState t a -> EvalState t a -> EvalState t a
notSubs t1 t2 e1 e2 = do
    env <- getEnv
    if   not $ isSubOf env t1 t2
    then e1
    else e2