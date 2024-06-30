{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Subtyping(isSubOf, makeSubGraph, putSubEdge, subs, notSubs) where
import Utils.EvalEnv
import Data.Graph (buildG, path)
import Data.Maybe (fromJust)
import GHC.Arr (indices, (!))
import Data.Map (keys, member)
import qualified Data.Map ((!))

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

isSubOf env (Tuple arr1) (Tuple arr2) 
    | length arr1 > length arr2 = False -- Rule :: Wide
    | otherwise = and $ do
        i <- indices arr1
        return $ isSubOf env (arr1 ! i) (arr2 ! i)

isSubOf env (Record m1) (Record m2)
    | length m1 > length m2 = False -- Rule :: Wide
    | otherwise = and $ do
        k <- keys m1
        return $ 
               member k m2 -- Rule :: Label
            && isSubOf env (m1 Data.Map.! k) (m2 Data.Map.! k)


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