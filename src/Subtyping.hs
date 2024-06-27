{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Subtyping where
import Utils.EvalEnv
import Data.Graph (buildG, path)
import Control.Monad
import Data.List (elemIndex)
import Data.Functor ((<&>))

putSubEdge :: (Int, Int) -> EvalState t ()
putSubEdge s = modifyEnv $
    \EvalEnv{subEdges, ..} -> EvalEnv{subEdges = s:subEdges, ..}

makeSubGraph :: EvalState t ()
makeSubGraph = modifyEnv $
    \EvalEnv{subEdges,typeSigs, ..} -> EvalEnv {
          subGraph = Just $ buildG (0, length typeSigs - 1) subEdges
        , ..
    }

isSubOf :: Ty -> Ty -> EvalState t Bool
isSubOf a b | a == b = return True -- Invariance

-- For covariance of singletons
isSubOf (Si a) (Si b) = do
    sigs  <- getEnv <&> typeSigs
    graph <- getEnv >>= fromJust . subGraph
    ia    <- fromJust $ elemIndex a sigs
    ib    <- fromJust $ elemIndex b sigs
    return $ path graph ia ib

isSubOf (Abs a as) (Abs b bs) = liftM2 (&&)
    (isSubOf b a)   -- Contravariance for parameter type
    (isSubOf as bs) -- Covariance for return type

isSubOf (Prod a as) (Prod b bs) = liftM2 (&&)
    (isSubOf a b)
    (isSubOf as bs)
    
isSubOf _ _ = pure False