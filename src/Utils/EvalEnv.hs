{-# LANGUAGE LambdaCase #-}
module Utils.EvalEnv(
      EvalState
    , EvalEnv(..)
    , EvalError(..)
    , Ty(..)
    , defaultEnv
    , check
    , next
    , pop
    , refresh
    , getEnv
    , putEnv
    , modifyEnv
    , runEval
    , fromJust
    ) where
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, MonadState (get, put), MonadTrans (lift), modify, evalState)
import Data.Map (insert, Map)
import Data.Graph (Graph)
import Data.IntMap (IntMap, fromList)

type FI = (Int, Int)

data Ty = Si String | Prod Ty Ty | Abs Ty Ty deriving Eq

instance Show Ty where
  show (Si s) = s
  show (Prod t1 t2) = "(" ++ show t1 ++ "," ++ show t2 ++ ")"
  show (Abs t1 t2)  = show t1 ++ "->" ++ show t2

data EvalError = 
      UndefinedType     FI String
    | BadTyped          FI Ty Ty
    | UnboundVariable   FI String
    | UndefinedBehavior FI

    | InternalError
    | EndOfEval
    deriving Show

data EvalEnv = EvalEnv {
    typeSigs :: [String],
    binders  :: [(String, Ty)],
    subEdges :: [(Int, Int)],
    subGraph :: Maybe Graph
}

type EvalState t = ExceptT [EvalError] (State ([t], [t], EvalEnv))

fromJust :: Maybe a -> EvalState t a
fromJust = maybe (throwError [InternalError]) return

defaultEnv :: EvalEnv
defaultEnv = EvalEnv 
    ["Top", "Int", "Bool", "Bot"]
    []
    [(0, 1), (1, 2), (2, 3)]
    Nothing

check :: EvalState t t
check = lift get >>= \case 
    (hd:_, _, _) -> return hd 
    _ -> throwError [EndOfEval]

next :: EvalState t t
next = lift get >>= \case 
    (a:as, bs, e) -> put (as, a:bs, e) >> return a
    _ -> throwError [EndOfEval]

pop :: EvalState t t
pop = lift get >>= \case 
    (a:as, bs, e) -> put (as, bs, e) >> return a
    _ -> throwError [EndOfEval]

refresh :: EvalState t ()
refresh = modify $ \(a, b, e) -> (b, a, e)

getEnv :: EvalState t EvalEnv
getEnv = lift get >>= \case (_, _, e) -> return e

putEnv :: EvalEnv -> EvalState t ()
putEnv env = modify $ \(a, b, _) -> (a, b, env)

modifyEnv :: (EvalEnv -> EvalEnv) -> EvalState t ()
modifyEnv f = getEnv >>= \env -> putEnv (f env)

runEval :: [t] -> EvalState t a -> Either [EvalError] a
runEval t s = evalState (runExceptT s) (t, [], defaultEnv)