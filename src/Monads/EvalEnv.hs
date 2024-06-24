{-# LANGUAGE LambdaCase #-}
module Monads.EvalEnv(
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
    , throwOneError
    , runEval
    ) where
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, MonadState (get, put), MonadTrans (lift), modify, evalState)

type FI = (Int, Int)

data Ty = Si String | Prod Ty Ty | Abs Ty Ty deriving Eq

instance Show Ty where
  show (Si s) = s
  show (Prod t1 t2) = "(" ++ show t1 ++ "," ++ show t2 ++ ")"
  show (Abs t1 t2)  = show t1 ++ "->" ++ show t2

data EvalError = 
      UndefinedType     FI Ty
    | BadTyped          FI Ty Ty
    | UnboundVariable   FI String
    | UndefinedBehavior FI
    | EndOfEval
    deriving Show

data EvalEnv = EvalEnv {
    typeSigs :: [Ty],
    binders  :: [(String, Ty)]
}

type EvalState t = ExceptT [EvalError] (State ([t], [t], EvalEnv))

throwOneError :: EvalError -> EvalState t ()
throwOneError = throwError . pure

defaultEnv :: EvalEnv
defaultEnv = EvalEnv 
    [Si "Int", Si "Bool", Si "Top", Si "Bottom"]
    []

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