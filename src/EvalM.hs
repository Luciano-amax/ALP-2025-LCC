module EvalM
  ( EvalEnv
  , EvalM
  , EvalResult
  , ErrorType(..)
  , runEvalM
  , throwEval
  , lookupVar
  ) where

import Control.Applicative (Alternative(..))

data ErrorType
  = DivideByZero
  | UndefinedVariable String
  | DomainError String
  deriving (Show, Eq)

type EvalResult = Either ErrorType Double
type EvalEnv a = [(String, a)]

-- Monada de evaluacion: combina entorno de variables y errores.
newtype EvalM env a = EvalM (EvalEnv env -> Either ErrorType a)

runEvalM :: EvalEnv env -> EvalM env a -> Either ErrorType a
runEvalM env (EvalM action) = action env

throwEval :: ErrorType -> EvalM env a
throwEval err = EvalM $ \_ -> Left err

lookupVar :: String -> EvalM env env
lookupVar name = EvalM $ \env ->
  case lookup name env of
    Just value -> Right value
    Nothing -> Left (UndefinedVariable name)

instance Functor (EvalM env) where
  fmap f action = EvalM $ \env -> fmap f (runEvalM env action)

instance Applicative (EvalM env) where
  pure value = EvalM $ \_ -> Right value
  f <*> value = EvalM $ \env ->
    runEvalM env f <*> runEvalM env value

instance Monad (EvalM env) where
  action >>= next = EvalM $ \env -> do
    value <- runEvalM env action
    runEvalM env (next value)

instance Alternative (EvalM env) where
  empty = throwEval $ DomainError "empty evaluation"
  left <|> right = EvalM $ \env ->
    case runEvalM env left of
      Left _ -> runEvalM env right
      result -> result
