-- Fachada publica del lenguaje: evita que la consola dependa de modulos internos.
module DSL
  ( Expr(..)
  , Dual(..)
  , EvalEnv
  , EvalM
  , EvalResult
  , ErrorType(..)
  , eval
  , evalWithEnv
  , evalDual
  , evalDualWithEnv
  , lookupVar
  , optimize
  , parseExpr
  , prettyPrint
  , prettyPrintOptimized
  , prettyPrintWithParens
  , mostrarError
  , runEvalM
  , throwEval
  ) where

import EvalM
import Evaluator
import Expr
import Parser
import PrettyPrinter
