module Runner (runProgram) where

import TypeChecker (TypeEnv, ValueEnv)
import Types (Type(..), Expr(..))
import Evaluator (eval)

runProgram :: TypeEnv -> ValueEnv -> [Expr] -> IO (Either String Expr)
runProgram _ venv [] = return (Right NoOp)
runProgram tenv venv ((Call x):xs) = do
  let tenv' :: TypeEnv
      tenv' = []
      venv' :: ValueEnv
      venv' = []
  runProgram tenv' venv' x
  runProgram tenv' venv xs

runProgram tenv venv ((Print x):xs) = do
  case eval tenv venv x of
    Right (_, _, y) -> do
      print y
      runProgram tenv venv xs
    Left x -> return (Left x)

runProgram tenv venv (x:xs) = do
  case eval tenv venv x of
    Right (tenv', venv', y) -> runProgram tenv' venv' xs
    Left x -> return (Left x)

