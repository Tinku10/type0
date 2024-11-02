module Runner (runProgram) where

import TypeChecker (TypeEnv, ValueEnv)
import Types (Type(..), Expr(..))
import Evaluator (eval, evalIO)

runProgram :: Expr -> IO (Either String Expr)
runProgram x = do
  let tenv :: TypeEnv
      tenv = []
      venv :: ValueEnv
      venv = []

  res <- evalIO tenv venv x
  case res of
    Left e -> return $ Left e
    Right (_, _, a) -> return $ Right a
