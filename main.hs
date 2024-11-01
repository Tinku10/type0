module Main where

import Data.List (intersect)

import Types (Type(..), Expr(..))
import TypeChecker (typeCheck, TypeEnv, ValueEnv)
import Evaluator (eval)

data IOEither a b = IOLeft (IO a) | IORight (IO b)

runProgram :: TypeEnv -> ValueEnv -> [Expr] -> IO (Either String Expr)
runProgram _ venv [] = return (Right NoOp)
runProgram tenv venv ((Print x):xs) = do
  case eval tenv venv x of
    Right (_, _, y) -> do
      print y
      runProgram tenv venv xs
    Left x -> return (Left x)
-- runProgram tenv venv (x:xs) = eval tenv venv x >>= \(tenv', venv', x') -> runProgram tenv' venv' xs
runProgram tenv venv (x:xs) = do
  case eval tenv venv x of
    Right (tenv', venv', y) -> runProgram tenv' venv' xs
    Left x -> return (Left x)

main = do
  let typeEnv = []
  let valueEnv = []

  let program =
        [
          Assign (Var "x") (LInt 10),
          Assign (Var "y") (Add (Var "x") (LInt 10)),
          Assign (Var "z") (Add (Var "x") (Var "y")),
          Print (Add (Var "x") (Var "y")),
          Assign (Var "w") (LComposite "Person" [
            ("name", LString "John"),
            ("age", LInt 20)
          ]),
          Print (Assign (Var "a") (Get "name" (Var "w")))
        ]

  runProgram typeEnv valueEnv program
