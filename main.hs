module Main where

import Data.List (intersect)

import Types (Type(..), Expr(..))
import TypeChecker (typeCheck, TypeEnv, ValueEnv)
import Evaluator (eval, evalIO)
import Runner (runProgram)
import Control.Monad (void)


main = do
  -- program can call subprograms just like functions
  let routine2 =
        [
          Print (Assign (Var "hello") (Print (LString "Hello form routine2")))
        ]

  let routine1 =
        [
          Print (LString "Hello from routine1"),
          Assign (Var "x") (LInt 10),
          Assign (Var "y") (Add (Var "x") (LInt 10)),
          Assign (Var "z") (Add (Var "x") (Var "y")),
          Add (Var "x") (Var "y"),
          Call routine2,
          Call routine1,
          Assign (Var "w") (LComposite "Person" [
            ("name", LString "John"),
            ("age", LInt 20)
          ]),
          Assign (Var "a") (Get "name" (Var "w")),
          And (LList [LInt 1, LInt 2]) (LList [LInt 1, LInt 3, LInt 2]),
          And (LBool True) (LBool False),
          Or (LList [LInt 1, LInt 2]) (LList [LInt 1, LInt 3, LInt 2])
        ]

  let mainProgram = Call routine1

  runProgram (Call routine1)
  -- runProgram typeEnv valueEnv mainProgram
  -- runProgram typeEnv valueEnv program
