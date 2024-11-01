module Main where

import Data.List (intersect)

import Types (Type(..), Expr(..))
import TypeChecker (typeCheck, TypeEnv, ValueEnv)
import Evaluator (eval)
import Runner (runProgram)


main = do
  let typeEnv :: TypeEnv
      typeEnv = []
      valueEnv :: ValueEnv
      valueEnv = []

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
          Print (Add (Var "x") (Var "y")),
          Call routine2,
          Assign (Var "w") (LComposite "Person" [
            ("name", LString "John"),
            ("age", LInt 20)
          ]),
          Print (Assign (Var "a") (Get "name" (Var "w"))),
          Print (And (LList [LInt 1, LInt 2]) (LList [LInt 1, LInt 3, LInt 2])),
          Print (And (LBool True) (LBool False)),
          Print (Or (LList [LInt 1, LInt 2]) (LList [LInt 1, LInt 3, LInt 2]))
        ]

  let mainProgram = 
        [
          Call routine1
        ]

  runProgram typeEnv valueEnv mainProgram
  -- runProgram typeEnv valueEnv program
