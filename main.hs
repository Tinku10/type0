module Main where

import Data.List (intersect)

import Types (Type(..), Expr(..))
import TypeChecker (typeCheck, TypeEnv, ValueEnv)
import Evaluator (eval)
import Runner (runProgram)
import Control.Monad (void)

main = do
  -- program can call subprograms just like functions
  let addNum = Func [Var "x", Var "y"]
        [
          Assign (Var "t") (LInt 1),
          Assign (Var "z") (Add (Var "x") (Var "y")),
          Return (LTuple [Add (Var "z") (Var "t")])
        ]

  let someFunc = Func []
        [
          Assign (Var "i") (LInt 10),
          Assign (Var "j") (Add (Var "i") (LInt 10)),
          Assign (LTuple [Var "add"]) (Call addNum [LInt 1, LInt 2]),
          Assign (Var "x") (Add (Var "add") (Var "j")),

          -- embed arguments into the child venv
          -- Assign (LTuple [Var "ans"]) (Call routine2 [LInt 10, LInt 20]),
          -- Assign (Var "w") (LComposite "Person" [
          --   ("name", LString "John"),
          --   ("age", LInt 20)
          -- ]),
          -- Assign (Var "a") (Get "name" (Var "w")),
          Assign (Var "z") (And (LList [LInt 1, LInt 2]) (LList [LInt 1, LInt 3, LInt 2])),
          -- And (LBool True) (LBool False),
          -- Or (LList [LInt 1, LInt 2]) (LList [LInt 1, LInt 3, LInt 2]),
          Return (LTuple [Var "z"])
        ]

  let mainProgram = Func [] 
        [
          Return (Call someFunc [])
        ]

  runProgram $ Call mainProgram []
