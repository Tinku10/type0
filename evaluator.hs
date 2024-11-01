module Evaluator (eval) where

import TypeChecker (TypeEnv, ValueEnv, typeCheck)
import Types (Expr(..), Type(..))

eval :: TypeEnv -> ValueEnv -> Expr -> Either String (TypeEnv, ValueEnv, Expr)
eval tenv venv (LInt x) = Right (tenv, venv, LInt x)
eval tenv venv (LBool x) = Right (tenv, venv, LBool x)
eval tenv venv (LString x) = Right (tenv, venv, LString x)
eval tenv venv (LList x) = Right (tenv, venv, LList x)
eval tenv venv (Var x) = do
  y <- maybe (Left ("Var " ++ x ++ " is unbound")) Right (lookup x venv)
  Right (tenv, venv, y)

eval tenv venv (LComposite x ys) = do
  expressions <- mapM (\(_, y) -> eval tenv venv y) ys
  let v = map (\(_, _, y) -> y) expressions
  let w = map fst ys
  Right (tenv, venv, LComposite x (zip w v))

eval tenv venv (Assign (Var x) a) =
  case typeCheck tenv a of
    Left x -> Left x
    Right y -> do
      (tenv, venv, z) <- eval tenv venv a
      Right ((x, y):tenv, (x, z):venv, z)
eval tenv venv (Assign _ _) = Left "Assign requires a variable"

eval tenv venv (Add x y) = case typeCheck tenv (Add x y) of
  Left x -> Left x
  Right TInt -> do
    (_, _, a) <- eval tenv venv x
    (_, _, b) <- eval tenv venv y
    case (a, b) of
      (LInt x, LInt y) -> Right (tenv, venv, LInt (x + y))
      _ -> Left "Addtion can only be performed on integers"
  _ -> Left "Addtion can only be performed on integers"

eval tenv venv (Get x y) = do
  (_, _, z) <- eval tenv venv y
  t <- typeCheck tenv z
  case (t, z) of
    (TComposite _ _, LComposite c props) ->
      maybe (Left ("Property " ++ x ++ " not found in " ++ c)) 
            (\val -> Right (tenv, venv, val)) 
            (lookup x props)
    _ -> Left "Get can only be applied on composite types"

eval tenv venv (Print x) = do
  (tenv, venv, y) <- eval tenv venv x
  Right (tenv, venv, x)

eval tenv venv NoOp = Right (tenv, venv, NoOp)


  -- case (x, y) of
  --   (LInt a, LInt b) -> Right (tenv, venv, LInt (a + b))
  --   (Var a, LInt b) -> do
  --     case lookup a venv of
  --       Just (LInt c) -> Right (tenv, venv, LInt (c + b))
  --       Just _ -> Left "Var should be of type integer"
  --       Nothing -> Left "Unbound variable found"
  --   (LInt a, Var b) -> do
  --     case lookup b venv of
  --       Just (LInt c) -> Right (tenv, venv, LInt (a + c))
  --       Just _ -> Left "Var should be of type integer"
  --       Nothing -> Left "Unbound variable found"
  --   (Var a, Var b) -> do
  --     c <- case lookup b venv of
  --       Just (LInt c) -> Right c
  --       Just _ -> Left "Var should be of type integer"
  --       Nothing -> Left "Unbound variable found"
  --     d <- case lookup a venv of
  --       Just (LInt d) -> Right d
  --       Just _ -> Left "Var should be of type integer"
  --       Nothing -> Left "Unbound variable found"
  --     Right (tenv, venv, LInt (c + d))
  --   _ -> Left "Both arguments should be of type integer"
  -- _ -> Left "Can only add integers"
