-- {-# OPTIONS_GHC -Wall #-}

module Evaluator (eval) where

import TypeChecker (TypeEnv, ValueEnv, typeCheck)
import Types (Expr(..), Type(..))
import Data.List (intersect, union)

import Debug.Trace (trace)

eval :: TypeEnv -> ValueEnv -> Expr -> Either String (TypeEnv, ValueEnv, Expr)
eval tenv venv (LInt x) = Right (tenv, venv, LInt x)
eval tenv venv (LBool x) = Right (tenv, venv, LBool x)
eval tenv venv (LString x) = Right (tenv, venv, LString x)
eval tenv venv (LList x) = Right (tenv, venv, LList x)

eval tenv venv (LTuple x) = do
  es <- mapM (eval tenv venv) x
  let results = map (\(_, _, v) -> v) es
  Right (tenv, venv, LTuple results)

eval tenv venv (Var x) = do
  y <- maybe (Left ("Var " ++ x ++ " is unbound")) Right (lookup x venv)
  Right (tenv, venv, y)

eval tenv venv (LComposite x ys) = do
  expressions <- mapM (\(_, y) -> eval tenv venv y) ys
  let v = map (\(_, _, y) -> y) expressions
  let w = map fst ys
  Right (tenv, venv, LComposite x (zip w v))

eval tenv venv (Assign (Var x) a) = do
  case typeCheck tenv a of
    Left e -> Left e
    Right (_, y) -> do
      (_, _, z) <- eval tenv venv a
      Right ((x, y):tenv, (x, z):venv, z)

eval tenv venv (Assign (LTuple x) a) = do
  let vs = [v | (Var v) <- x]
  case typeCheck tenv a of
    Right (_, TTuple rt) -> do
      (_, _, z) <- eval tenv venv a
      case z of
        LTuple t -> if length x /= length t
          then Left "Both side of the assignment should have same number of elements"
          else Right (foldl (flip (:)) tenv (zip vs rt), foldl (flip (:)) venv (zip vs t), z)
        _ -> Left "Right operand must be a tuple"
    _ -> Left "Right operand must of a tuple"

eval _ _ (Assign _ _) = Left "Assign requires a variable"

eval tenv venv (Add x y) = case typeCheck tenv (Add x y) of
  Left x -> Left x
  Right (_, TInt) -> do
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
    ((_, TComposite _ _), LComposite c props) ->
      maybe (Left ("Property " ++ x ++ " not found in " ++ c))
            (\val -> Right (tenv, venv, val))
            (lookup x props)
    _ -> Left "Get can only be applied on composite types"

eval tenv venv (Print x) = do
  (tenv, venv, y) <- eval tenv venv x
  Right (tenv, venv, x)

eval tenv venv NoOp = Right (tenv, venv, NoOp)

eval tenv venv (And x y) = do
  (_, _, x') <- eval tenv venv x
  (_, _, y') <- eval tenv venv y
  case typeCheck tenv (And x y) of
    Right (_, TInt) -> case (x', y') of
      (LInt a, LInt b) -> Right (tenv, venv, LInt a)
      _ -> Left "Both arguments should be of type int"
    Right (_, TBool) -> case (x', y') of
      (LBool a, LBool b) -> Right (tenv, venv, LBool (a && b))
      _ -> Left "Both arguments should be of type bool"
    Right (_, TList _) -> case (x', y') of
      (LList a, LList b) -> Right (tenv, venv, LList (a `intersect` b))
      _ -> Left "Both arguments should be of type list"
    Left t -> Left t

eval tenv venv (Or x y) = do
  (_, _, x') <- eval tenv venv x
  (_, _, y') <- eval tenv venv y
  case typeCheck tenv (And x y) of
    Right (_, TInt) -> case (x', y') of
      (LInt a, LInt b) -> Right (tenv, venv, LInt (if a == 0 then b else a))
      _ -> Left "Both arguments should be of type int"
    Right (_, TBool) -> case (x', y') of
      (LBool a, LBool b) -> Right (tenv, venv, LBool (a || b))
      _ -> Left "Both arguments should be of type bool"
    Right (_, TList _) -> case (x', y') of
      (LList a, LList b) -> Right (tenv, venv, LList (a `union` b))
      _ -> Left "Both arguments should be of type list"
    Left t -> Left t

eval tenv venv (Return xs) = do
  trace ("Evaluating " ++ show (Return xs) ++ "\n") (return ())
  t <- typeCheck tenv xs
  (tenv', venv', v) <- eval tenv venv xs
  case (t, v) of
    ((_, TTuple _), LTuple ys) -> do
      ex <- mapM (eval tenv' venv') ys
      let ex' = map (\(_, _, r) -> r) ex
      Right (tenv', venv', LTuple ex')
    _ -> Left "Function can only return tuples"

eval tenv venv (Func _ []) = Right (tenv, venv, LTuple [])
eval tenv venv (Func f xs) = do
  trace ("Evaluating " ++ show (Func f xs)) (return ())
  helper tenv venv xs
  where
    helper :: TypeEnv -> ValueEnv -> [Expr] -> Either String (TypeEnv, ValueEnv, Expr)
    helper tenv' venv' [] = Right (tenv', venv', LTuple [])
    helper tenv' venv' ((Return x):_) = do
      (tenv'', venv'', r) <- eval tenv' venv' (Return x)
      Right (tenv'', venv'', r)
    helper tenv' venv' (x':xs') = do
      (tenv'', venv'', r) <- eval tenv' venv' x'
      helper tenv'' venv'' xs'

eval tenv venv s@(Call (Func io ys) xs) = do
  let es = [v | Var v <- io]
  -- mapM won't work if xs has to keep it's env between statements
  -- an foldM can be used instead
  tv <- mapM (typeCheck tenv) xs
  let tv' = map snd tv
  ev <- mapM (eval tenv venv) xs

  let ev' = map (\(_, _, v) -> v) ev

  if length tv /= length ev
    then Left "Incorrect number of arguments passed to the function"
    else do
      let tenv' :: TypeEnv
          tenv' = zip es tv'
          venv' :: ValueEnv
          venv' = zip es ev'

      t <- typeCheck tenv' s

      r <- eval tenv' venv' (Func io ys)
      case r of
        (_, _, LTuple r) -> Right (tenv, venv, LTuple r)
        _ -> Left "Function did not return a tuple"

eval _ _ (Call _ _) = Left "Funtion is not invoked properly"
