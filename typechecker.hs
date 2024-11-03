{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
module TypeChecker (typeCheck, TypeEnv, ValueEnv) where

import Types (Type(..), Expr(..))
import Debug.Trace (trace)
import Control.Monad (foldM)

type TypeEnv = [(String, Type)]
type ValueEnv = [(String, Expr)]

typeCheck :: TypeEnv -> Expr -> Either String (TypeEnv, Type)
typeCheck env (LInt x) = Right (env, TInt)
typeCheck env (LBool x) = Right (env, TBool)
typeCheck env (LString x) = Right (env, TString)
typeCheck env (Var x) = maybe (Left $ "Var " ++ x ++ " not found") (\t -> Right (env, t)) (lookup x env)

typeCheck env (LComposite x xs) = do
  ts <- mapM (typeCheck env . snd) xs
  let props = map fst xs
  let ts' = map snd ts
  Right (env, TComposite x (zip props ts'))

typeCheck env (LList (x:xs)) = do
  (_, t1) <- typeCheck env x
  t2 <- mapM (typeCheck env) xs
  let t2' = map snd t2
  if all (== t1) t2' then Right (env, TList t1) else Left "List should be homogeneous"

typeCheck env (LTuple xs) = do
  ts <- mapM (typeCheck env) xs
  let ts' = map snd ts
  Right (env, TTuple ts')

typeCheck env (Get x y) = do
  (_, t) <- typeCheck env y
  case t of
    TComposite c props -> maybe (Left ("Property " ++ x ++ " does not exist in " ++ c)) (\t -> Right (env, t)) (lookup x props)
    _ -> Left (show y ++ " is not a composite type")

typeCheck env (Add x y) = do
  (_, t1) <- typeCheck env x
  (_, t2) <- typeCheck env y
  case (t1, t2) of
    (TInt, TInt) -> Right (env, TInt)
    _ -> Left "Unsupported types for addition"

typeCheck env (StartsWith x y) = do
  (_, t1) <- typeCheck env x
  (_, t2) <- typeCheck env y
  case (t1, t2) of
    (TString, TString) -> Right (env, TBool)
    _ -> Left "Both arguments should be of type string"

typeCheck env (And x y) = do
  (_, t1) <- typeCheck env x
  (_, t2) <- typeCheck env y
  case (t1, t2) of
    (TBool, TBool) -> Right (env, TBool)
    (TInt, TInt) -> Right (env, TInt)
    (TString, TString) -> Right (env, TString)
    (TList x, TList y) -> if x == y then Right (env, TList x) else Left "Lists should be of same type"

typeCheck env (Or x y) = do
  (_, t1) <- typeCheck env x
  (_, t2) <- typeCheck env y
  case (t1, t2) of
    (TBool, TBool) -> Right (env, TBool)
    (TInt, TInt) -> Right (env, TInt)
    (TString, TString) -> Right (env, TString)
    (TList x, TList y) -> if x == y then Right (env, TList x) else Left "Lists should be of same type"

typeCheck env (Print x) = typeCheck env x >>= \t -> Right t

typeCheck env (Assign (Var x) a) = do
  (env', t) <- typeCheck env a
  Right ((x, t):env, t)

typeCheck env (Assign (LTuple xs) as) = do
  (env', t) <- typeCheck env as
  case t of
    TTuple a -> if length xs /= length a
                  then Left "Both side of assignment should be of equal length"
                  else do
                    let vs = [v | Var v <- xs]
                    Right (foldl (flip (:)) env (zip vs a), TTuple a)
    _ -> Left "Right operand was expected to be tuple"

-- type of a call is the return type of the called function
typeCheck env (Call f@(Func ti to) args) = do
    (if length ti /= length args then Left "Function called with incorrect number of arguments" else (do
      ts <- mapM (typeCheck env) args
      let ts' = map snd ts
      let vs = [x | Var x <- ti]
      (_, fts) <- typeCheck (zip vs ts') (Func args to)
      case fts of
        TFunc i o -> Right (env, o)
        _ -> Left "Function returned invalid type"))

typeCheck env (Return xs) = do
  (_, t) <- typeCheck env xs
  case t of
    TTuple a -> Right (env, TTuple a)
    _ -> Left "Return should return a tuple"

typeCheck env (Func is os) = do
  trace ("type checking>>>" ++ show (Func is os) ++ show env ++ "\n") (return ())
  -- filter out the return expression
  let fxs = filter (\case
                  Return ys -> True
                  _ -> False
              ) os

  (ti', env') <- typeCheckWithEnv env is
  (to', env'') <- typeCheckWithEnv env' os
  (tr', envFinal) <- typeCheckWithEnv env'' fxs

  if all (== head tr') tr'
    then Right (envFinal, TFunc ti' (head tr'))
    else Left "All returns should be of same type"

typeCheckWithEnv :: TypeEnv -> [Expr] -> Either String ([Type], TypeEnv)
typeCheckWithEnv env = foldM accumulate ([], env)
  where
    accumulate :: ([Type], TypeEnv) -> Expr -> Either String ([Type], TypeEnv)
    accumulate (types, currEnv) expr = do
      (newEnv, t) <- typeCheck currEnv expr
      Right (types ++ [t], newEnv)
