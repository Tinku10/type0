module TypeChecker (typeCheck, TypeEnv, ValueEnv) where

import Types (Type(..), Expr(..))

type TypeEnv = [(String, Type)]
type ValueEnv = [(String, Expr)]

typeCheck :: TypeEnv -> Expr -> Either String Type
typeCheck _ (LInt x) = Right TInt
typeCheck _ (LBool x) = Right TBool
typeCheck _ (LString x) = Right TString
typeCheck env (Var x) = maybe (Left $ "Var " ++ x ++ " not found") Right (lookup x env)

typeCheck env (LComposite x xs) = do
  ts <- mapM (typeCheck env . snd) xs
  let props = map fst xs
  Right (TComposite x (zip props ts))

typeCheck env (LList (x:xs)) = do
  t1 <- typeCheck env x
  t2 <- mapM (typeCheck env) xs
  if all (== t1) t2 then Right (TList t1) else Left "List should be homogeneous"

typeCheck env (Get x y) = do
  t <- typeCheck env y
  case t of
    TComposite c props -> maybe (Left ("Property " ++ x ++ " does not exist in " ++ c)) Right (lookup x props)
    _ -> Left (show y ++ " is not a composite type")

typeCheck env (Add x y) = do
  t1 <- typeCheck env x
  t2 <- typeCheck env y
  case (t1, t2) of
    (TInt, TInt) -> Right TInt
    _ -> Left "Unsupported types for addition"

typeCheck env (StartsWith x y) = do
  t1 <- typeCheck env x
  t2 <- typeCheck env y
  case (t1, t2) of
    (TString, TString) -> Right TBool
    _ -> Left "Both arguments should be of type string"

typeCheck env (And x y) = do
  t1 <- typeCheck env x
  t2 <- typeCheck env y
  case (t1, t2) of
    (TBool, TBool) -> Right TBool
    (TInt, TInt) -> Right TInt
    (TString, TString) -> Right TString
    (TList x, TList y) -> if x == y then Right (TList x) else Left "Lists should be of same type"

typeCheck env (Or x y) = do
  t1 <- typeCheck env x
  t2 <- typeCheck env y
  case (t1, t2) of
    (TBool, TBool) -> Right TBool
    (TInt, TInt) -> Right TInt
    (TString, TString) -> Right TString
    (TList x, TList y) -> if x == y then Right (TList x) else Left "Lists should be of same type"

typeCheck env (Print x) = typeCheck env x >>= \t -> Right t

-- type of a function is the type of the last expression
typeCheck env (Call []) = Left "Programs cannot be blank"
typeCheck env (Call [x]) = typeCheck env x
typeCheck env (Call xs) = do
  ts <- mapM (typeCheck env) xs
  Right (last ts)
  
