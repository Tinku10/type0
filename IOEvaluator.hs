-- This is another evaluator that has IO built in and as such does not require `evalIO`
-- There are a lot of code changes required to make this work and most of the expressions do not need IO
-- So, it seems `evalIO` for the Call handler is a better option
--
-- module Evaluator (eval) where

-- import TypeChecker (TypeEnv, ValueEnv, typeCheck)
-- import Types (Expr(..), Type(..))
-- import Data.List (intersect, union)

-- eval :: TypeEnv -> ValueEnv -> Expr -> IO (Either String (TypeEnv, ValueEnv, Expr))
-- eval tenv venv (LInt x) = return $ Right (tenv, venv, LInt x)
-- eval tenv venv (LBool x) = return $ Right (tenv, venv, LBool x)
-- eval tenv venv (LString x) = return $ Right (tenv, venv, LString x)
-- eval tenv venv (LList x) = return $ Right (tenv, venv, LList x)
-- eval tenv venv (Var x) =
--   case lookup x venv of
--     Just y  -> return $ Right (tenv, venv, y)
--     Nothing -> return $ Left ("Var " ++ x ++ " is unbound")

-- eval tenv venv (LComposite x ys) = do
--   expressions <- mapM (\(_, y) -> eval tenv venv y) ys
--   case sequence expressions of
--     Left e -> return $ Left e
--     Right a -> do
--       let v = map (\(_, _, y) -> y) a
--       let w = map fst ys
--       return $ Right (tenv, venv, LComposite x (zip w v))

-- eval tenv venv (Assign (Var x) a) =
--   case typeCheck tenv a of
--     Left x -> return $ Left x
--     Right y -> do
--       expression <- eval tenv venv a
--       case expression of
--         Left e -> return $ Left e
--         Right (tenv, venv, z) -> return $ Right ((x, y):tenv, (x, z):venv, z)
-- eval tenv venv (Assign _ _) = return $ Left "Assign requires a variable"

-- eval tenv venv (Add x y) = case typeCheck tenv (Add x y) of
--   Left x -> return $ Left x
--   Right TInt -> do
--     evalX <- eval tenv venv x
--     evalY <- eval tenv venv y
--     case (evalX, evalY) of
--       (Right (_, _, a), Right(_, _, b)) -> 
--         case (a, b) of
--           (LInt x, LInt y) -> return $ Right (tenv, venv, LInt (x + y))
--           _ -> return $ Left "Addtion can only be performed on integers"
--       (Left err, _) -> return $ Left err
--       (_, Left err) -> return $ Left err
--   _ -> return $ Left "Addtion can only be performed on integers"

-- eval tenv venv (Get x y) = do
--   evalY <- eval tenv venv y
--   case evalY of
--     Left err -> return $ Left err
--     Right (_, _, z) -> do
--       t <- case typeCheck tenv z of
--         Left e -> return $ Left e
--         Right x -> return $ Right x
--       case t of
--         Left e -> return $ Left e
--         Right a -> do
--           case (a, z) of
--             (TComposite _ _, LComposite c props) -> 
--               case lookup x props of
--                 Just val -> return $ Right (tenv, venv, val)
--                 Nothing  -> return $ Left ("Property " ++ x ++ " not found in " ++ c)
--             _ -> return $ Left "Get can only be applied on composite types"

-- eval tenv venv (Print x) = do
--   (tenv, venv, y) <- eval tenv venv x
--   Right (tenv, venv, x)

-- eval tenv venv NoOp = Right (tenv, venv, NoOp)

-- eval tenv venv (And x y) = do
--   (_, _, x') <- eval tenv venv x
--   (_, _, y') <- eval tenv venv y
--   case typeCheck tenv (And x y) of
--     Right TInt -> case (x', y') of
--       (LInt a, LInt b) -> Right (tenv, venv, LInt a)
--       _ -> Left "Both arguments should be of type int"
--     Right TBool -> case (x', y') of
--       (LBool a, LBool b) -> Right (tenv, venv, LBool (a && b))
--       _ -> Left "Both arguments should be of type bool"
--     Right (TList _) -> case (x', y') of
--       (LList a, LList b) -> Right (tenv, venv, LList (a `intersect` b))
--       _ -> Left "Both arguments should be of type list"
--     Left t -> Left t

-- eval tenv venv (Or x y) = do
--   (_, _, x') <- eval tenv venv x
--   (_, _, y') <- eval tenv venv y
--   case typeCheck tenv (And x y) of
--     Right TInt -> case (x', y') of
--       (LInt a, LInt b) -> Right (tenv, venv, LInt (if a == 0 then b else a))
--       _ -> Left "Both arguments should be of type int"
--     Right TBool -> case (x', y') of
--       (LBool a, LBool b) -> Right (tenv, venv, LBool (a || b))
--       _ -> Left "Both arguments should be of type bool"
--     Right (TList _) -> case (x', y') of
--       (LList a, LList b) -> Right (tenv, venv, LList (a `union` b))
--       _ -> Left "Both arguments should be of type list"
--     Left t -> Left t

-- eval tenv venv (Call []) = Left "Cannot call an empty program"
-- eval tenv venv (Call (x:xs)) = do
--   -- typecheck is not applicable right now
--   -- it would be applicable when functions have types
--   -- maybe we can pass arguments to call and have returns -> Call [Expr] Expr Expr
--   (tenv', venv', r) <- helper tenv venv (x:xs)
--   -- restore the environment
--   Right (tenv', venv', r)

--   where
--     helper :: TypeEnv -> ValueEnv -> [Expr] -> Either String (TypeEnv, ValueEnv, Expr)
--     helper tenv venv [] = Right (tenv, venv, NoOp)
--     helper tenv venv ((Call x):xs) = do
--       let tenv' :: TypeEnv
--           tenv' = []
--           venv' :: ValueEnv
--           venv' = []
--       (tenv'', venv'', _) <- eval tenv' venv' (Call x)
--       helper tenv' venv' xs
--     helper tenv venv (x:xs) = do
--       case eval tenv venv x of
--         Right (tenv', venv', y) -> helper tenv' venv' xs
--         Left x -> Left x

