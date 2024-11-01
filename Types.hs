module Types (Type(..), Expr(..)) where

data Type = TInt
          | TBool
          | TString
          | TList Type
          | TComposite String [(String, Type)]
          deriving (Show, Eq)

data Expr
  = LInt            Int
  | LBool           Bool
  | LString         String
  | Var             String
  | LComposite      String [(String, Expr)]
  | Get             String Expr
  | Print           Expr
  | LList           [Expr]
  | Assign          Expr Expr
  | StartsWith      Expr Expr
  | Add             Expr Expr
  | And             Expr Expr
  | NoOp
  deriving (Show, Eq)

-- data Type a where
--   TPrimitive :: PType -> Type PType
--   TList      :: Type a -> Type [a]
--   TComposite :: String -> [(String, PType)] -> Type [(String, PType)]
-- data TypedExpr a where
--   TypedLInt           :: Int -> TypedExpr Int
--   TypedTLBool         :: Bool -> TypedExpr Bool
--   TypedLString        :: String -> TypedExpr String
--   TypedLComposite     :: String -> [(String, TypedExpr a)] -> TypedExpr [(String, TypedExpr a)]
--   TypedFStartsWith    :: TypedExpr String -> TypedExpr String -> TypedExpr Bool
--   TypedFAdd           :: TypedExpr Int -> TypedExpr Int -> TypedExpr Int
--   TypedFAnd           :: TypedExpr [a] -> TypedExpr [a] -> TypedExpr [a]
