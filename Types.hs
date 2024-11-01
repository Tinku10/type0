module Types (Type(..), Expr(..)) where

data Type 
  = TInt
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
  | Or              Expr Expr
  | Call            [Expr]
  | NoOp
  deriving (Show, Eq)

