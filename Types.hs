module Types (Type(..), Expr(..)) where

data Type 
  = TInt
  | TBool
  | TString
  | TTuple          [Type]
  | TList           Type
  | TFunc           [Type] Type
  | TComposite      String [(String, Type)]
  deriving (Show, Eq)

data Expr
  = LInt            Int
  | LBool           Bool
  | LString         String
  | Var             String
  | LComposite      String [(String, Expr)]
  | Get             String Expr
  | Print           Expr
  | LTuple          [Expr]
  | LList           [Expr]
  | Assign          Expr Expr
  | StartsWith      Expr Expr
  | Add             Expr Expr
  | And             Expr Expr
  | Or              Expr Expr
  | Func            [Expr] [Expr]
  | Call            Expr [Expr]
  | Return          Expr
  | NoOp
  deriving (Show, Eq)

