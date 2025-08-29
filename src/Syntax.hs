module Syntax
  ( Expr (..),
    Value (..),
    Env,
  )
where

import Data.Map (Map)

data Expr
  = ENum Float
  | EBool Bool
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EEq Expr Expr
  | ENe Expr Expr
  | ELt Expr Expr
  | ELe Expr Expr
  | EGt Expr Expr
  | EGe Expr Expr
  | EVar String
  | EFun String [String] Expr
  | ECal Expr Expr
  | EIf Expr Expr Expr
  deriving (Show)

data Value
  = VNum Float
  | VBool Bool
  | VFun [String] Expr Env
  deriving (Show)

instance Eq Value where
  (VNum n1) == (VNum n2) = n1 == n2
  (VBool b1) == (VBool b2) = b1 == b2
  _ == _ = False

instance Ord Value where
  (VNum n1) `compare` (VNum n2) = n1 `compare` n2
  (VBool b1) `compare` (VBool b2) = b1 `compare` b2
  (VNum _) `compare` (VBool _) = GT
  (VBool _) `compare` (VNum _) = LT
  _ `compare` _ = EQ

type Env = Map String Value