{-# LANGUAGE RankNTypes #-}

module Eval (eval) where

import qualified Data.Map as Map
import Syntax (Env, Expr (..), Value (..))

eval :: Env -> Expr -> (Value, Env)
eval env (ENum n) = (VNum n, env)
eval env (EBool b) = (VBool b, env)
eval env (EAdd e1 e2) = (binOp env (+) e1 e2, env)
eval env (ESub e1 e2) = (binOp env (-) e1 e2, env)
eval env (EMul e1 e2) = (binOp env (*) e1 e2, env)
eval env (EDiv e1 e2) = (binOp env (/) e1 e2, env)
eval env (EEq e1 e2) = (cmpOp env (==) e1 e2, env)
eval env (ENe e1 e2) = (cmpOp env (/=) e1 e2, env)
eval env (ELt e1 e2) = (cmpOp env (<) e1 e2, env)
eval env (ELe e1 e2) = (cmpOp env (<=) e1 e2, env)
eval env (EGt e1 e2) = (cmpOp env (>) e1 e2, env)
eval env (EGe e1 e2) = (cmpOp env (>=) e1 e2, env)
eval env (EIf cond bThen bElse) =
  let (bool, _) = eval env cond
   in case bool of
        VBool True -> eval env bThen
        VBool False -> eval env bElse
        _ -> error "Error"
eval env (EVar name) =
  case Map.lookup name env of
    Just val -> (val, env)
    Nothing -> error $ "Undefined: " ++ name
eval env (EFun funcName params body) =
  let closure = VFun params body env
   in (closure, Map.insert funcName closure env)
eval env (ECal funcExpr argExpr) =
  let (funcVal, _) = eval env funcExpr
      (argVal, _) = eval env argExpr
   in case funcVal of
        VFun params body fEnv ->
          case params of
            [] -> eval fEnv body
            (param : remainingParams) ->
              let newEnv = Map.insert param argVal fEnv
               in if null remainingParams
                    then eval newEnv body
                    else (VFun remainingParams body newEnv, newEnv)
        _ -> error $ "Expected a function, but got " ++ show funcVal

binOp :: Env -> (Float -> Float -> Float) -> Expr -> Expr -> Value
binOp env op e1 e2 =
  let (v1, _) = eval env e1
      (v2, _) = eval env e2
   in case (v1, v2) of
        (VNum n1, VNum n2) -> VNum (op n1 n2)
        _ -> error "Error"

cmpOp :: Env -> (forall a. (Ord a) => a -> a -> Bool) -> Expr -> Expr -> Value
cmpOp env op e1 e2 =
  let (v1, _) = eval env e1
      (v2, _) = eval env e2
   in case (v1, v2) of
        (VNum l, VNum r) -> VBool (op l r)
        (VBool l, VBool r) -> VBool (op l r)
        _ -> error "Error"