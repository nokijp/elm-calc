module Expression exposing
  ( Expression(..)
  , Function(..)
  , showExpression
  )

type Expression
  = Number Float
  | Negate Expression
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  | Mod Expression Expression
  | Apply Function Expression

type Function
  = Exp
  | Log
  | Sin
  | Cos
  | Tan

showExpression : Expression -> String
showExpression expression =
  let
    showBinaryOp op e1 e2 = "(" ++ showExpression e1 ++ ") " ++ op ++ " (" ++ showExpression e2 ++ ")"
  in case expression of
    Number x -> String.fromFloat x
    Negate e -> "-(" ++ showExpression e ++ ")"
    Add e1 e2 -> showBinaryOp "+" e1 e2
    Sub e1 e2 -> showBinaryOp "-" e1 e2
    Mul e1 e2 -> showBinaryOp "*" e1 e2
    Div e1 e2 -> showBinaryOp "/" e1 e2
    Mod e1 e2 -> showBinaryOp "%" e1 e2
    Apply f e -> functionName f ++ "(" ++ showExpression e ++ ")"

functionName : Function -> String
functionName f =
  case f of
    Exp -> "exp"
    Log -> "log"
    Sin -> "sin"
    Cos -> "cos"
    Tan -> "tan"
