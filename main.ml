type variable = string  
(*langType representa os tipos da linguagem*)
type langType =
  | IntType
  | BoolType
  | FuncType of langType * langType
  | ListType of langType
  | TupleType of langType * langType
  | MaybeType of langType
  | VarType of variable
        (*bin_op representa os operandos binários*)
type bin_op =
  | AddOp
  | SubOp
  | MultOp
  | DivOp
  | LowerThanOp
  | LessEqualOp
  | GreaterThanOp
  | GreaterEqualOp
  | EqualOp
  | AndOp
  | OrOp 
    
type value = 
  | Int of int 
  | Bool of bool 
  | Fun of string * expr * env 
  | Pair of expr * expr 
  | List of expr list 
  | Maybe of value option
        
type env = 
  (variable * result) list
    
(*exp representa as expressões da linguagem*) 
type expr = 
  | IntExpr of int
  | BoolExpr of bool
  | BinOpExpr of bin_op * expr * expr
  | IfExpr of expr * expr * expr
  | VarExpr of variable
  | AppExpr of expr * expr
  | FunExpr of variable * typ * expr
  | LetExpr of variable * typ * expr * expr
  | LetRecExpr of variable * typ * typ * expr * expr
  | TupleExpr of expr * expr
  | FstExpr of expr
  | SndExpr of expr
  | NilExpr of typ
  | ConsExpr of expr * expr
  | MatchExpr of expr * expr * variable * variable * expr
  | JustExpr of expr
  | NothingExpr of typ
  | MatchMaybeExpr of expr * variable * expr * variable * expr 
  