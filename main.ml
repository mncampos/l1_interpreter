type variable = string  

(*langType representa os tipos da linguagem*)
type langType =
    IntType
  | BoolType
  | FuncType of langType * langType
  | ListType of langType
  | TupleType of langType * langType
  | MaybeType of langType
  | VarType of variable

        (*bin_op representa os operandos binários*)
type bin_op =
    AddOp
  | SubOp
  | MultOp
  | DivOp
  | LessThanOp
  | LessEqualOp
  | GreaterThanOp
  | GreaterEqualOp
  | EqualOp
  | AndOp
  | OrOp 
  
        
type env = 
  (variable * langType) list 
    
(*exp representa as expressões da linguagem*) 
type expr = 
    IntExpr of int
  | BoolExpr of bool
  | BinOpExpr of bin_op * expr * expr
  | IfExpr of expr * expr * expr
  | VarExpr of variable
  | AppExpr of expr * expr
  | FunExpr of variable * langType * expr
  | LetExpr of variable * langType * expr * expr
  | LetRecExpr of variable * langType * langType * expr * expr
  | TupleExpr of expr * expr
  | FstExpr of expr
  | SndExpr of expr
  | NilExpr of langType
  | ConsExpr of expr * expr
  | MatchExpr of expr * expr * variable * variable * expr
  | JustExpr of expr
  | NothingExpr of langType
  | MatchMaybeExpr of expr * variable * expr * variable * expr 

type value = 
    VInt of int 
  | VBool of bool 
  | VFun of string * expr * env 
  | VPair of expr * expr 
  | VList of expr list 
  | VMaybe of value option

        (*exceções*)
exception TypeError of string

    

      (*funçoes auxiliares*)

let rec lookup env variable = 
  match env with
    [] -> None
  | (fstElement, typeElement) :: tl -> 
      if (fstElement=variable) 
      then Some typeElement 
      else lookup tl variable




(*comecar o typeinfer*)

let rec typeInfer (env:env) (e:expr) =
  match e with
  | IntExpr _ -> IntType
  | VarExpr x -> (match lookup env x with 
        Some t -> t
      | None -> raise (TypeError ("Variavel não declarada:" ^ x)))
  | BoolExpr _ -> BoolType 
  | NothingExpr x -> MaybeType x 
  | JustExpr e1 -> MaybeType (typeInfer env e1)
  | NilExpr x -> ListType x
                   

  | TupleExpr (e1,e2) -> 
      let t1 = typeInfer env e1 in
      let t2 = typeInfer env e2 in
      TupleType(t1,t2)  

  | FstExpr e1 ->
      let t1 = typeInfer env e1 in
      (match t1 with
         TupleType(t,_) -> t
       | _ 		   -> raise(TypeError "argumento não é par ordenado"))

  | SndExpr e1 ->
      let t1 = typeInfer env e1 in
      (match t1 with
         TupleType(_,t) -> t
       | _ 		   -> raise(TypeError "argumento não é par ordenado"))

  | IfExpr (e1,e2,e3) ->
      let t1 = typeInfer env e1 in
      (match t1 with
         BoolType ->
           let t2 = typeInfer env e2 in
           let t3 = typeInfer env e3 in
           if t2 = t3 then t2 else raise (TypeError "parâmetros de tipos diferentes")
       | _          -> raise (TypeError "primeiro parâmetro do if não é booleano")) 
      
  | BinOpExpr (op, e1, e2) ->
      let t1 = typeInfer env e1 in
      let t2 = typeInfer env e2 in
      (match op with
         AddOp | SubOp | MultOp | DivOp ->
           (match (t1, t2) with
              (IntType, IntType) -> IntType
            | _ -> raise (TypeError "Operação aritmética aplicada a tipos inválidos"))
       | LessThanOp | LessEqualOp | GreaterThanOp | GreaterEqualOp | EqualOp ->
           (match (t1, t2) with
              (IntType, IntType) | (BoolType, BoolType) -> BoolType
            | _ -> raise (TypeError "Operação de comparação aplicada a tipos inválidos"))
       | AndOp | OrOp ->
           (match (t1, t2) with
              (BoolType, BoolType) -> BoolType
            | _ -> raise (TypeError "Operação lógica aplicada a tipos inválidos")))


  | AppExpr (e1, e2) -> 
      let t1 = typeInfer env e1 in
      let t2 = typeInfer env e2 in
      (match t1 with
       | FuncType (t11, t12) -> 
           if t2 = t11 then t12 
           else raise (TypeError ("Tipo incorreto para argumento da aplicação de função."))
       | _ -> raise (TypeError ("Aplicação de função em expressão que não é função")))











        (*testes*)
let envTeste = [("alvaro", IntType)]