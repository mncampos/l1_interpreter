(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

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
  | DivOp (*acho q não precisa de divisao*)
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
  | LetRecExpr of variable * langType * expr * expr
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
  | VFun of string * value * env  (*ver se existe o value de funcao*)
  | VTuple of value * value 
  | VList of value list 
  | VMaybe of value option

        (*exceções*)
exception TypeError of string

exception CannotHappen

      (*funçoes auxiliares*)

let rec lookup env variable = 
  match env with
    [] -> None
  | (fstElement, typeElement) :: tl -> 
      if (fstElement=variable) 
      then Some typeElement 
      else lookup tl variable


let  update amb valor tipo = (valor,tipo) :: amb 
                    

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

  | FunExpr (e1, e2, e3) -> IntType (*Arrumar aqui*)
  | MatchExpr (e1, e2, v1, v2, e3) -> IntType (*Arrumar Aqui*)
  | MatchMaybeExpr (e1, e2, v1, v2, e3) ->IntType (*Arrumar aqui*)
  | ConsExpr (e1, e2) -> IntType (*Arrumar aqui*)
                                 
  | LetExpr (v1, t1, e1, e2) -> 
      if (typeInfer env e1) = t1 then typeInfer (update env v1 t1) e2 
      else raise (TypeError "Expressão não é do tipo declarado")    
          
  | LetRecExpr (f,(FuncType (t1,t2) as tf), FunExpr(x,tx,e1), e2) -> 
      let env_com_tf = update env f tf in 
      let env_com_tf_tx = update env_com_tf x tx in
      if (typeInfer env_com_tf_tx e1) = t2 then typeInfer env_com_tf e2
      else raise (TypeError "Tipo da funcao diferente do declarado") (*Ver melhor se tá certo*)
                                    
  | LetRecExpr _ -> raise CannotHappen 
                                    
                                    
          (*Ending typeInfer*)


                      (*Auxiliary types ver se precisa usar a memoria*)
type endereco = int
  
type memoria = (endereco * value) list
    

type resultado = (value * memoria)  

  

    (*Auxiliary Functions*)
let compute (operando: bin_op) (v1: value) (v2: value) : value =
  match (operando, v1, v2) with
  
    (* operadores aritméticos com números *)
    (AddOp,  VInt n1, VInt n2) -> VInt (n1 + n2)
  | (SubOp,  VInt n1, VInt n2) -> VInt (n1 - n2)
  | (MultOp, VInt n1, VInt n2) -> VInt (n1 * n2) 
                                                
    (* operadores relacionais com números *)
  | (EqualOp, VInt n1, VInt n2) -> VBool( n1 = n2) 
  | (GreaterThanOp, VInt n1, VInt n2) -> VBool( n1 > n2)
  | (LessThanOp, VInt n1, VInt n2) -> VBool( n1 < n2)
  | (GreaterEqualOp, VInt n1, VInt n2) -> VBool( n1 >= n2)
  | (LessEqualOp, VInt n1, VInt n2) -> VBool( n1 <= n2)
  | (AndOp, VBool b1, VBool b2) -> VBool( b1 && b2)
  | (OrOp, VBool b1, VBool b2) -> VBool( b1 || b2)
                                
  | _ -> raise (TypeError "Tipo Incompatível")

           
           (*Testar direito isso *)

          (*starting Evaluation*)                                           
                                                                     
let rec eval (env:env) (e:expr) : (value) =
  match e with
    IntExpr n -> (VInt n)
                 
  | BoolExpr b -> (VBool b)
                  
  | IfExpr (e1,e2,e3) ->
      (match eval env e1 with
         (VBool true) -> eval env e2
       | (VBool false) -> eval env e3
       | _ -> raise CannotHappen) 
      
        (* | VarExpr x ->
          (match lookup env x with
             Some v -> v
           | None -> (TypeError "Tipo Incompatível"))
*)
  | TupleExpr (e1,e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      VTuple(v1,v2)
        
  | FstExpr e ->
      (match eval env e with
       | (VTuple(v1,_)) -> v1
       | _ -> raise (TypeError "Tipo Incompatível"))
      
  | SndExpr e ->
      (match eval env e with
       | (VTuple(_,v2)) -> v2
       | _ -> raise (TypeError "Tipo Incompatível"))
                                   
                                    









        (*testes*)
let envTeste = [("alvaro", IntType)]