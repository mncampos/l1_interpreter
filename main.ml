
type variable = string  

type langType =
    IntType
  | BoolType
  | FuncType of langType * langType
  | PairType of langType * langType
  | ListType of langType 
  | VarType of variable 
  | MaybeType of langType
      
type bin_op = AddOp|SubOp|MultOp|DivOp|LessThanOp|LessEqualOp|GreaterThanOp|GreaterEqualOp|EqualOp|AndOp|OrOp 
  
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
  | PairExpr of expr * expr
  | FstExpr of expr
  | SndExpr of expr
  | NilExpr of langType
  | ConsExpr of expr * expr
  | HeadExpr of expr
  | TailExpr of expr
  | MatchExpr of expr * expr * variable * variable * expr
  | JustExpr of expr
  | NothingExpr of langType
  | MatchMaybeExpr of expr * variable * expr * variable * expr 

type value = 
    VInt of int 
  | VBool of bool 
  | VPair of value * value 
  | VList of value list 
  | VMaybe of value option 
  | VClosure of variable * expr * envV
  | VRecClosure of variable * variable * expr * env
and
  env = (variable * langType) list
and
  envV = (variable * value) list 
;;

exception TypeError of string 
exception CannotHappen 
exception NotFound

      (*funçoes auxiliares*)

    (*Este lookup serve para retornar o TIPO*)

let rec lookup env variable : langType = match env with
    [] -> raise NotFound
  | (name, v)::tl ->
      if (name == variable)      
      then v                     
      else lookup tl variable;;

          (*Já este retorna o VALOR*) 
let rec lookupV env variable : value = match env with 
    [] -> raise NotFound
  | (firstElement, v)::tl ->
      if (firstElement = variable)      
      then v                     
      else lookupV tl variable 
          

let  update env valor tipo = (valor,tipo) :: env
                             
let updateV variable v environment : envV = match environment with
  | [] -> [(variable, v)]
  | hd::tl -> List.append [(variable, v)] environment ;;
                             
  
let rec typeInfer (env:env) (e:expr) =
  match e with
  | IntExpr _ -> IntType
  | VarExpr (v) -> lookup env v
  | BoolExpr _ -> BoolType 
  | NothingExpr x -> MaybeType x 
  | JustExpr e1 -> MaybeType (typeInfer env e1)
  | NilExpr x -> ListType x
                   

  | PairExpr (e1,e2) -> 
      let t1 = typeInfer env e1 in
      let t2 = typeInfer env e2 in
      PairType(t1,t2)  

  | FstExpr e1 ->
      let t1 = typeInfer env e1 in
      (match t1 with
         PairType(t,_) -> t
       | _ 		   -> raise(TypeError "argumento não é par ordenado"))

  | SndExpr e1 ->
      let t1 = typeInfer env e1 in
      (match t1 with
         PairType(_,t) -> t
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

  | FunExpr(v, t, e) -> FuncType(t, (typeInfer(update env v t) e))
<<<<<<< HEAD
                          
                          
  | MatchExpr (e1, e2, x1, x2, e3) ->
      let t1 = typeInfer env e1 in
      let t2 = typeInfer env e2 in
      let env' = (x2, ListType t1) :: (x1, t1) :: env in
      let t3 = typeInfer env' e3 in
      if t2 = t3 then t2
      else raise (TypeError "Erro de tipagem na expressão de match")
          
          
  | MatchMaybeExpr (e1, x, e2, y, e3) ->
      let t1 = typeInfer env e1 in
      let t2 = typeInfer env e2 in
      let env' = (x, t1) :: env in
      let env'' = (y, t1) :: env' in
      let t3 = typeInfer env'' e3 in
      (match t2, t3 with
       | VarType v, t' when v = y && t1 = t' -> t2
       | _, _ when t2 = t3 -> t2
       | _, _ -> raise (TypeError "Erro de tipagem na expressão de match"))
                                                  
                                                  
=======
  | MatchExpr (e1, e2, v1, v2, e3) -> IntType (*Arrumar Aqui*)
  | MatchMaybeExpr (e1, e2, v1, v2, e3) ->IntType (*Arrumar aqui*)
>>>>>>> 03c11de42d20ae2acfd827f5ed94d2c530c8c8b3
  | ConsExpr (e1, e2) ->
      let t1 = typeInfer env e1 in
      let t2 = typeInfer env e2 in
      if t2 = ListType t1 then t2
      else raise(TypeError("Erro de tipagem na construção da lista"))
<<<<<<< HEAD
          
          
      
=======
>>>>>>> 03c11de42d20ae2acfd827f5ed94d2c530c8c8b3
                                 
  | LetExpr (v1, t1, e1, e2) -> 
      if (typeInfer env e1) = t1 then typeInfer (update env v1 t1) e2 
      else raise (TypeError "Expressão não é do tipo declarado")    
          
  | LetRecExpr (var, paramType, returnType, body, expr) ->
      let funEnv = (var, FuncType(paramType, returnType)) :: env in
      let bodyType =
<<<<<<< HEAD
        typeInfer funEnv body in
=======
        typeInfer ((var, FuncType(paramType, returnType)) :: env) body in
>>>>>>> 03c11de42d20ae2acfd827f5ed94d2c530c8c8b3
      if bodyType = returnType then
        typeInfer funEnv expr
      else
        raise (TypeError "Erro de tipagem no letRec")
<<<<<<< HEAD
          
  | HeadExpr e ->
      (match typeInfer env e with
       | ListType t -> t
       | _ -> raise (TypeError "Head de um tipo que não é lista"))
      
  | TailExpr e ->
      (match typeInfer env e with
       | ListType t -> ListType t
       | _ -> raise (TypeError "Cauda de um tipo que não é lista"))
      
      
      
=======
>>>>>>> 03c11de42d20ae2acfd827f5ed94d2c530c8c8b3
                                    
                                                                     
let rec eval (env:envV) (e:expr) : (value) =
  match e with
    IntExpr n -> (VInt n)
                 
  | BoolExpr b -> (VBool b)
                  
  | VarExpr v -> (lookupV env v)
                  
  | IfExpr (e1,e2,e3) ->
      (match eval env e1 with
         (VBool true) -> eval env e2
       | (VBool false) -> eval env e3
       | _ -> raise CannotHappen) 
  
  | BinOpExpr (op, e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match (op, v1, v2) with
       | (AddOp, VInt n1, VInt n2) -> VInt (n1 + n2)
       | (SubOp, VInt n1, VInt n2) -> VInt (n1 - n2)
       | (MultOp, VInt n1, VInt n2) -> VInt (n1 * n2)
       | (LessThanOp, VInt n1, VInt n2) -> VBool (n1 < n2)
       | (LessEqualOp, VInt n1, VInt n2) -> VBool (n1 <= n2)
       | (GreaterThanOp, VInt n1, VInt n2) -> VBool (n1 > n2)
       | (GreaterEqualOp, VInt n1, VInt n2) -> VBool (n1 >= n2)
       | (EqualOp, VInt n1, VInt n2) -> VBool (n1 = n2)
       | (AndOp, VBool b1, VBool b2) -> VBool (b1 && b2)
       | (OrOp, VBool b1, VBool b2) -> VBool (b1 || b2)
       | _ -> failwith "Invalid operands for binary operator")

  | PairExpr (e1,e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      VPair(v1,v2)
        
  | FstExpr e ->
      (match eval env e with
       | (VPair(v1,_)) -> v1
       | _ -> raise (TypeError "Tipo Incompatível"))
      
  | SndExpr e ->
      (match eval env e with
       | (VPair(_,v2)) -> v2
       | _ -> raise (TypeError "Tipo Incompatível"))
  
  | FunExpr (x, typ, expr) -> VClosure (x, expr, env)
  
  | LetExpr (x, t, e1, e2) ->
      let v = eval env e1 in
      let env' = (x, v) :: env in
      eval env e2
        






        (*testes*)
let envTeste = [("alvaro", IntType)]
  