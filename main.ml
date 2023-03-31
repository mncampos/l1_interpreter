
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
  | LetRecExpr of variable * langType * langType * variable * expr * expr
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
  | VCons of value * value 
  | VMaybe of value option 
  | VClosure of variable * expr * envV
  | VRecClosure of variable * variable * expr * envV
  | VNil
    
and
  env = (variable * langType) list
and
  envV = (variable * value) list 
;;

exception TypeError of string 
exception CannotHappen 
exception NotFound
exception EvalError of string

      (*funçoes auxiliares*)

    (*Este lookup serve para retornar o TIPO*)

let rec lookup env variable : langType = match env with
    [] -> raise NotFound
  | (name, v)::tl ->
      if (name = variable)      
      then v                     
      else lookup tl variable;;

          (*Já este retorna o VALOR*) 
let rec lookupV env variable : value = match env with 
    [] -> raise NotFound
  | (firstElement, v)::tl ->
      if (firstElement = variable)      
      then v                     
      else lookupV tl variable 
  
          
          (* Update no env de tipos *)
let rec update_env (env: env) (var: variable) (typ: langType) : env =
  match env with
  | [] -> [(var, typ)]
  | (v, _)::tl when v = var -> (var, typ)::tl
  | hd::tl -> hd :: update_env tl var typ
                             
                             
                (*Já este é no de valores *)
let rec update_envV (envV: envV) (var: variable) (value: value) : envV =
  match envV with
  | [] -> [(var, value)]
  | (v, _)::tl when v = var -> (var, value)::tl
  | hd::tl -> hd :: update_envV tl var value
                             
  
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

  | FunExpr(v, t, e) -> FuncType(t, (typeInfer(update_env env v t) e))
                          
                          
  | MatchExpr (e, e1, x, xs, e2) ->
      let t1 = typeInfer env e1 in
      let env' = (xs, ListType t1) :: (x, t1) :: env in
      let t2 = typeInfer env' e2 in
      if t2 = typeInfer env e1 then t2
      else raise (TypeError ("Erro no match"))
          
          
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
                                                  
                                                  
  | ConsExpr (e1, e2) ->
      let t1 = typeInfer env e1 in
      let t2 = typeInfer env e2 in
      if t2 = ListType t1 then t2
      else raise(TypeError("Erro de tipagem na construção da lista"))
          
          
      
                                 
  | LetExpr (v1, t1, e1, e2) -> 
      if (typeInfer env e1) = t1 then typeInfer (update_env env v1 t1) e2 
      else raise (TypeError "Expressão não é do tipo declarado")    
          
  | LetRecExpr (f, t1, t2, x, e1, e2) -> 
      let env' = update_env env f t1 in
      let env'' = update_env env' x t2 in
      if typeInfer env'' e1 = t2 
      then typeInfer env' e2
      else raise (TypeError "erro de tipagem no LetRec")
        
        
          
  | HeadExpr e ->
      (match typeInfer env e with
       | ListType t -> t
       | _ -> raise (TypeError "Head de um tipo que não é lista"))
      
  | TailExpr e ->
      (match typeInfer env e with
       | ListType t -> ListType t
       | _ -> raise (TypeError "Cauda de um tipo que não é lista"))
      
      
      
                                    
                                                                     
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
      
  | NilExpr (v) -> VNil
    
  | NothingExpr(t) -> VMaybe(None)
    
  | JustExpr(e) ->
      let v = eval env e in
      (match v with 
       | _ -> v)
  
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
       | _ -> raise(TypeError "Operandos inválidos"))

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
      eval env' e2 
        
  | AppExpr(e1, e2) ->
      let exp1 = eval env e1 in
      let exp2 = eval env e2 in
      (match exp1, exp2 with
         VClosure(variable, e, env'), value -> eval (update_envV env' variable value) e
       | VRecClosure(f, x, e, env'), value -> eval (update_envV (update_envV env' x value) f (VRecClosure(f, x, e, env')) ) e
       | _ -> raise (EvalError "Erro de avaliação"))

  | ConsExpr(e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      VCons(v1, v2)
  | HeadExpr(e) ->
      let v = eval env e in
      (match v with
       | VCons(v1, _) -> v1
       | _ -> raise (EvalError "Erro de avaliação de lista"))
  | TailExpr(e) ->
      let v = eval env e in
      (match v with
       | VCons(_, v2) -> v2
       | _ -> raise (EvalError "Erro de avaliação de lista"))
      
  | MatchMaybeExpr (e1,x,e2,y,e3) -> (*ver se precisa do x mesmo*)
      (let v1 = eval env e1 in
       match v1 with
       | VNothing -> eval env e2
       | VJust y -> eval env e3)

      
  | _ -> raise (EvalError "Erro de avaliação")



        (*testes typeInfer*)
let envTest = [("x", IntType)] ;;
let env2Test = [("f", (FuncType (IntType, IntType)))];; 

               
let intExprTest = IntExpr(1);;
let trueTest = BoolExpr(true);;
let falseTest = BoolExpr(false);;
let binOpExprTest = BinOpExpr (AddOp, IntExpr 3, IntExpr 4);;  
let ifExprTest = IfExpr (BoolExpr true, IntExpr 1, IntExpr 2) (* langType: IntType *)
let varExprTest = VarExpr "x" (* langType: VarType "x" *)
let appExprTest = AppExpr (VarExpr "f", IntExpr 42) (* langType: VarType "f" *)
let funExprTest = FunExpr ("x", IntType, BinOpExpr (AddOp, VarExpr "x", IntExpr 1)) (* langType: FuncType (IntType, IntType) *)
let letExprTest = LetExpr ("x", IntType, IntExpr 42, VarExpr "x") (* langType: IntType *) 
let letRecExprTest = LetRecExpr ("f", FuncType (IntType, IntType), IntType, "x", BinOpExpr (AddOp, VarExpr "x", IntExpr 1), AppExpr (VarExpr "f", IntExpr 2)) (* langType: intType  *)
let pairExprTest = PairExpr (IntExpr 1, BoolExpr true) (* langType: PairType (IntType, BoolType) *)
let fstExprTest = FstExpr pairExprTest (* langType: IntType *)
let sndExprTest = SndExpr pairExprTest (* langType: BoolType *)
let nilExprTest = NilExpr (IntType) (* langType: ListType IntType *)
let consExprTest = ConsExpr (IntExpr 1, ConsExpr (IntExpr 2, NilExpr IntType)) (* langType: ListType IntType *)
let headExprTest = HeadExpr consExprTest (* langType: IntType *)
let tailExprTest = TailExpr consExprTest (* langType: ListType IntType *)
let matchExprTest = MatchExpr (consExprTest, IntExpr 0, "x", "y", BinOpExpr (AddOp, IntExpr 1, IntExpr 2)) (* langType: IntType *)
let justExprTest = JustExpr (IntExpr 1) (* langType: MaybeType IntType *)
let nothingExprTest = NothingExpr IntType (* langType: MaybeType IntType *)
let matchMaybeExprTest = MatchMaybeExpr (justExprTest, "x", BinOpExpr (AddOp, VarExpr "x", IntExpr 1), "y", VarExpr "y") (* langType: IntType *)
  
  
  
  
    (*testes eval*)

let env_test : envV = [
  ("f", VClosure("x", FunExpr("y", IntType, BinOpExpr(AddOp, VarExpr("x"), VarExpr("y"))), []));
  ("true", VBool(true));
  ("false", VBool(false))
]
  
let appExprTestEval = AppExpr (appExprTest, binOpExprTest) (*Valor esperado com env_test = VInt 49 *)
  
  
  
  
  
  
  
  