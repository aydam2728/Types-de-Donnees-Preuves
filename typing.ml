
open Lang
  
(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list}

let tp_const = function
| BoolV _ -> BoolT
| IntV -> IntT;;

let tp_var env v = 
    
let tp_expr env = function
| Const c -> tp_const c
| VarE v -> tp_var env v
| CallE es -> g
| BinOp (b, e1, e2) -> match b with | BArith b -> let e1 = tp_expr env e1 in let e2 = tp_expr env e2 in if e1 = IntT && e2 = IntT then IntT else failwith "Probleme de typage" 
                                    | BLogic b -> let e1 = tp_expr env e1 in let e2 = tp_expr env e2 in if e1 = BoolT && e2 = BoolT then BoolT else failwith "Probleme de typage"
                                    | BCompar b -> let e1 = tp_expr env e1 in let e2 = tp_expr env e2 in if e1 = IntT && e2 = IntT then BoolT else failwith "Probleme de typage"
|IfThenElse (e1, e2, e3) -> let e1 = tp_expr env e1 in let e2 = tp_expr env e2 in let e3 = tp_expr env e3 in if e1 = BoolT && e2 = e3 then e2 else failwith "Probleme de typage"
|_ -> failwith "a completer"

    
(* TODO: implement *)
let tp_prog (Prog (fdfs, e)) = IntT
