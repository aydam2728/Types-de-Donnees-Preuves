
open Lang
  
(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list}

let tp_const  = function
    | BoolV -> BoolT
    | IntV -> IntT
    | _ -> failwith "tp_const : internal error"

let tp_var environment = function
    | Var v -> check environment.localvar v
    | Fun f -> check environment.funbind f
    | _ -> failwith "tp_var : internal error"

    
let rec check vn = function
        | [] -> failwith "tp_var : internal error"
        | (vn, fp) :: _ when vn = v -> fp
        | _ :: vs -> check vn vs

let rec tp_application ft argst =
    
let tp_fdefn environment (Fundefn (f, fps, e)) = 
    let environment' = {localvar = fps; funbind = environment.funbind} in
    let et = tp_expr environment' e in
    if et = ft then ft else failwith "tp_fdefn : internal error"
    

let tp_prog (Prog (fdfs, e)) = IntT

let rec tp_expr environment  = function
    | Const expr -> tp_const expr
    | VarE expr -> tp_var expr
    | CallE (f, args) -> 
        let ft = tp_expr environment f in 
        let argst = List.map (tp_expr environment) args in
        tp_application ft argst
    | CallE (_) -> failwith "tp_expr : internal error"


let tp_prog (Prog (fdfs, e)) = IntT
