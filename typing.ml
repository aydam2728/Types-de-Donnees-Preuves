
open Lang
  
(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list}

let tp_const  = function
    | BoolV -> BoolT
    | IntV -> IntT
    | _ -> failwith "tp_const : internal error"

let tp_var env vname =
  try check vname env.localvar
  with Failure _ -> try 
    match check vname env.funbind with
    | FPdecl (return_type, _, _) -> return_type
    with Failure _ -> failwith ("Variable or function not found: " ^ vname)

    
let rec check vn = function
        | [] -> failwith "tp_var : internal error"
        | (vn, fp) :: _ when vn = v -> fp
        | _ :: vs -> check vn vs

let tp_application env fname args =
  match List.assoc_opt fname env.funbind with
  | Some (FPdecl (return_type, _, param_types)) ->
    if List.length args = List.length param_types then
      let arg_types = List.map (fun (Vardecl (_, t)) -> t) param_types in
      List.iter2 (fun expected arg ->
        let arg_type = tp_expr env arg in
        if expected <> arg_type then
          failwith "Type mismatch in function arguments"
      ) arg_types args;
      return_type
    else failwith "Incorrect number of arguments in function call"
  | None -> failwith ("Function not found: " ^ fname)

    
let tp_fdefn environment (Fundefn (f, fps, e)) = 
    let environment' = {localvar = fps; funbind = environment.funbind} in
    let et = tp_expr environment' e in
    if et = ft then ft else failwith "tp_fdefn : internal error"
    

let rec tp_expr env = function
  | Const c -> tp_const c
  | VarE v -> tp_var env v
  | CallE (f, args) ->
    let fname = match f with
      | Var v -> v
      | _ -> failwith "Function name expected"
    in
    let arg_types = List.map (tp_expr env) args in
    tp_application env fname arg_types
  | _ -> failwith "Unsupported expression type"

let tp_prog (Prog (fdefs, e)) =
  let global_env = { localvar = []; funbind = fdefs } in
  let _ = List.map (tp_fdefn global_env) fdefs in
  tp_expr global_env e

