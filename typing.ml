
open Lang
  
(* Environments *)
type prog = Prog of (fpdefn list) * expr

type fpdefn = 
    | Fundefn of fpdecl * expr
    | Procdefn of fpdecl * cmd

type fpdecl = FPdecl of tp * vname * ( vardecl list)

type vardecl = Vardecl of vname * tp


type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list}

let tp_const  = function
    | BoolV _ -> BoolT
    | IntV _ -> IntT
    | _ -> failwith "tp_const : internal error"

let rec check vn env =
    match env with
    | [] -> failwith ("Variable or function not found: " ^ vn)
    | (name, tp) :: rest -> if name = vn then tp else check vn rest
    
let tp_var env vname =
    try check vname env.localvar
    with Failure _ -> try 
        match check vname env.funbind with
        | FPdecl (return_type, _, _) -> return_type
        with Failure _ -> failwith ("Variable or function not found: " ^ vname)
      

let tp_application env fname args =
    try 
        match check fname env.funbind with
        | FPdecl (return_type, _, arg_types) -> 
            if List.length args <> List.length arg_types then
                failwith "tp_application : wrong number of arguments"
            else
                let rec check_args arg_types args =
                    match arg_types, args with
                    | [], [] -> return_type
                    | tp :: arg_types', arg :: args' -> 
                        if tp = arg then check_args arg_types' args'
                        else failwith "tp_application : wrong type of arguments"
                    | _, _ -> failwith "tp_application : internal error"
                in
                check_args arg_types args
    with Failure _ -> failwith ("Function not found: " ^ fname)
          
  (* 
    let tp_fdefn environment = function
    | Fundefn (FPdecl (return_type, fname, arg_types), body) -> 
        let env' = {localvar = List.map (function Vardecl (vn, tp) -> (vn, tp)) arg_types; funbind = environment.funbind} in
        let body_type = tp_expr env' body in
        if body_type = return_type then ()
        else failwith "tp_fdefn : wrong return type"
    | Procdefn (FPdecl (_, fname, arg_types), body) ->
        let env' = {localvar = List.map (function Vardecl (vn, tp) -> (vn, tp)) arg_types; funbind = environment.funbind} in
        tp_cmd env' body

*)     

let rec tp_expr environment  = function
    | Const expr -> tp_const expr
    | VarE expr -> tp_var environment expr
    | CallE (a::b) -> let tf = tp_expr environment a in let tl = tp_expr environment (CallE b) in if tf = tl then tf else failwith "Probleme de typage"
    | CallE (_) -> failwith "tp_expr : internal error"
    | BinOp (b, e1, e2) -> (match b with | BArith b -> let e1 = tp_expr environment e1 in let e2 = tp_expr environment e2 in if e1 = IntT && e2 = IntT then IntT else failwith "Probleme de typage" 
                                    | BLogic b -> let e1 = tp_expr environment e1 in let e2 = tp_expr environment e2 in if e1 = BoolT && e2 = BoolT then BoolT else failwith "Probleme de typage"
                                    | BCompar b -> let e1 = tp_expr environment e1 in let e2 = tp_expr environment e2 in if e1 = IntT && e2 = IntT then BoolT else failwith "Probleme de typage")    
    | IfThenElse (e1, e2, e3) -> if (tp_expr environment e1) = BoolT then let e2 = tp_expr environment e2 in let e3 = tp_expr environment e3 in if e2 = e3 then e2 else failwith "Probleme de typage" else failwith "Probleme de typage"

let tp_prog (Prog (fdfs, e)) = IntT
