open Lang

(* ************************************************************ *)
(* ****  Operational Semantics                             **** *)
(* ************************************************************ *)

(* ****  Auxiliary functions                               **** *)


(* ****  Result of evaluation                              **** *)

type result =
    Val of value
  | Closure of fpdefn * (result list)

type state = (vname * result) list

      
(* TODO: implement *)
let eval_prog (Prog (fdfs, e)) = Val (IntV 42)
;;

let is_tail_expr = function
  | _ -> true

let transf_expr f p expr = 
  if is_tail_expr expr then
    expr
  else
    None
    
