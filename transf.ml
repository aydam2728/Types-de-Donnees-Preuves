  (* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String)

(* TODO: implement *)

let rec names_expr = function
    Const(_) -> StringSet.empty
  | VarE(x) -> StringSet.singleton x
  | BinOp(_, e1, e2) -> StringSet.union (names_expr e1) (names_expr e2)
  | IfThenElse(e1, e2, e3) -> StringSet.union (names_expr e1) (StringSet.union (names_expr e2) (names_expr e3))
  | CallE ( a :: b ) -> StringSet.union (names_expr a) (names_expr b)

(* TODO: implement *)
let transf_prog (Prog(fdfs, e)) = Prog(fdfs, e)

let rec is_tail_expr fname e = match e with
  | VarE v ->  v != fname
  | BinOp (_, e1, e2) -> is_tail_expr fname e1 && is_tail_expr fname e2
  | IfThenElse (e1, e2, e3) -> is_tail_expr fname e1 && is_tail_expr fname e2 && is_tail_expr fname e3
  | CallE (f, es) -> f != fname && List.for_all (is_tail_expr fname) es
  | _ -> true
  
let rec transf_expr f p expr =
  match expr with
  | Const n -> Some (PyConst n)
  | VarE x -> Some (PyVar x)
  | BinOp (op, e1, e2) -> 
    (match transf_expr f p e1, transf_expr f p e2 with
    | Some e1', Some e2' -> Some (PyBinOp (op, e1', e2'))
    | _ -> None)
  | IfThenElse (e1, e2, e3) -> 
    (match transf_expr f p e1, transf_expr f p e2, transf_expr f p e3 with
    | Some e1', Some e2', Some e3' -> Some (PyIfThenElse (e1', e2', e3'))
    | _ -> None)
  | CallE (g, es) -> 
    (match List.map (transf_expr f p) es with
    | es' when List.for_all Option.is_some es' -> Some (PyCallE (g, List.map Option.get es'))
    | _ -> None)

let rec transf_fpdefn f p (FPDefn (fname, args, e)) =
  let tail = is_tail_expr fname e in
  let e' = transf_expr f p e in
  match e' with
  | Some e'' -> Some (PyFPDefn (fname, args, e'', tail))
  | _ -> None

let rec transf_prog (Prog (fdfs, e)) =
  let fdfs' = List.map (transf_fpdefn fdfs) fdfs in
  match List.map Option.is_some fdfs' with
  | bs when List.for_all (fun b -> b) bs -> Some (Prog (List.map Option.get fdfs', e))
  | _ -> None


let rec fac (n : int) : int =
  if n = 0 then 1 else n * fac (n - 1)


let rec fib (n : int) : int =
  if n = 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)

(* translate to python*)

let rec string_of_py_expr = function
  | PyConst n -> string_of_int n
  | PyVar x -> x
  | PyBinOp (op, e1, e2) -> "(" ^ string_of_py_expr e1 ^ " " ^ op ^ " " ^ string_of_py_expr e2 ^ ")"
  | PyIfThenElse (e1, e2, e3) -> "if " ^ string_of_py_expr e1 ^ " then " ^ string_of_py_expr e2 ^ " else " ^ string_of_py_expr e3
  | PyCallE (f, es) -> f ^ "(" ^ String.concat ", " (List.map string_of_py_expr es) ^ ")"


let rec string_of_py_fpdefn = function
  | PyFPDefn (fname, args, e, tail) -> "def " ^ fname ^ "(" ^ String.concat ", " args ^ "): return " ^ string_of_py_expr e

let rec string_of_py_prog = function
  | Prog (fdfs, e) -> String.concat "\n" (List.map string_of_py_fpdefn fdfs) ^ "\n" ^ string_of_py_expr e

let rec string_of_py_prog_opt = function
  | Some p -> string_of_py_prog p
  | None -> "None"

let _ = print_string (string_of_py_prog_opt (transf_prog (Prog ([FPDefn ("fac", ["n"], VarE "n")], CallE ("fac", [Const 5])))))

