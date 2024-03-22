  (* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String)

(* TODO: implement *)

let rec names_expr = function
    Const(_) -> StringSet.empty
  | VarE(x) -> StringSet.singleton x
  | BinOp(_, e1, e2) -> StringSet.union (names_expr e1) (names_expr e2)
  | IfThenElse(e1, e2, e3) -> StringSet.union (names_expr e1) (StringSet.union (names_expr e2) (names_expr e3))
  | CallE (f, es) -> List.fold_left (fun acc e -> StringSet.union acc (names_expr e)) (StringSet.singleton f) es

  

(* TODO: implement *)
let transf_prog (Prog(fdfs, e)) = Prog(fdfs, e)

let rec is_tail_expr fname e = match e with
  | VarE v ->  v != fname
  | BinOp (_, e1, e2) -> is_tail_expr fname e1 && is_tail_expr fname e2
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
  