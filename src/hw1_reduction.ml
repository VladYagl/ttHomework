open Hw1

(* TODO: REMOVE *)
let (!!) = lambda_of_string
let (?<) = string_of_lambda

module VarSet = Set.Make(String)

let rec free_vars x = match x with
    Var name -> VarSet.singleton name
  | App (f, a) -> VarSet.union (free_vars f) (free_vars a)
  | Abs (name, a) -> VarSet.remove name (free_vars a)

let rec free_to_subst phi alpha var = match alpha with
    Var name -> true
  | App (f, a) -> (free_to_subst phi f var) && (free_to_subst phi a var)
  | Abs (name, a) -> if name = var then true
    else not (VarSet.mem var (free_vars a)) ||
      (not (VarSet.mem name (free_vars phi)) && free_to_subst phi a var)

let rec substitute phi alpha var = match alpha with
    Var name -> if name = var then phi else alpha
  | App (f, a) -> App ((substitute phi f var), (substitute phi a var))
  | Abs (name, a) -> if name == var then alpha else
      let new_name, new_a =
        if VarSet.mem name (free_vars phi) then
          let new_name = "t_" ^ string_of_int (Hashtbl.hash (App (a, phi))) in
          new_name, substitute (Var new_name) a name
        else name, a
      in
      Abs(new_name, substitute phi new_a var)

let rec is_alpha_equivalent x y =
  match (x, y) with
    (Var a, Var b) -> a = b
  | (App (p, a), App (q, b)) -> (is_alpha_equivalent p q) && (is_alpha_equivalent a b)
  | (Abs (a, alpha), Abs (b, phi)) ->
    is_alpha_equivalent alpha (substitute (Var a) phi b)
  | _ -> false

let rec is_normal_form alpha = match alpha with
    Var name -> true
  | App (Abs(name, b), a) -> false
  | App (f, a) -> is_normal_form f && is_normal_form a
  | Abs (name, a) -> is_normal_form a

let lambda_comp alpha beta =
  if is_alpha_equivalent alpha beta then 0
  else compare alpha beta

module MemMap = Map.Make(struct type t = lambda let compare = lambda_comp end)

let mem = ref MemMap.empty

let rec normal_beta_reduction alpha =
  if MemMap.mem alpha !mem then
    MemMap.find alpha !mem
  else
    let res = match alpha with
        Var name -> alpha
      | App (Abs(name, b), a) -> substitute a b name
      | App (f, a) ->
        let new_f = normal_beta_reduction f in
        if (new_f != f) then
          App (new_f, a)
        else
          App (f, normal_beta_reduction a)
      | Abs (name, a) -> Abs(name, normal_beta_reduction a)
    in
    mem := MemMap.add alpha res !mem;
    res


let rec reduce_to_normal_form alpha =
    if MemMap.mem alpha !mem then MemMap.find alpha !mem
    else if is_normal_form alpha then alpha
    else reduce_to_normal_form (normal_beta_reduction alpha)

(* Replace my variant with needed one *)
let free_vars x = VarSet.elements (free_vars x)
