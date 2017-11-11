open Hw1

(* TODO: REMOVE *)
let (!!) = lambda_of_string
let (?<) = string_of_lambda

module VarSet = Set.Make(String)
module VarMap = Map.Make(String)

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

let rec normal_beta_reduction alpha = match alpha with
    Var name -> alpha
  | App (Abs(name, b), a) -> substitute a b name
  | App (f, a) ->
    let new_f = normal_beta_reduction f in
    if (new_f != f) then
      App (new_f, a)
    else
      App (f, normal_beta_reduction a)
  | Abs (name, a) -> Abs(name, normal_beta_reduction a)

type lambda_ref = VarRef of string | AbsRef of (string * lambda_ref ref) | AppRef of (lambda_ref ref * lambda_ref ref);;

let rec ref_of_lambda alpha = match alpha with
    Var a -> ref (VarRef a)
  | App (f, a) -> ref (AppRef (ref_of_lambda f, ref_of_lambda a))
  | Abs (name, a) -> ref (AbsRef (name, ref_of_lambda a))

let rec lambda_of_ref ref = match !ref with
    VarRef a -> Var a
  | AppRef(f, a) -> App (lambda_of_ref f, lambda_of_ref a)
  | AbsRef(name, a) -> Abs (name, lambda_of_ref a)

let rec substitute_ref phi_ref alpha_ref var = match !alpha_ref with
    VarRef a -> if a = var then alpha_ref := !phi_ref
  | AppRef(a, b) ->
    substitute_ref phi_ref a var;
    substitute_ref phi_ref b var
  | AbsRef(a, b) -> if a <> var then substitute_ref phi_ref b var

let rec varrep alpha map = match alpha with
    Var a -> 
    if VarMap.mem a map then Var (VarMap.find a map)
    else alpha
  | App(f, a) -> App(varrep f map, varrep a map)
  | Abs(name, a) ->
    let new_name = "t_" ^ string_of_int (Hashtbl.hash alpha) in
    Abs(new_name, varrep a (VarMap.add name new_name map));;  

let rec reduce_to_normal_form alpha =
  let alpha_ref = ref_of_lambda alpha in

  let rec reduce alpha_ref = match !alpha_ref with
      VarRef (var) -> None
    | AbsRef (name, a) -> (match reduce a with
          Some res -> Some alpha_ref
        | None -> None)
    | AppRef (f, a) -> match !f with
        AbsRef (name, b) ->
        let new_name = "t_" ^ string_of_int (Hashtbl.hash f) in
        alpha_ref := !(ref_of_lambda (varrep (lambda_of_ref b) (VarMap.singleton name new_name)));
        substitute_ref a alpha_ref new_name;
        Some alpha_ref
      | _ -> match reduce f with
          Some ret -> Some alpha_ref
        | None -> match reduce a with
            Some ret -> Some alpha_ref
          | None -> None in

  let rec reqursion alpha_ref = match reduce alpha_ref with
      Some ret -> reqursion ret
    | None -> alpha_ref in

  lambda_of_ref (reqursion alpha_ref)


(* Replace my variant with needed one *)
let free_vars x = VarSet.elements (free_vars x)
