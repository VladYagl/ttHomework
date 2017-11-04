type algebraic_term = Var of string | Fun of string * (algebraic_term list)

(* TODO: REMOVE *)
let rec string_of_term term = match term with
    Var name -> name
  | Fun (name, args) -> name ^ "(" ^ (String.concat ", " (List.map (fun term -> string_of_term term) args)) ^ ")"
let (?<<) = string_of_term
let solution_printer system = String.concat "\n" (List.map (fun (left, right) -> left ^ "=" ^ ?<<right) system)

(* val system_to_equation: (algebraic_term * algebraic_term) list -> (algebraic_term * algebraic_term) *)
let system_to_equation system =
  let fun_name = "F_" ^ string_of_int (Hashtbl.hash system) in
  let left = List.map (fun (left, right) -> left) system in
  let right = List.map (fun (left, right) -> right) system in
  Fun (fun_name, left), Fun (fun_name, right)

module VarMap = Map.Make(String)

(* val apply_substitution: (string * algebraic_term) list -> algebraic_term -> algebraic_term *)
let apply_substitution sub_list term =
  let sub_map = List.fold_left (fun map (var, term) -> VarMap.add var term map) VarMap.empty sub_list in
  let rec impl sub_map term =
    match term with
      Var name -> if VarMap.mem name sub_map then VarMap.find name sub_map else term
    | Fun (name, args) -> Fun (name, List.map (fun term -> impl sub_map term) args)
  in impl sub_map term

(* val check_solution: (string * algebraic_term) list -> (algebraic_term * algebraic_term) list -> bool *)
let check_solution solution system =
  List.fold_left (fun result (left, right) ->
      result && (apply_substitution solution left = apply_substitution solution right)
    ) true system

let rec member var term =
  match term with
    Var name -> name = var
  | Fun (f, args) -> List.fold_left (fun last arg -> last || member var arg) false args

(* val solve_system: (algebraic_term * algebraic_term) list -> (string * algebraic_term) list option *)
let solve_system system =
  let rec impl system solution = match system with
      [] -> Some (VarMap.bindings solution)
    | first::other ->
      match first with left, right ->
        (* let () = print_string (?<<left ^ " = " ^ ?<<right ^ "\n") in *)
        if left = right then impl other solution
        else match first with

          | Var x, any ->
            if member x any then None
            else 
              let new_solution = VarMap.add x any (VarMap.map (fun term -> apply_substitution [(x, any)] term) solution) in
              let do_sub = apply_substitution (VarMap.bindings new_solution) in
              impl (List.map (fun (left, right) -> do_sub left, do_sub right) other) new_solution

          | any, Var x -> impl ((Var x, any)::other) solution

          | Fun(f, f_args), Fun(g, g_args) ->
            if f <> g then None
            else impl (List.fold_left2 (fun other fa ga -> (fa, ga)::other) other f_args g_args) solution

  in impl system VarMap.empty
