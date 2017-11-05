open Hw1
(* open Hw1_reduction *)
(* open Hw2_unify *)

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type
type hm_lambda = HM_Var of string | HM_Abs of string * hm_lambda | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda

module TypeMap = Map.Make(String)

let create_system alpha =
  let new_type type_map = "type_" ^ string_of_int (Hashtbl.hash type_map) in
  let rec impl alpha type_map = match alpha with
      Hw1.Var x -> ([], TypeMap.find x type_map)
    | Hw1.App (f, a) ->
      let system_f, type_f = impl f type_map in
      let system_a, type_a = impl a type_map in
      let pi_type = S_Elem(new_type type_map) in
      (system_a @ system_f @ [(type_f, S_Arrow(type_a, pi_type))], pi_type)
    | Hw1.Abs (name, a) ->
      let new_map = TypeMap.add name (S_Elem(new_type type_map)) type_map in
      let system_a, type_a = impl a new_map in
      (system_a, S_Arrow (TypeMap.find name new_map, type_a)) in

  let type_map = List.fold_left (fun map type_name -> TypeMap.add type_name (S_Elem(new_type map)) map) TypeMap.empty (Hw1_reduction.free_vars alpha) in
  impl alpha type_map


let infer_simp_type alpha =
  let simp_system, res_type = create_system alpha in

  let rec term_of_type alpha = match alpha with
      S_Elem x -> Hw2_unify.Var x
    | S_Arrow (f, a) -> Hw2_unify.Fun("impl", [term_of_type f; term_of_type a]) in

  let rec type_of_term term = match term with
      Hw2_unify.Var x -> S_Elem x
    | Hw2_unify.Fun("impl", [a; b]) -> S_Arrow(type_of_term a, type_of_term b)
    | _ -> failwith "Cant make simp_type from algebraic_term" in

  let system = List.map (fun (a, b) -> (term_of_type a, term_of_type b)) simp_system in
  match Hw2_unify.solve_system system with
    None -> None
  | Some solution -> Some (
      List.map (fun (a, b) -> (a, type_of_term b)) solution,
      type_of_term (Hw2_unify.apply_substitution solution (term_of_type res_type))
    )

let typeMap_union a b = TypeMap.merge (
    fun f a b -> match (a, b) with
        Some(a), Some(b) -> Some(a)
      | None, Some(b) -> Some(b)
      | Some(a), None -> Some(a)
      | None, None -> None
  ) a b

let algorithm_w alpha =

  let rec var_types alpha = match alpha with
      HM_Var name -> TypeMap.singleton name (HM_Elem ("type_var_" ^ name))
    | HM_Abs (name, a) -> TypeMap.remove name (var_types a)
    | HM_App (f, a) -> typeMap_union (var_types f) (var_types a)
    | HM_Let (name, a, b) -> typeMap_union (var_types a) (TypeMap.remove name (var_types b)) in

  let rec apply_context context hm_type = match hm_type with
      HM_Elem name -> if (TypeMap.mem name context) then TypeMap.find name context else hm_type
    | HM_Arrow (f, a) -> HM_Arrow(apply_context context f, apply_context context a)
    | HM_ForAll (name, a) -> HM_ForAll(name, apply_context (TypeMap.remove name context) a) in

  let merge_context first second = typeMap_union (TypeMap.map (apply_context second) first) second in

  let count = ref 0 in
  let new_type () =
    count := !count + 1;
    HM_Elem ("type_" ^ (string_of_int !count)) in

  let add_for_all context hm_type =
    let rec free_vars hm_type = match hm_type with
        HM_Elem name -> TypeMap.singleton name hm_type
      | HM_ForAll (name, a) -> TypeMap.remove name (free_vars a)
      | HM_Arrow  (f, a) -> typeMap_union (free_vars f) (free_vars a) in

    let free_in_context = (TypeMap.fold (fun name hm_type var_list -> typeMap_union (free_vars hm_type) var_list) context TypeMap.empty) in
    let need_to_add = TypeMap.fold (fun name hm_type var_list -> TypeMap.remove name var_list) free_in_context (free_vars hm_type) in
    TypeMap.fold (fun name var_type hm_type -> HM_ForAll(name, hm_type)) need_to_add hm_type in

  let rec remove_for_all hm_type = match hm_type with
      HM_ForAll (name, a) -> apply_context (TypeMap.singleton name (new_type())) (remove_for_all a)
    | _ -> hm_type in

  let rec term_of_type hm_type = match hm_type with
      HM_Elem x -> Hw2_unify.Var x
    | HM_Arrow (f, a) -> Hw2_unify.Fun("arrow", [term_of_type f; term_of_type a])
    | _ -> failwith "Can't create algebraic_term with ForAll type" in

  let rec type_of_term term = match term with
      Hw2_unify.Var x -> HM_Elem x
    | Hw2_unify.Fun("arrow", [a; b]) -> HM_Arrow(type_of_term a, type_of_term b)
    | _ -> failwith "Cant make hm_type from algebraic_term" in

  let rec impl context alpha =
    let new_type = new_type() in
    match alpha with
      HM_Var name -> (TypeMap.empty, remove_for_all (TypeMap.find name context))
    | HM_Abs (name, a) ->
      let (new_context, hm_type) = impl (TypeMap.add name new_type context) a in
      (new_context, HM_Arrow(apply_context new_context new_type, hm_type))
    | HM_Let (name, a, b) ->
      let (a_context, a_type) = impl context a in
      let temp_context = TypeMap.map (apply_context a_context) context in
      let new_context = TypeMap.add name (add_for_all (TypeMap.remove name temp_context) a_type) temp_context in
      let (b_context, b_type) = impl new_context b in
      (merge_context b_context a_context, b_type)
    | HM_App (f, a) ->
      let (f_context, f_type) = impl context f in
      let (a_context, a_type) = impl (TypeMap.map (apply_context f_context) context) a in
      match (Hw2_unify.solve_system [term_of_type (apply_context a_context f_type), term_of_type (HM_Arrow(a_type, new_type))]) with
        Some ans ->
        let temp_context = List.fold_left (fun context (name, term) -> TypeMap.add name (type_of_term term) context) TypeMap.empty ans in
        let new_context = merge_context temp_context (merge_context a_context f_context) in
        (new_context, apply_context new_context new_type)
      | None -> failwith "System has no solution, NANI?!"
  in

  let (context, hm_type) = impl (var_types alpha) alpha in
  Some ((TypeMap.bindings context), hm_type)



