open OUnit2

open Hw1
open Hw1_reduction
open Hw2_unify
open Hw2_inference

let string_printer str = str
let list_printer l = String.concat "" l
let (!) = lambda_of_string
let (?<) = string_of_lambda
let (=?) = is_alpha_equivalent
let is_not_alpha_equivalent a b = not (is_alpha_equivalent a b)

let assert_equal_string = assert_equal ~printer:string_printer
let assert_equal_int = assert_equal ~printer:string_of_int
let assert_equal_list = assert_equal ~printer:list_printer
let assert_equal_lambda = assert_equal ~printer:string_of_lambda ~cmp:is_alpha_equivalent
let assert_not_equal_lambda = assert_equal ~printer:string_of_lambda ~cmp:is_not_alpha_equivalent

let int_of_peano_test ctxt = assert_equal_int 2 (int_of_peano (S (S (Z))))
let peano_of_int_test ctxt = assert_equal_int 10 (int_of_peano (peano_of_int 10))
let inc_test ctxt = assert_equal_int 12 (int_of_peano (inc (inc (peano_of_int 10))))
let add_test ctxt = assert_equal_int 41 (int_of_peano (add (peano_of_int 10) (peano_of_int 31)))
let sub_test ctxt = assert_equal_int 20 (int_of_peano (sub (peano_of_int 30) (peano_of_int 10)))
let mul_test ctxt = assert_equal_int 104 (int_of_peano (mul (peano_of_int 13) (peano_of_int 8)))
let power_test ctxt = assert_equal_int 1024 (int_of_peano (power (peano_of_int 2) (peano_of_int 10)))

let peano_suite = "peano_suite">:::[
    "int_of_peano_test">:: int_of_peano_test;
    "int_of_peano_test">:: peano_of_int_test;
    "inc_test">:: inc_test;
    "add_test">:: add_test;
    "sub_test">:: sub_test;
    "mul_test">:: mul_test;
    "power_test">:: power_test;
  ]

let reverse_test ctxt = assert_equal [7;6;5;4;3;2;1] (rev [1;2;3;4;5;6;7])
let merge_sort_test ctxt =
  let test_list = [8;2;5;4;6;5;3;1;1;0;8;6] in
  assert_equal (List.sort compare test_list) (merge_sort test_list)

let list_suite = "list_suite">:::[
    "reverse_test">:: reverse_test;
    "merge_sort_test">:: merge_sort_test;
  ]

let string_of_lambda_test ctxt =
  assert_equal_string "(F (\\x.(T x)))" ?<(App (Var "F", Abs ("x", App (Var "T", Var "x"))))

let lambda_of_string_test ctxt =
  assert_equal_string "((\\x.(F x)) (T x))" ?< !"(\\x.F x) (T x)";
  assert_equal_string "(((\\x.x) y) (T x))" ?< !"((\\x.x) y) (T x)";
  assert_equal_string "(F x)" ?< ! "F x";
  assert_equal_string "T" ?< ! "T";
  assert_equal_string "((\\x.(F x)) (T x))" ?< !"(\\x.F    x    ) (T x    )    ";
  assert_equal_string "((\\z.(z (y z))) ((z x) z))" ?< !"(\\z.z (y z)) (z x) z"

let lambda_suite = "lambda_suite">:::[
    "string_of_lambda_test">:: string_of_lambda_test;
    "lambda_of_string_test">:: lambda_of_string_test;
  ]

let free_vars_test ctxt = assert_equal_list ["F"; "T"; "y"] (List.sort compare (free_vars !"((\\x.(F x)) (T y))"))

let free_to_subst_test ctxt =
  assert_bool "1" (     free_to_subst (Var "x") !"\\y.(\\x.(y y) y) (\\x.f y)"      "y");
  assert_bool "2" (not (free_to_subst (Var "x") !"(\\x.(y y) y) (\\x.f y)"          "y"));
  assert_bool "3" (     free_to_subst (Var "x") !"((\\f.(\\x.x) y) y) ((\\x.f) y)"  "y");
  assert_bool "4" (not (free_to_subst (Var "x") !"(\\f.((\\x.x) y) y) (\\x.f y)"    "y"))

let substitute_test ctxt =
  assert_equal_lambda (Var "x") (substitute (Var "x") (Var "y") "y");
  assert_equal_lambda !"x \\x.x" (substitute !"x" !"y \\x.x" "y")

let is_alpha_equivalent_test ctxt =
  assert_equal_lambda !"x" !"x";
  assert_equal_lambda !"\\x.x" !"\\y.y";
  assert_equal_lambda !"F x" !"F x";

  assert_equal_lambda !"(\\f.((\\y.y) f) f) (\\x.f y)"
                      !"(\\z.((\\y.y) z) z) (\\x.f y)";

  assert_not_equal_lambda !"(\\f.((\\y.y) x) f) (\\x.f y)"
                          !"(\\z.((\\y.y) z) z) (\\x.f y)"

let is_normal_form_test ctxt =
  assert_bool "1" (is_normal_form !"x");
  assert_bool "2" (not (is_normal_form !"(\\x.x) y"));
  assert_bool "3" (is_normal_form !"x x");
  assert_bool "4" (is_normal_form !"\\x.x y")

let normal_beta_reduction_test ctxt =
  assert_equal_lambda !"y" (normal_beta_reduction !"(\\x.x) y");

  assert_equal_lambda !"((\\y.y) ((\\x.f) y)) ((\\x.f) y)"
    (normal_beta_reduction !"(\\z.((\\y.y) z) z) ((\\x.f) y)")

let reduce_to_normal_form_test ctxt =
  assert_equal_lambda !"y" (reduce_to_normal_form !"(\\x.x) y");

  assert_equal_lambda !"(f) (f)"
    (reduce_to_normal_form !"(\\z.((\\y.y) z) z) ((\\x.f) y)")

let reduction_suite = "reduction_suite">:::[
    "free_vars_test">:: free_vars_test;
    "substitute_test">:: substitute_test;
    "free_to_subst_test">:: free_to_subst_test;
    "is_alpha_equivalent_test">:: is_alpha_equivalent_test;
    "is_normal_form_test">:: is_normal_form_test;
    "normal_beta_reduction_test">:: normal_beta_reduction_test;
    "reduce_to_normal_form_test">:: reduce_to_normal_form_test;
  ]

let rec string_of_term term = match term with
    Var name -> name
  | Fun (name, args) -> name ^ "(" ^ (String.concat ", " (List.map (fun term -> string_of_term term) args)) ^ ")"

let rec string_of_type s_type = match s_type with
    S_Elem x -> x
  | S_Arrow(f, a) -> "(" ^ (string_of_type f) ^ "->" ^ (string_of_type a) ^ ")"

let rec string_of_hmtype hm_type = match hm_type with
    HM_Elem x -> x
  | HM_Arrow(f, a) -> "(" ^ (string_of_hmtype f) ^ "->" ^ (string_of_hmtype a) ^ ")"
  | HM_ForAll(name, a) -> "!" ^ name ^ "." ^ (string_of_hmtype a)

module TypeMap = Map.Make(String)

let (?<<) = string_of_term
let solution_printer system = "{\n" ^ String.concat "\n" (List.map (fun (left, right) -> left ^ "=" ^ ?<<right) system) ^ "\n}\n"
let assert_equal_term = assert_equal ~printer:string_of_term
let assert_equal_type = assert_equal ~printer:string_of_type
let assert_equal_solution = assert_equal ~printer:solution_printer

let (!!) x = Var x
let (=>) x y = Fun(x, y)

let string_of_term_test ctxt =
  assert_equal_string "F(x)" (?<< (Fun ("F", [Var "x"])));
  assert_equal_string "F(x, T(x, y))" (?<< (Fun ("F", [Var "x"; Fun ("T", [Var "x"; Var "y"])])))

let system_to_equation_test =
  let left, right = system_to_equation [
      Fun ("F", [Var "x"; Fun ("T", [Var "x"; Var "y"])]), Var "x";
      Fun ("T", []), Var "x"
    ] in
  print_string ?<<left;
  print_string "=";
  print_string ?<<right;
  print_string "\n"

let apply_substitution_test ctxt =
  assert_equal_term !!"x" (apply_substitution ["y", !!"x"] !!"y");
  assert_equal_term ("P"=>[!!"x"; "f"=>[!!"y"]]) (apply_substitution ["p", !!"x"; "q", "f"=>[!!"y"]] ("P"=>[!!"p"; !!"q"]))

let check_solution_test ctxt =
  assert_bool "1" (check_solution ["x", !!"y"] [!!"x", !!"y"]);
  assert_bool "2" (check_solution ["x", "f"=>[!!"y"]] ["t"=>[ !!"x" ], "t"=>[ "f"=>[ !!"y" ] ]])

let solve_system_test ctxt =
  let system = [
    "t"=>[ !!"x" ], "t"=>[ "f"=>[ !!"y" ] ];
    "f"=>[ !!"x" ], "f"=>[ !!"z" ];
    "f"=>[ !!"x" ], "f"=>[ !!"z" ]
  ] in
  assert_equal_solution [
    "x", "f"=>[!!"y"];
    "z", "f"=>[!!"y"]
  ] (match (solve_system system) with
        Some value -> value
      | None -> assert_failure "No solution WutFace");

  let system = [
    !!"a", "impl"=>[ !!"b"; !!"g" ];
    !!"a", "impl"=>[ !!"g"; !!"b" ];
  ] in
  assert_equal_solution [
    "a", "impl"=>[!!"b"; !!"b"];
    "g", !!"b"
  ] (match (solve_system system) with
        Some value -> value
      | None -> assert_failure "No solution WutFace");

  assert_equal None (solve_system [!!"x", "f"=>[!!"x"]])


let unify_suite = "unify_suite">:::[
    "string_of_term_test">:: string_of_term_test;
    "apply_substitution_test">:: apply_substitution_test;
    "check_solution_test">:: check_solution_test;
    "solve_system_test">:: solve_system_test;
  ]

let infer_simp_type_test ctxt =
  let res = infer_simp_type !"\\f.\\x.(f (f x))" in
  match res with
    None -> assert_failure "No type"
  | _ -> print_string "\n----- INFER SIMPLE TYPE WASN'T TESTED -----\n"
  (* | Some (vars, res_type) -> assert_equal_type (S_Arrow ((S_Arrow (S_Elem "delta", S_Elem "delta")), (S_Arrow (S_Elem "delta", S_Elem "delta")))) res_type *)

let string_of_hmtype_test ctxt =
  assert_equal_string "!x.(x->y)" (string_of_hmtype (HM_ForAll ("x", (HM_Arrow ((HM_Elem "x"), (HM_Elem "y"))))))

let (!!) x = HM_Var x
let (=>) x y = HM_App(x, y)
let (|>) name a = HM_Abs(name, a)

let algorithm_w_test ctxt =
  match algorithm_w (HM_Let("z", ("x" |> !!"f" => !!"x"), !!"z" => !!"y")) with
  (* match algorithm_w (HM_Let("z", !!"x", !!"z")) with *)
  (* match algorithm_w (HM_Var "x") with *)
    None -> assert_failure "No type"
  | Some (context, hm_type) -> print_string "\n\nResult:\n";
    print_string ("context:\n" ^ (String.concat ",\n" (List.map (fun (name, hm_type) -> name ^ "=" ^ (string_of_hmtype hm_type)) context)));
    print_string ("\ntype: " ^ (string_of_hmtype hm_type) ^ "\n");
    print_string "\n----- ALGORITHM W WASN'T TESTED ------\n"

let inference_suite = "inference_suite">:::[
    "string_of_hmtype_test">:: string_of_hmtype_test;
    "infer_simp_type_test">:: infer_simp_type_test;
    "algorithm_w_test">:: algorithm_w_test
  ]

let () =
  print_string "HW1\n";
  run_test_tt_main peano_suite;
  run_test_tt_main list_suite;
  run_test_tt_main lambda_suite;
  print_string "HW1 Reduction\n";
  run_test_tt_main reduction_suite;
  print_string "HW2 Unify\n";
  run_test_tt_main unify_suite;
  print_string "HW2 Inference\n";
  run_test_tt_main inference_suite;
