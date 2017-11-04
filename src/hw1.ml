type peano = Z | S of peano (* типы необходимо копировать в реализацию *)
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda

let rec peano_of_int x = if x < 0 then failwith "peano_of_int < 0" else if x == 0 then Z else S (peano_of_int (x - 1))

let rec int_of_peano p = match p with
    Z -> 0
  | S x -> 1 + int_of_peano x

let inc x = S x

let rec add x y = match x with
    Z -> y
  | S x -> S (add x y)

let rec sub x y = match y with
    Z -> x
  | S y -> match x with
      Z -> failwith "sub x y : x < y"
    | S x -> sub x y

let rec mul x y = match y with
    Z -> Z
  | S y -> add x (mul x y)

let rec power x y = match y with
    Z -> S(Z)
  | S y -> mul x (power x y)

let rec rev x = match x with
    [] -> []
  | head :: tail -> (rev tail) @ (head :: [])

let merge_sort x =

  let rec merge a b = match a with
      [] -> b
    | a_head :: a_tail -> match b with
        [] -> a
      | b_head :: b_tail ->
        if a_head < b_head then
          a_head :: (merge a_tail b)
        else
          b_head :: (merge a b_tail) in

  let rec impl x left right =
    if (left >= right - 1) then (List.nth x left) :: []
    else
      let m = (left + right) / 2 in
      merge (impl x left m) (impl x m right) in

  impl x 0 (List.length x)

let rec string_of_lambda x = match x with
    Var name -> name
  | Abs (name, a) -> "(\\" ^ name ^ "." ^ (string_of_lambda a) ^ ")"
  | App (f, a) -> "(" ^ (string_of_lambda f) ^ " " ^ (string_of_lambda a) ^ ")"

let rec lambda_of_string s =
  (* print_string ("lambda_of_string " ^ s ^ "\n"); *)

  let first_on_top s c =
    let rec impl s c index level =
      if (index >= String.length s) then
        String.length s
      else
        if (level = 0 && s.[index] = c) then
          index
        else
          impl s c (index + 1) (level +
                                             match s.[index] with
                                               '(' -> 1
                                             | ')' -> -1
                                             | c -> 0) in
    impl s c 0 0 in

  let is_letter c =
    let c = Char.lowercase c in
    if Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' then '1'
    else '0' in

  let s = String.trim s in
  match String.get s 0 with
  | '(' when first_on_top s ' ' = String.length s ->
    lambda_of_string (String.sub s 1 (String.length s - 2))
  | c when c =  '\\' ->
    let dot_pos = String.index s '.' in
    Abs (String.sub s 1 (dot_pos - 1), lambda_of_string (String.sub s (dot_pos + 1) (String.length s - dot_pos - 1)))
  | c when not (String.contains (String.map (is_letter) s) '0') ->
    Var s
  | c ->
    let space_pos = first_on_top s ' ' in
    App (lambda_of_string (String.sub s 0 (space_pos)), lambda_of_string (String.sub s (space_pos + 1) (String.length s - space_pos - 1)))
