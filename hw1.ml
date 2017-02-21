type peano = Z | S of peano;; (* типы необходимо копировать в реализацию *)
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x = if x < 0 then failwith "peano_of_int < 0" else if x == 0 then Z else S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
    Z -> 0
  | S x -> 1 + int_of_peano x;;

let inc x = S x;;

let rec add x y = match x with
    Z -> y
  | S x -> S (add x y);;

let rec sub x y = match y with
    Z -> x
  | S y -> match x with 
      Z -> failwith "sub x y : x < y"
    | S x -> sub x y;;

let rec mul x y = match y with
    Z -> Z
  | S y -> add x (mul x y);;

let rec power x y = match y with
    Z -> S(Z)
  | S y -> mul x (power x y);;
                     
let rev x = failwith "Not implemented";;
let merge_sort x = failwith "Not implemented";;
                     
let string_of_lambda x = failwith "Not implemented";;
let lambda_of_string x = failwith "Not implemented";;
