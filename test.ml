open Hw1;;

print_string " int of peano test: ";;
print_int (int_of_peano (S (S (Z))));;

print_string "\n peano of int test: ";;
print_int (int_of_peano (peano_of_int 10));;

print_string "\n inc test: ";;
print_int (int_of_peano (inc (inc (peano_of_int 10))));;

print_string "\n add test: ";;
print_int (int_of_peano (add (peano_of_int 10) (peano_of_int 31)));;

print_string "\n sub test: ";;
print_int (int_of_peano (sub (peano_of_int 30) (peano_of_int 10)));;

print_string "\n mul test: ";;
print_int (int_of_peano (mul (peano_of_int 13) (peano_of_int 8)));;

print_string "\n power test: ";;
print_int (int_of_peano (power (peano_of_int 2) (peano_of_int 10)));;



(*
print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\x.\\y.x"));;
*)
