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


print_string "\n\n_______________________\n list testing";;

print_string "\n reverse test: ";;
List.iter(print_int) (rev [1;2;3;4;5;6;7]);;

print_string "\n merge_sort test: ";;
List.iter(print_int) (merge_sort [8;2;5;4;6;5;3;1;1;0;8;6]);;


print_string "\n\n_______________________\n lambda testing";;

print_string "\n string_of_lambda test: ";;
print_string (string_of_lambda (App (Var "F", Abs ("x", App (Var "T", Var "x")))));;
print_string "\n";;
print_string (string_of_lambda (lambda_of_string (string_of_lambda (App (Var "F", Abs ("x", App (Var "T", Var "x")))))));;
print_string "\n";;
print_string (string_of_lambda (lambda_of_string "T"));;
print_string "\n";;
print_string (string_of_lambda (lambda_of_string "F x"));;
print_string "\n";;
print_string (string_of_lambda (lambda_of_string "(λx.F x) (T x)"));;
print_string "\n";;
print_string (string_of_lambda (lambda_of_string "T"));;
print_string "\n";;
print_string (string_of_lambda (lambda_of_string "T"));;
print_string "\n";;
print_string (string_of_lambda (lambda_of_string "T"));;
print_string "\n";;
print_string (string_of_lambda (lambda_of_string "T"));;
print_string "\n";;
print_string (string_of_lambda (lambda_of_string "T"));;
print_string "\n";;
print_string (string_of_lambda (lambda_of_string "T"));;
print_string "\n";;
print_string (string_of_lambda (lambda_of_string "T"));;
print_string "\n";;

print_string "\n labda_sym size: ";;
print_int (String.length "λ");;

print_string "\n\n test finished";;

(*
print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\x.\\y.x"));;
*)
