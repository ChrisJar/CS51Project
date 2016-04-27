open Printf
open Expr
open Evaluation

type test = {label: string;
             content: bool Lazy.t;
             time: int;
             fail_msg: string
  } ;;

type status =
  | Passed
  | Failed of string
  | Raised_exn of string
  | Timed_out of int


exception Timeout ;;


let sigalrm_handler =
  Sys.Signal_handle (fun _ -> raise Timeout) ;;

let timeout (time : int) (delayed : 'a Lazy.t) : 'a =
  let old_behavior =
    Sys.signal Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () =
    ignore (Unix.alarm 0);
    Sys.set_signal Sys.sigalrm old_behavior in
  ignore (Unix.alarm time) ;
  let res = Lazy.force delayed in
  reset_sigalrm () ; res ;;


let run_test ({label; time; content; fail_msg} : test)
             (continue : string -> status -> unit)
           : unit =
  try
    if timeout time content
    then continue label Passed
    else continue label (Failed fail_msg)
  with
  | Timeout -> continue label (Timed_out time)
  | exn     -> continue label
               (Raised_exn (Printexc.to_string exn))

let present label status =   match status with
  | Passed -> printf "%s: PASSED\n" label
  | Failed msg -> printf "%s: FAILED %s\n" label msg
  | Timed_out secs -> printf "%s: timed out in %d\n" label secs
  | Raised_exn msg -> printf "%s: raised %s\n" label msg ;;


let report (tests: test list) (part: string) : unit =
  printf "\n\n*** %s TESTS ***\n\n" part;
  List.iter (fun test -> run_test test present) tests ;;


let test ?(fail_msg="somehow") ?(time=5) label content =
  {label = label;
   content = content;
   fail_msg = fail_msg;
   time = time} ;;

(* EXPRESSIONS *)

(* Var *)
let var = Var "x" ;;

(* Num *)
let num1 = Num 0 ;;
let num2 = Num (-3) ;;
let num3 = Num (4) ;;

(* Bool *)
let bool1 = Bool true ;;
let bool2 = Bool false ;;

(* Unop *)
let unop1 = Unop("~",num1) ;;
let unop2 = Unop("~",num2) ;;
let unop3 = Unop("~",num3) ;;

(* Binop *)
let binop1 = Binop("-",num3,num2) ;;
let binop2 = Binop("+",num1,num3) ;;
let binop3 = Binop("*",num2,num2) ;;
let binop4 = Binop("<",num2,num1) ;;

(* Conditional *)
let cond1 = Conditional(Binop("=", Num(4), Num(5)), Num(4), Num(5)) ;;
let cond2 = Conditional(Binop("<", Num(4), Binop("+", Num(3), Num(3))), Num(4), Num(6)) ;;

(* Function *)
let func = Fun("x", Binop("+", Var("x"), Var("y"))) ;;

(* Let *)
let let1 = Let("y", Fun("x", Binop("-", Var("x"), Num(12))), App(Var("y"), Num(6))) ;;
let let2 = Let("x", Num(5), Let("y", Unop("~", Num(5)), Let("z", Unop("~", Num(3)), Binop("*", Binop("*", Var("x"), Var("y")), Var("z"))))) ;;
let let3 = Let("x", Num(1), Let("f", Fun("y", Binop("+", Var("x"), Var("y"))), Let("x", Num(2), App(Var("f"), Num(3))))) ;;

(* Letrec *)
let letrec1 = Letrec("f", Fun("x", Conditional(Binop("=", Var("x"), Num(0)), Var("x"), App(Var("f"), Binop("-", Var("x"), Num(1))))), App(Var("f"), Num(2))) ;;
let letrec2 = Letrec("f", Fun("x", Conditional(Binop("=", Var("x"), Num(1)), Unop("~", Num(2)), App(Var("f"), Binop("-", Var("x"), Num(1))))), App(Var("f"), Num(5))) ;;
let letrec3 = Letrec("f", Fun("x", Conditional(Binop("=", Var("x"), Num(18)), Num(3), App(Var("f"), Binop("*", Var("x"), Num(3))))), App(Var("f"), Num(2))) ;;

(* App *)
let app1 = App(Fun("x", Binop("+", Var("x"), Var("x"))), Num(5)) ;;
let app2 = App(Conditional(Binop("<", Num(3), Num(2)), Fun("x", Binop("+", Var("x"), Num(3))), Fun("y", Binop("*", Var("y"), Num(2)))), Unop("~", Num(5))) ;;

(* STRINGS *)
let numstring = "Num(-3)"
let varstring = "Var(x)"
let boolstring = "Bool(false)"
let unopstring = "Unop(~, Num(0))"
let binopstring = "Binop(+, Num(0), Num(4))"
let letstring = "Let(x, Num(1), Let(f, Fun(y, Binop(+, Var(x), Var(y))), Let(x, Num(2), App(Var(f), Num(3)))))"
let letrecstring = "Letrec(f, Fun(x, Conditional(Binop(=, Var(x), Num(0)), Var(x), App(Var(f), Binop(-, Var(x), Num(1))))), App(Var(f), Num(2)))"


(* ENVIRONMENTS *)

let env1 = Env.create () ;;
let env2 = Env.extend env1 "x" (ref (Env.Val bool2)) ;;
let env3 = Env.extend env2 "y" (ref (Env.Val let2)) ;;

(* DUMMY TESTS *)
let tests =
  [ test "should fail"            (lazy (3 > 4)) ;
    test "should pass"            (lazy (4 > 3)) ;
    test "should time out"        (lazy (let rec f x = f x in
                                         f 1)) ;
    test "should raise exception" (lazy ((List.nth [0;1] 3) = 3))
  ] ;;

(* EXPR TESTS *)

(* exp_to_string tests *)
let es_tests =
  [ test "Exp to string num" (lazy (exp_to_string num2 = numstring)) ;
    test "Exp to string var" (lazy (exp_to_string var = varstring)) ;
    test "Exp to string bool" (lazy (exp_to_string bool2 = boolstring)) ;
    test "Exp to string unop" (lazy (exp_to_string unop1 = unopstring)) ;
    test "Exp to string binop" (lazy (exp_to_string binop2 = binopstring)) ;
    test "Exp to string let" (lazy (exp_to_string let3 = letstring)) ;
    test "Exp to string letrec" (lazy (exp_to_string letrec1 = letrecstring)) ;
  ] ;;

(* new_varname tests *)
let nv_tests =
  [ test "New varname 1" (lazy (new_varname () = "var1")) ;
    test "New varame 2" (lazy (new_varname () = "var2")) ;
    test "New varname 3" (lazy (new_varname () = "var3")) ;
  ] ;;

let expr_tests =
  es_tests @ nv_tests ;;

(* EVALUATION TESTS *)

(* eval test generator *)
let eval_tests eval =
  [ test "Should raise exception" (lazy (eval var env1 = Env.Val (Var "x"))) ;
    test "Eval num" (lazy (eval num2 env1 = Env.Val (Num (-3)))) ;
    test "Eval bool" (lazy (eval bool1 env1 = Env.Val (Bool true))) ;
    test "Eval unop 1" (lazy (eval unop1 env1 = Env.Val (Num 0))) ;
    test "Eval unop 2" (lazy (eval unop2 env1 = Env.Val (Num 3))) ;
    test "Eval unop 3" (lazy (eval unop3 env1 = Env.Val (Num (-4)))) ;
    test "Eval binop 1" (lazy (eval binop1 env1 = Env.Val (Num 7))) ;
    test "Eval binop 2" (lazy (eval binop2 env1 = Env.Val (Num 4))) ;
    test "Eval binop 3" (lazy (eval binop3 env1 = Env.Val (Num 9))) ;
    test "Eval binop 4" (lazy (eval binop4 env1 = Env.Val (Bool true))) ;
    test "Eval conditional 1" (lazy (eval cond1 env1 = Env.Val (Num 5))) ;
    test "Eval conditional 2" (lazy (eval cond2 env1 = Env.Val (Num 4))) ;
    test "Eval let1" (lazy (eval let1 env1 = Env.Val (Num (-6)))) ;
    test "Eval let2" (lazy (eval let2 env1 = Env.Val (Num 75))) ;
    test "Eval let3" (lazy (let res = (eval let3 env1) in
                       if eval == eval_d then res = Env.Val (Num 5)
                         else res = Env.Val (Num 4))) ;
    test "Eval letrec1" (lazy (eval letrec1 env1 = Env.Val (Num 0))) ;
    test "Eval letrec2" (lazy (eval letrec2 env1 = Env.Val (Num (-2)))) ;
    test "Eval letrec3" (lazy (eval letrec3 env1 = Env.Val (Num 3))) ;
    test "Eval app1" (lazy (eval app1 env1 = Env.Val (Num 10))) ;
    test "Eval app2" (lazy (eval app2 env1 = Env.Val (Num (-10)))) ;
  ] ;;


(* ENV TESTS *)

(* close tests *)
let close_tests =
  [ test "Close letrec in empty environment" (lazy (Env.close letrec2 env1 =
      Env.Closure(letrec2,env1))) ;
    test "Close let" (lazy (Env.close let3 env2 = Env.Closure(let3,env2))) ;
    test "Close app" (lazy (Env.close app1 env3 = Env.Closure(app1,env3))) ;
  ] ;;

(* close tests *)
let lookup_tests =
  [ test "Lookup should raise exception" (lazy (Env.lookup env1 "x" =
      Env.Val bool2)) ;
    test "Lookup x" (lazy (Env.lookup env3 "x" = Env.Val bool2)) ;
    test "Lookup y" (lazy (Env.lookup env3 "y" = Env.Val let2)) ;
  ] ;;

(* extend tests *)
let extend_tests =
  [ test "Extend change value"(lazy (Env.lookup (Env.extend env3 "y"
  	  (ref (Env.Val let1))) "y" = Env.Val let1)) ;
    test "Extend add new variable z" (lazy (Env.lookup (Env.extend env3 "z"
      (ref (Env.Val cond1))) "z" = Env.Val cond1)) ;
    test "Extend an empty environment" (lazy (Env.lookup (Env.extend env1 "z" 
      (ref (Env.Val app2))) "z" = Env.Val app2)) ;
  ] ;;

let env_tests =
  close_tests @ lookup_tests @ extend_tests ;;

(* TESTING SEQUENCE *)

report expr_tests "EXPR" ;;
report (eval_tests eval_s) "EVAL_S" ;;
report (eval_tests eval_d) "EVAL_D" ;;
report (eval_tests eval_l) "EVAL_L" ;;
report env_tests "ENV" ;;

