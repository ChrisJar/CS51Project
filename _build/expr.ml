
(** Abstract syntax of MiniML expressions *)

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of varid * expr                 (* unary operators *)
  | Binop of varid * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;
  
(** Sets of varids *)
module SS = Set.Make(struct
		      type t = varid
		      let compare = String.compare
		    end);;
  
type varidset = SS.t ;;

(** Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(** Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;
  
(** Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var v -> SS.singleton v
  | Num n -> SS.empty
  | Bool b -> SS.empty
  | Unop(v,e) -> free_vars e
  | Binop(v,e1,e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional(e1,e2,e3) -> SS.union (SS.union (free_vars e2) (free_vars e3)) (free_vars e1)
  | Fun(v,e) -> SS.remove v (free_vars e)
  | Let(v,e1,e2) -> SS.union (free_vars e1) (SS.remove v (free_vars e2))
  | Letrec(v,e1,e2) -> SS.remove v (SS.union (free_vars e1) (free_vars e2))
  | Raise -> SS.empty
  | Unassigned -> SS.empty
  | App(e1,e2) -> SS.union (free_vars e1) (free_vars e2)
;;
  
(** Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let gensym =
  let ctr = ref 0 in
  fun () -> ctr := !ctr + 1; ("var" ^ (string_of_int !ctr)) ;;

let new_varname () : varid = gensym ()
 
(* Replace an old variable for a new variable *)  
let rec replace exp newv oldv =
  match exp with
    | Var v -> if v = oldv then Var newv else Var v
    | Num n -> exp
    | Bool b -> exp
    | Unop(v,e) -> Unop(v,(replace e newv oldv))
    | Binop(v,e1,e2) -> Binop(v,(replace e1 newv oldv),(replace e2 newv oldv))
    | Conditional(e1,e2,e3) -> Conditional((replace e1 newv oldv),(replace e2 newv oldv),(replace e3 newv oldv))
    | Fun(v,e) -> if v = oldv then Fun(newv,(replace e newv oldv)) else Fun(v,(replace e newv oldv))
    | Let(v,e1,e2) -> if v = oldv then Let(newv,(replace e1 newv oldv),(replace e2 newv oldv))
                      else Let(v,(replace e1 newv oldv),(replace e2 newv oldv))
    | Letrec(v,e1,e2) -> if v = oldv then Letrec(newv,(replace e1 newv oldv),(replace e2 newv oldv))
                         else Letrec(v,(replace e1 newv oldv),(replace e2 newv oldv))
    | Raise -> Raise
    | Unassigned -> Unassigned
    | App(e1,e2) -> App((replace e1 newv oldv),(replace e2 newv oldv))

(** Substitute [repl] for free occurrences of [var_name] in [exp] *)
let subst (var_name: varid) (repl: expr) (exp: expr) : expr =
  let freerepl = free_vars repl in
    let rec subst' exp' =
      match exp' with
      | Var v -> if v = var_name then repl else Var v
      | Num n -> exp'
      | Bool b -> exp'
      | Unop(v,e) -> Unop(v,(subst' e))
      | Binop(v,e1,e2) -> Binop(v,(subst' e1),(subst' e2))
      | Conditional(e1,e2,e3) -> Conditional((subst' e1),(subst' e2),(subst' e3))
      | Fun(v,e) -> if v = var_name then exp'
                    else if SS.mem v freerepl then subst' (replace exp' (gensym ()) v)
                    else Fun(v,(subst' e))
      | Let(v,e1,e2) -> if v = var_name then Let(v,(subst' e1),e2)
                        else if SS.mem v freerepl then subst' (replace exp' (gensym ()) v)
                        else Let(v,(subst' e1),(subst' e2))
      | Letrec(v,e1,e2) -> if v = var_name then Letrec(v,e1,e2)
                           else if SS.mem v freerepl then subst' (replace exp' (gensym ()) v)
                           else Letrec(v,(subst' e1),(subst' e2))
      | Raise -> Raise
      | Unassigned -> Unassigned
      | App(e1,e2) -> App((subst' e1),(subst' e2)) in
  subst' exp ;;

(** Returns a string representation of the expr *)
let rec exp_to_string (exp: expr) : string =
  let mkstr1 nm con = nm ^ "(" ^ con ^ ")" in
  let mkstr2 nm con1 con2 = mkstr1 nm (con1 ^ ", " ^ con2) in
  let mkstr3 nm con1 con2 con3 = mkstr2 nm con1 (con2 ^ ", " ^ con3) in 
  match exp with
  | Var v -> mkstr1 "Var" v
  | Num n -> mkstr1  "Num" (string_of_int n)
  | Bool b -> mkstr1 "Bool" (string_of_bool b)
  | Unop(v,e) -> mkstr2 "Unop" v (exp_to_string e)
  | Binop(v,e1,e2) -> mkstr3 "Binop" v (exp_to_string e1) (exp_to_string e2)
  | Conditional(e1,e2,e3) -> mkstr3 "Conditional" (exp_to_string e1) (exp_to_string e2) (exp_to_string e3)
  | Fun(v,e) -> mkstr2 "Fun" v (exp_to_string e)
  | Let(v,e1,e2) -> mkstr3 "Let" v (exp_to_string e1) (exp_to_string e2)
  | Letrec(v,e1,e2) -> mkstr3 "Letrec" v (exp_to_string e1) (exp_to_string e2)
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App(e1,e2) -> mkstr2 "App" (exp_to_string e1) (exp_to_string e2) ;;


