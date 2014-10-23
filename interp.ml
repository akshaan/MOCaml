(* Name: Akshaan Kakar

   UID: 804029538

   Others With Whom I Discussed Things:

   Other Resources I Consulted:

*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
*)
exception DynamicTypeError

(* This exception is thrown when pattern matching fails during evaluation. *)
exception MatchFailure

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
      (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | (BoolPat(i),BoolVal(j)) when i=j -> Env.empty_env()
    | (WildcardPat,j) -> Env.empty_env()
    | (NilPat , ListVal(j))  when j=NilVal -> Env.empty_env()   
    | _ -> raise (ImplementMe "pattern matching not implemented")


(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
      IntConst(i) -> IntVal(i)
    | BoolConst(i) -> BoolVal(i)
    | Nil -> ListVal(NilVal)
    | Var(i) -> Env.lookup i env
    | BinOp(IntConst(expr1),op,IntConst(expr2)) -> match op with Plus -> IntVal(expr1 + expr2) 
					 | Minus -> IntVal(expr1 - expr2)
					 | Times -> IntVal(expr1 * expr2)
					 | Eq -> BoolVal(expr1 = expr2)
				         | Gt -> BoolVal(expr1 > expr2)
    | _ -> raise (ImplementMe "expression evaluation not implemented")


(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      Expr(e) -> (None, evalExpr e env)
    | _ -> raise (ImplementMe "let and let rec not implemented")

