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
    | (WildcardPat, _) -> Env.empty_env()
    | (NilPat , ListVal(j))  when j=NilVal -> Env.empty_env()  
    | (VarPat(str), value)  -> (Env.add_binding str value (Env.empty_env()))
    | (ConsPat(pat1,pat2) , ListVal(ConsVal(head,tail))) -> (Env.combine_envs (patMatch pat1 head) (patMatch pat2 (ListVal(tail)))) 
    | _ -> raise MatchFailure 



let rec findMatch (v:movalue) (lis: (mopat * moexpr) list) : (moenv * moexpr)  =
  match lis with [] -> raise MatchFailure | (p,e)::tail -> (try ((patMatch p v), e) with MatchFailure -> findMatch v tail)

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
    | Var(i) -> (try (Env.lookup i env) with Env.NotBound -> raise DynamicTypeError)
    | BinOp(expr1,op,expr2) -> (match op with Plus -> (match ((evalExpr expr1 env) , (evalExpr expr2 env)) with (IntVal(i),IntVal(j)) -> IntVal(i + j) | _ -> raise DynamicTypeError)   
		 			 | Minus -> (match ((evalExpr expr1 env) , (evalExpr expr2 env)) with (IntVal(i),IntVal(j)) -> IntVal(i - j) | _ -> raise DynamicTypeError)
					 | Times -> (match ((evalExpr expr1 env) , (evalExpr expr2 env)) with (IntVal(i),IntVal(j)) -> IntVal(i * j) | _ -> raise DynamicTypeError)
					 | Eq -> (match ((evalExpr expr1 env) , (evalExpr expr2 env)) with (IntVal(i),IntVal(j)) -> BoolVal(i = j) | _ -> raise DynamicTypeError)
				         | Gt -> (match ((evalExpr expr1 env) , (evalExpr expr2 env)) with (IntVal(i),IntVal(j)) -> BoolVal(i > j) | _ -> raise DynamicTypeError)
				         | Cons -> (match ((evalExpr expr1 env) , (evalExpr expr2 env)) with (i, ListVal(j)) -> ListVal(ConsVal(i,j)) | _ -> raise DynamicTypeError)) 
    | Negate(exp) -> (match (evalExpr exp env) with IntVal(i) -> IntVal(-i) | _ -> raise DynamicTypeError) 
    | If(exp1,exp2,exp3) -> (match (evalExpr exp1 env) with BoolVal(i) -> if i then (evalExpr exp2 env) else (evalExpr exp3 env) | _ -> raise DynamicTypeError) 
    | Function(pat,expr) -> FunctionVal(None,pat,expr,env)
    | FunctionCall(exp1,exp2) -> (match ((evalExpr exp1 env), (evalExpr exp2 env)) with (FunctionVal(opt,pat,expr,env1),j) -> (match opt with None ->(let env2 = (try (patMatch pat j)  with MatchFailure  -> raise MatchFailure) in (evalExpr expr  (Env.combine_envs env1 env2))) | Some str -> (let env2 = (try (patMatch pat j)  with MatchFailure  -> raise MatchFailure) in (evalExpr expr  (Env.combine_envs (Env.add_binding str (FunctionVal(Some str,pat,expr,env1)) env1) env2)))) | _ -> raise MatchFailure)
    | Match(exp, lis) ->  let value = evalExpr exp env in (let (newEnv,resExpr) = (try findMatch value lis with MatchFailure -> raise MatchFailure) in (evalExpr resExpr (Env.combine_envs env newEnv))) 
    | _ -> raise MatchFailure 
  
(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      Expr(e) -> (None, evalExpr e env)
    |  Let(str,expr) -> (Some str, (evalExpr expr env))
    |  LetRec(str,pat,expr) -> (Some str, FunctionVal(Some str,pat,expr,env)) 
    | _ -> raise (ImplementMe "let and let rec not implemented")

