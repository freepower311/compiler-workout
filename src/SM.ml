open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         


let rec eval env (stack, (state, inputStream, outputStream)) = function
	| [] -> stack, (state, inputStream, outputStream)    
	| instruction :: tail -> 
		match instruction with
		| BINOP operation -> 
			(match stack with 
          | y :: x :: rest -> eval env ((Expr.evaluateOperation operation x y) :: rest, (state, inputStream,outputStream))
          | _ -> failwith "Stack does not have enough elements") tail
		| CONST value -> eval env (value :: stack, (state, inputStream, outputStream)) tail
    | READ -> 
      let num = List.hd inputStream in 
      eval env (num :: stack, (state, List.tl inputStream, outputStream)) tail
    | WRITE -> 
      let num = List.hd stack in 
      eval env (List.tl stack, (state, inputStream, outputStream @ [num])) tail
		| LD x -> eval env ((state x) :: stack, (state, inputStream, outputStream)) tail
		| ST x -> let num = List.hd stack in eval env (List.tl stack, (Expr.update x num state, inputStream, outputStream)) tail
		| LABEL _ -> eval env (stack, (state, inputStream, outputStream)) tail
		| JMP label -> eval env (stack, (state, inputStream, outputStream)) (env#labeled label)
		| CJMP (condition, label) -> 
		  let value::stack' = stack in
		  let x = match condition with
		  | "nz" -> value <> 0
      | "z" -> value = 0 
      | _ -> failwith "CJMP error"
      in
        eval env (stack', (state, inputStream, outputStream)) (if (x) then (env#labeled label) else tail)

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine

let compile p = failwith "Not yet implemented"*)

let env = object
    val mutable label_count = 0
    method gen_label = label_count <- (label_count + 1); "L" ^ string_of_int label_count
end 

let rec compileWithLabels program lastLabel =
	let rec compileExpr = function
	| Expr.Const value -> [CONST value]
	| Expr.Var variable -> [LD variable]
	| Expr.Binop (oper, left, right) -> (compileExpr left) @ (compileExpr right) @ [BINOP oper]
	in
	match program with
	| Stmt.Assign (variable, expr) -> (compileExpr expr) @ [ST variable], false
  | Stmt.Read variable ->  [READ; ST variable], false
  | Stmt.Write expr -> (compileExpr expr) @ [WRITE], false
  | Stmt.Seq (left, right) -> 
    (let newLabel = env#gen_label in
    let (compiledL, lUsedL) = compileWithLabels left newLabel in
    let (compiledR, lUsedR) = compileWithLabels right lastLabel in
    (compiledL @ (if lUsedL then [LABEL newLabel] else []) @ compiledR), lUsedR) 
	| Stmt.Skip -> [], false
	| Stmt.If (expr, left, right) -> 
    let l_else = env#gen_label in
    let (compiledL, lUsedL) = compileWithLabels left lastLabel in
    let (compiledR, lUsedR) = compileWithLabels right lastLabel in
    (compileExpr expr @ 
    [CJMP ("z", l_else)] @ 
    compiledL @ 
    (if lUsedL then [] else [JMP lastLabel]) @ 
    [LABEL l_else] @ 
    compiledR @ 
    (if lUsedR then [] else [JMP lastLabel])), true
	| Stmt.While (expr, statement) ->
    let l_end = env#gen_label in
    let l_loop = env#gen_label in
    let (body, _) = compileWithLabels statement l_end in
    ([JMP l_end] @ 
    [LABEL l_loop] @ 
    body @ 
    [LABEL l_end] @ 
    compileExpr expr @ 
    [CJMP ("nz", l_loop)]), false
	| Stmt.Repeat (expr, statement) ->
    let l_loop = env#gen_label in
    let (body, _) = compileWithLabels statement lastLabel in 
    ([LABEL l_loop] @ body @ compileExpr expr @ [CJMP ("z", l_loop)]), false
  
let rec compile p =
  let label = env#gen_label in
  let compiled, used = compileWithLabels p label in
  compiled @ (if used then [LABEL label] else [])
