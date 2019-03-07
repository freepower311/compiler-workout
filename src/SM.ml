open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
 let evaluateInstr stConfiguration instruction =
  let (stack, configuration) = stConfiguration in
  let (state, inputStream, outputStream) = configuration in
  match instruction with
    | BINOP operation -> (match stack with
      | y::x::rest  -> [(Language.Expr.evaluateOperation operation) x y] @ rest , configuration
      )
    | CONST value -> [value] @ stack, configuration
    | READ -> (match inputStream with 
      | input::rest  -> [input] @ stack, (state, rest , outputStream)
      )
    | WRITE -> (match stack with 
      | value::rest  -> rest , (state, inputStream, outputStream @ [value])
      )
    | LD variable -> ([state variable] @ stack, configuration)
    | ST variable -> (match stack with 
      | value::rest  -> (rest , (Language.Expr.update variable value state, inputStream, outputStream))
      )

let eval configuration program = List.fold_left evaluateInstr configuration program

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compileExpression expression = match expression with
 | Language.Expr.Const value -> [CONST value]
 | Language.Expr.Var variable -> [LD variable]
 | Language.Expr.Binop (operator, left, right) -> (compileExpression left) @ (compileExpression right) @ [BINOP operator];;

let rec compile statement = match statement with
 | Language.Stmt.Read variable -> [READ; ST variable]
 | Language.Stmt.Write expression -> (compileExpression expression) @ [WRITE]
 | Language.Stmt.Assign (variable, expression) -> (compileExpression expression) @ [ST variable]
 | Language.Stmt.Seq (first, second) -> (compile first) @ (compile second)
