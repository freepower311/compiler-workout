(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)

    let intToBool value = value != 0

    let boolToInt value = if value then 1 else 0

    let evaluateOperation operator left right = match operator with
      | "+" -> left + right
      | "-" -> left - right
      | "*" -> left * right
      | "/" -> left / right
      | "%" -> left mod right
      | "<" -> boolToInt (left < right)
      | ">" -> boolToInt (left > right)
      | "<=" -> boolToInt (left <= right)
      | ">=" -> boolToInt (left >= right)
      | "==" -> boolToInt (left == right)
      | "!=" -> boolToInt (left != right)
      | "&&" -> boolToInt (intToBool left && intToBool right)
      | "!!" -> boolToInt (intToBool left || intToBool right)

    (* Expression evaluator
        val eval : state -> expr -> int
    
      Takes a state and an expression, and returns the value of the expression in 
      the given state.
    *)

    let rec eval state expression = match expression with
      | Const value -> value
      | Var variable -> state variable
      | Binop(operator, left, right) -> evaluateOperation operator (eval state left) (eval state right)

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval configuration statement = 
      let (state, inputStream, outputStream) = configuration in
      match statement with
        | Read variable -> (match inputStream with 
          | value::rest -> (Expr.update variable value state), rest, outputStream
          | [] -> failwith "Input is empty")
        | Write expression -> (state, inputStream, Expr.eval state expression :: outputStream)
        | Assign (variable, expression) -> (Expr.update variable (Expr.eval state expression) state), inputStream, outputStream
        | Seq (first, second) -> eval (eval configuration first) second                                                    
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
