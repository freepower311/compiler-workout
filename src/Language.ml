(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
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
      | _ -> failwith "Unknown operator"
    
    let rec eval state expression = match expression with
    | Const value -> value
    | Var variable -> state variable
    | Binop(operator, left, right) -> evaluateOperation operator (eval state left) (eval state right)

    let prsBinOp op = ostap(- $(op)), (fun l r -> Binop (op, l, r))

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      expr:
      !(Ostap.Util.expr
        (fun x -> x)
          (Array.map (fun (asc, ops) -> asc, List.map prsBinOp ops)
            [|
                `Lefta, ["!!"];
                `Lefta, ["&&"];
                `Nona , ["<="; "<"; ">="; ">"; "=="; "!="];
                `Lefta, ["+"; "-"];
                `Lefta, ["*"; "/"; "%"];
            |]
          )
        primary
      );
      primary: v:IDENT {Var v} | c:DECIMAL {Const c} | -"(" expr -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of Expr.t * t  with show
                                                                    
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
          | _ -> failwith "Input is empty")
        | Write expression -> (state, inputStream, outputStream @ [Expr.eval state expression])
        | Assign (variable, expression) -> (Expr.update variable (Expr.eval state expression) state), inputStream, outputStream
        | Seq (first, second) -> eval (eval configuration first) second
        | Skip -> configuration
        | If (expression, left, right) -> if (Expr.eval state expression) != 0 then eval (state, inputStream, outputStream) left else eval configuration right
        | While (expression, stmt) -> if (Expr.eval state expression) != 0 then eval (eval configuration stmt) statement else configuration
        | Repeat (expression, stmt) -> let (state', inputStream', outputStream') = eval configuration stmt in 
          if Expr.eval state' expression == 0 then eval (state', inputStream', outputStream') statement  else (state', inputStream', outputStream')
                               
    (* Statement parser *)
    ostap (
      stmnt:
      x:IDENT ":=" e:!(Expr.expr)         {Assign (x, e)}
      | "read"  "("  x:IDENT        ")"   {Read x}
      | "write" "("  e:!(Expr.expr) ")"   {Write e}
      | "skip" {Skip}
      | "if" expr:!(Expr.expr) "then" s:parse "fi" {If (expr, s, Skip)}
      | "if" expr:!(Expr.expr) "then" l:parse r:else_or_elif "fi" {If (expr, l, r)}
      | "while" expr:!(Expr.expr) "do" s:parse "od" {While (expr, s)}
      | "for" condition:parse "," expr:!(Expr.expr) "," l:parse "do" r:parse "od" {Seq (condition, While (expr, Seq (r, l)))}
      | "repeat" s:parse "until" expr:!(Expr.expr) {Repeat (expr, s)};
      else_or_elif: 
        "else" s:parse {s}
      | "elif" expr:!(Expr.expr) %"then" l:parse r:else_or_elif {If (expr, l, r)};

      parse: l:stmnt ";" rest:parse {Seq (l, rest)} | stmnt
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
