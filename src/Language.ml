(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators

(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let push_scope st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let drop_scope st st' = {st' with g = st.g}

  end
    
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
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
    
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      parse:
          !(Ostap.Util.expr 
             (fun x -> x)
              (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
                `Lefta, ["!!"];
                `Lefta, ["&&"];
                `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
                `Lefta, ["+" ; "-"];
                `Lefta, ["*" ; "/"; "%"];
              |] 
              )
              primary);
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
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
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters and a body for given definition
    *)
    let rec eval env configuration statement = 
      let (state, inputStream, outputStream) = configuration in
      match statement with
        | Read variable -> (match inputStream with 
          | value::rest -> (State.update variable value state), rest, outputStream
          | _ -> failwith "Input is empty")
        | Write expression -> (state, inputStream, outputStream @ [Expr.eval state expression])
        | Assign (variable, expression) -> (State.update variable (Expr.eval state expression) state), inputStream, outputStream
        | Seq (first, second) -> eval env (eval env configuration first) second
        | Skip -> configuration
        | If (expression, left, right) -> if (Expr.eval state expression) != 0 then eval env (state, inputStream, outputStream) left else eval env configuration right
        | While (expression, stmt) -> if (Expr.eval state expression) != 0 then eval env (eval env configuration stmt) statement else configuration
        | Repeat (stmt, expression) -> let (state', inputStream', outputStream') = eval env configuration stmt in 
          if Expr.eval state' expression == 0 then eval env (state', inputStream', outputStream') statement  else (state', inputStream', outputStream')
        | Call (name, expr) ->
          let (arg, locals, body) = env#definition name in
          let expr = List.combine arg (List.map (Expr.eval state) expr) in
          let state' = State.push_scope state (arg @ locals) in
          let fun_env_w_args = List.fold_left (fun state (name, value) -> State.update name value state) state' expr in
          let (new_s, inputStream, outputStream) = eval env (fun_env_w_args,inputStream, outputStream) body in
          (State.drop_scope new_s state, inputStream, outputStream)

    (* Statement parser *)
    ostap (                                      
      stmnt:
      x:IDENT ":=" e:!(Expr.parse)    {Assign (x, e)}
      | "read" "(" x:IDENT ")"         {Read x}
      | "write" "(" e:!(Expr.parse) ")" {Write e}
      | "skip" {Skip}
      | "if" expr:!(Expr.parse) "then" s:parse "fi" {If (expr, s, Skip)}
      | "if" expr:!(Expr.parse) "then" l:parse r:else_or_elif "fi" {If (expr, l, r)}
      | "while" expr:!(Expr.parse) "do" s:parse "od" {While (expr, s)}
      | "for" condition:parse "," expr:!(Expr.parse) "," l:parse "do" r:parse "od" {Seq (condition, While (expr, Seq (r, l)))}
      | "repeat" s:parse "until" expr:!(Expr.parse) {Repeat (s, expr)}
      | name:IDENT "(" args:(!(Expr.parse))* ")" {Call (name, args)};
      else_or_elif: 
        "else" s:parse {s}
      | "elif" expr:!(Expr.parse) "then" l:parse r:else_or_elif {If (expr, l, r)};
      parse: l:stmnt ";" rest:parse {Seq (l, rest)} | stmnt
    )

  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (                                      
      parse: 
          "fun" fun_name:IDENT "(" args: (IDENT)* ")"
          locals: (%"local" (IDENT)*)?
          "{" body: !(Stmt.parse) "}" 
          { 
            let locals = match locals with
            | Some x -> x
            | _ -> [] in
            fun_name, (args, locals, body)
          }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m        = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o  = Stmt.eval (object method definition f = snd @@ M.find f m end) (State.empty, i, []) body in o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
