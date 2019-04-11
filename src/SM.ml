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
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL  of string * int * bool
(* returns from a function         *) | RET   of bool with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                                                  
          
let rec eval env ((cstack, stack, ((st, i, o) as c)) as conf) = function
  | [] -> conf
  | instruction :: tail ->
    match instruction with
    | BINOP op ->
      (match stack with
      | y :: x :: rest ->
          eval env (cstack, (Expr.to_func op x y) :: rest, c) tail
      | _ -> failwith "BINOP error")
    | CONST v -> eval env (cstack, v :: stack, c) tail
    | READ -> eval env (cstack, (List.hd i) :: stack, (st, List.tl  i, o)) tail
    | WRITE -> eval env (cstack, List.tl stack, (st, i, o @ [List.hd stack])) tail
    | LD x -> eval env (cstack, (State.eval st x) :: stack, c) tail
    | ST x -> eval env (cstack, List.tl stack, (Language.State.update x (List.hd stack) st,  i, o)) tail
    | LABEL l -> eval env conf tail
    | JMP l -> eval env conf (env#labeled l)
    | CJMP (condition, label) -> 
      let value::stack' = stack in
      let x = match condition with
      | "nz" -> value <> 0
      | "z" -> value = 0 
      | _ -> failwith "CJMP error" in
      eval env (cstack, stack', (st, i, o)) (if (x) then (env#labeled label) else tail)
    | CALL (fun_name, _, _) -> eval env (((tail, st)::cstack), stack, c) (env#labeled fun_name)
    | BEGIN (_, fun_params, fun_locals) ->
      let assign_val = fun x ((v :: stack), st) -> (stack, State.update x v st) in
      let (stack', st') = List.fold_right assign_val fun_params (stack, State.enter st (fun_params @ fun_locals)) in
      eval env (cstack, stack', (st', i, o)) tail
    | END | RET _ ->
      match cstack with
      | (tail, old_s)::cstack -> eval env (cstack, stack, (Language.State.leave st old_s, i, o)) tail
      | _ -> conf
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
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)

let label_generator = object
    val mutable fLabel = 0
    method getLabel = fLabel <- fLabel + 1; "label" ^ string_of_int fLabel
end

let rec c_expr expr =
  match expr with
  | Expr.Var x -> [LD x]
  | Expr.Const c -> [CONST c]
  | Expr.Binop (op, e1, e2) -> (c_expr e1) @ (c_expr e2) @ [BINOP op]
  | Expr.Call (name, args) ->
    List.concat (List.map c_expr args) @ [CALL (name, List.length args, false)]

let rec c_stm stm =
  match stm with
  | Stmt.Assign (x, e) -> (c_expr e) @ [ST x]
  | Stmt.Read x -> [READ] @ [ST x]
  | Stmt.Write e -> (c_expr e) @ [WRITE]
  | Stmt.Seq (s1, s2) -> (c_stm s1) @ (c_stm s2)
  | Stmt.Skip -> []
  | Stmt.Call (name, args) ->
    List.concat (List.map c_expr (List.rev args)) @ [CALL (name, List.length args, false)]
  | Stmt.If (e, s1, s2) ->
      let l_else = label_generator#getLabel in
      let l_fi = label_generator#getLabel in
      (c_expr e) @ [CJMP ("z", l_else)] @ (c_stm s1) @ [JMP l_fi; LABEL l_else] @ (c_stm s2) @ [LABEL l_fi]
  | Stmt.While (e, s) ->
      let l_expr = label_generator#getLabel in
      let l_od = label_generator#getLabel in
      [LABEL l_expr] @ (c_expr e) @ [CJMP ("z", l_od)] @ (c_stm s) @ [JMP l_expr; LABEL l_od]
  | Stmt.Repeat (e, s) ->
      let l_repeat = label_generator#getLabel in
      [LABEL l_repeat] @ (c_stm s) @ (c_expr e) @ [CJMP ("z", l_repeat)]
  | Stmt.Return opt_res ->
      begin
        match opt_res with
        | Some res -> (c_expr res) @ [RET true]
        | _ -> [RET false]
      end

let rec compile_def (name, (params, locals, body)) =
  [LABEL name; BEGIN (name, params, locals)] @ c_stm body @ [END]

let compile (defs, p) =
  c_stm p @ [END] @ List.concat (List.map compile_def defs)

