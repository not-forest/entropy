(* This module specifies the structure of the parsed AST *)

open Ast

type identifier = 
    | Variable of Var_name.t 
    | ObjField of Var_name.t * Field_name.t

(* Possible executable expressions - note we pass in the posation of the start token to
   provide useful debugging information - which line + position the parsing errors
   occurred *)
type expr =
    | Char        of pos * char
    | Integer     of pos * int
    | Boolean     of pos * bool
    | Identifier  of pos * identifier
    (* optional type-parameter *)
    | Let         of pos * type_expr option * modifier option * Var_name.t * expr
    (* binds variable to expression (optional type annotation) *)
    | Assign      of pos * identifier * expr
    | Consume     of pos * identifier list
    | MethodApp   of pos * Var_name.t * Method_name.t * expr list (* read as x.m(args) *)
    | FunctionApp of pos * Function_name.t * expr list
    | FinishAsync of pos * async_expr list * block_expr
    (* a list async exprs and the current thread's expr *)
    | If          of pos * expr * block_expr * block_expr option (* If ___ then ___ else ___ *)
    | While       of pos * expr * block_expr (* While ___ do ___ *)
    | For         of pos * expr * expr * expr * block_expr
    (* For(init_expr; cond_expr ; step_expr) body_expr *)
    | BinOp       of pos * bin_op * expr * expr
    | PipeOp      of pos * pipe_op * expr * expr
    | UnOp        of pos * un_op * expr

and block_expr = Block of pos * expr option list

and async_expr = AsyncExpr of block_expr

(* Function defn consists of the function name, return type, the list of params, and the
   body expr of the function *)
type function_defn =
  | TFunction of
      Function_name.t * borrowed_ref option * type_expr * param list * block_expr

(* Method defn consists the method name, return type (and whether it returns a borrowed
   ref), the list of params, the capabilities used and the body expr of the function *)
type method_defn =
  | TMethod of
      Method_name.t
      * borrowed_ref option
      * type_expr
      * param list
      * block_expr

(* Struct definitions consist of the struct name and optionally specifying if generic and if
   it inherits from another struct, its capabilities and the fields and methods in the
   struct *)
type struct_defn =
  | TStruct of
      Struct_name.t
      * generic_type option
      * field_defn list
      * method_defn list

(* Each program defines the structes,followed by functions, followed by the main
   expression to execute. *)
type program = Prog of struct_defn list * function_defn list

module PastPP = struct
open Ast
open Core

let indent_space = "   "

let rec pprint_expr ppf ~indent expr =
let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
let new_indent = indent_space ^ indent in
match expr with
    | Char (_, c) -> print_expr (Fmt.str "Char:%c" c)
    | Integer (_, i) -> print_expr (Fmt.str "Int:%d" i)
    | Boolean (_, b) -> print_expr (Fmt.str "Bool:%b" b)
    | Identifier (_, id) -> (
        match id with
        | Variable var_name -> print_expr (Fmt.str "Variable: %s" (Var_name.to_string var_name))
        | ObjField (var_name, field_name) ->
            print_expr
                (Fmt.str "Objfield: %s.%s" (Var_name.to_string var_name)
                (Field_name.to_string field_name))
        )
    | Let (_, optional_type, optional_mod, var_name, bound_expr) ->
        print_expr (Fmt.str "Let var: %s" (Var_name.to_string var_name)) ;
        ( match optional_type with
            | None            -> ()
            | Some type_annot -> Fmt.pf ppf "Type annotation: %s" (string_of_type type_annot) ) ;
        ( match optional_mod with
            | None -> ()
            | Some modifier -> Fmt.pf ppf "Modifier: %s" (string_of_modifier modifier) ) ;
        pprint_expr ppf ~indent:new_indent bound_expr
    | Assign (loc, id, assigned_expr) ->
        print_expr "Assign" ;
        pprint_expr ppf ~indent:new_indent (Identifier (loc, id)) ;
        pprint_expr ppf ~indent:new_indent assigned_expr
    | Consume (loc, ids) ->
        print_expr "Consume" ;
        List.iter ~f:(fun id -> pprint_expr ppf ~indent:new_indent (Identifier (loc, id))) ids ;
    | FunctionApp (_, func_name, args) ->
        print_expr "Function App" ;
        Fmt.pf ppf "%sFunction: %s@." new_indent (Function_name.to_string func_name) ;
        pprint_args ppf ~indent:new_indent args
    | MethodApp (_, var_name, method_name, args) ->
        print_expr
            (Fmt.str "ObjMethod: %s.%s" (Var_name.to_string var_name)
            (Method_name.to_string method_name)) ;
        pprint_args ppf ~indent:new_indent args
    | FinishAsync (_, async_exprs, curr_thread_expr) ->
        print_expr "Finish async" ;
        List.iter ~f:(pprint_async_expr ppf ~indent:(indent_space ^ new_indent)) async_exprs ;
        pprint_block_expr ppf ~indent:new_indent ~block_name:"Current thread"
        curr_thread_expr
    | If (_, cond_expr, then_expr, else_expr) ->
        print_expr "If" ;
        pprint_expr ppf ~indent:new_indent cond_expr ;
        pprint_block_expr ppf ~indent:new_indent ~block_name:"Then" then_expr ;
        (match else_expr with
            | Some else_expr ->
                pprint_block_expr ppf ~indent:new_indent ~block_name:"Else" else_expr ;
            | None -> ())
    | While (_, cond_expr, loop_expr) ->
        print_expr "While" ;
        pprint_expr ppf ~indent:new_indent cond_expr ;
        pprint_block_expr ppf ~indent:new_indent ~block_name:"Body" loop_expr
    | For (_, start_expr, cond_expr, step_expr, loop_expr) ->
        print_expr "For" ;
        pprint_expr ppf ~indent:new_indent start_expr ;
        pprint_expr ppf ~indent:new_indent cond_expr ;
        pprint_expr ppf ~indent:new_indent step_expr ;
        pprint_block_expr ppf ~indent:new_indent ~block_name:"Body" loop_expr
    | BinOp (_, bin_op, expr1, expr2) ->
        print_expr (Fmt.str "Bin Op: %s" (string_of_bin_op bin_op)) ;
        pprint_expr ppf ~indent:new_indent expr1 ;
        pprint_expr ppf ~indent:new_indent expr2
    | PipeOp (_, pipe_op, expr1, expr2) ->
        print_expr (Fmt.str "Pipe Op: %s" (string_of_pipe_op pipe_op)) ;
        pprint_expr ppf ~indent:new_indent expr1 ;
        pprint_expr ppf ~indent:new_indent expr2
    | UnOp (_, un_op, expr) ->
        print_expr (Fmt.str "Unary Op: %s" (string_of_un_op un_op)) ;
        pprint_expr ppf ~indent:new_indent expr


and pprint_args ppf ~indent = function
  | []   -> Fmt.pf ppf "%s()@." indent
  | args -> List.iter ~f:(pprint_expr ppf ~indent) args

and pprint_block_expr ppf ~indent ~block_name (Block (_, exprs)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s%s block@." indent block_name ;
  List.iter exprs ~f:(function
    | Some e -> pprint_expr ppf ~indent:new_indent e
    | None -> ())

and pprint_async_expr ppf ~indent (AsyncExpr block_expr) =
  pprint_block_expr ppf ~indent ~block_name:"Async Expr" block_expr

let pprint_function_defn ppf ~indent
    (TFunction (func_name, maybe_borrowed_ret_ref, return_type, params, body_expr)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s Function: %s@." indent (Function_name.to_string func_name) ;
  Fmt.pf ppf "%s Return type: %s%s@." new_indent
    (string_of_maybe_borrowed_ref maybe_borrowed_ret_ref)
    (string_of_type return_type) ;
  AstPP.pp ppf ~indent:new_indent params ;
  pprint_block_expr ppf ~indent:new_indent ~block_name:"Body" body_expr

let pprint_method_defn ppf ~indent
    (TMethod
      ( method_name
      , maybe_borrowed_ret_ref
      , return_type
      , params
      , body_expr )) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s Method: %s@." indent (Method_name.to_string method_name) ;
  Fmt.pf ppf "%s Return type: %s%s@." new_indent
    (string_of_maybe_borrowed_ref maybe_borrowed_ret_ref)
    (string_of_type return_type) ;
  AstPP.pp ppf ~indent:new_indent params ;
  pprint_block_expr ppf ~indent:new_indent ~block_name:"Body" body_expr

let pprint_struct_defn ppf ~indent
    (TStruct
      ( struct_name
      , maybe_generic
      , field_defns
      , method_defns )) =
  Fmt.pf ppf "%sStruct: %s%s." indent
    (Struct_name.to_string struct_name)
    (string_of_maybe_generic maybe_generic) ;
  let new_indent = indent_space ^ indent in
  List.iter ~f:(AstPP.pprint_field_defn ppf ~indent:new_indent) field_defns ;
  List.iter ~f:(pprint_method_defn ppf ~indent:new_indent) method_defns

let pprint_program ppf (Prog (struct_defns, function_defns)) =
  Fmt.pf ppf "Program@." ;
  let indent = "└──" in
  List.iter ~f:(pprint_struct_defn ppf ~indent) struct_defns ;
  List.iter ~f:(pprint_function_defn ppf ~indent) function_defns
end
