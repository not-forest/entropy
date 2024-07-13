(** Entropy's Abstract Syntax Tree definitions *)

open Base

(** This global counter is used for creating unique anonymous types *)
let anonym_counter = ref 0

let next_anonym_name () =
    Core.incr anonym_counter;
    "ANONYM_" ^ Core.string_of_int !anonym_counter

let name_or_anonym = function
    | Some name -> name
    | None -> next_anonym_name ()

type pos = Lexing.position

let string_of_pos pos =
    Fmt.str "Line:%d Position:%d" pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

module type ID = sig
    type t

    val of_string : string -> t
    val to_string : t -> string
    val ( = ) : t -> t -> bool
end

module String_id = struct
    type t = string

    let of_string x = x
    let to_string x = x
    let ( = ) = String.( = )
end

module Var_name : ID = String_id
module Struct_name : ID = String_id
module Union_name : ID = String_id
module Field_name : ID = String_id
module Method_name : ID = String_id
module Function_name : ID = String_id

type modifier = MConst | Mmut

let string_of_modifier = function 
    | MConst -> "Const" 
    | Mmut -> "Mut"

let string_of_maybe_modified = function
    | Some modified -> string_of_modifier modified
    | None -> ""

(* determines if a reference is being borrowed for that scope *)
type borrowed_ref = Borrowed

let string_of_maybe_borrowed_ref = function Some Borrowed -> "Borrowed " | None -> ""

(* If struct/enum is type-parameterised *)
type generic_type = Generic of char list

let string_of_maybe_generics = function 
    | Some Generic c -> String.concat ~sep:", " (List.map ~f:(String.make 1) c)
    | None -> ""

type type_expr =
    | TEu8 | TEu16 | TEu32 | TEu64 | TEu128
    | TEi8 | TEi16 | TEi32 | TEi64 | TEi128
    | TEDiverge
    | TEBool
    | TEEmpty
    | TEGeneric of char
    (** optionally specify type parameters *)
    | TEstruct of Struct_name.t * type_expr option
    | TEunion of Union_name.t * type_expr option 

let rec string_of_type = function
    | TEu8 -> "u8" | TEu16 -> "u16" | TEu32 -> "u32" | TEu64 -> "u64" | TEu128 -> "u128"
    | TEi8 -> "i8" | TEi16 -> "i16" | TEi32 -> "i32" | TEi64 -> "i64" | TEi128 -> "i128"
    | TEDiverge -> "~"
    | TEEmpty -> "Empty"
    | TEBool -> "Bool"
    | TEGeneric c -> String.make 1 c

    | TEunion (union_name, maybe_type_param) ->
        let maybe_type_param_str =
            match maybe_type_param with
            | Some type_param -> Fmt.str "<%s>" (string_of_type type_param)
            | None            -> "" in
        Fmt.str "%s%s" (Union_name.to_string union_name) maybe_type_param_str
    | TEstruct (struct_name, maybe_type_param) ->
        let maybe_type_param_str =
            match maybe_type_param with
            | Some type_param -> Fmt.str "<%s>" (string_of_type type_param)
            | None            -> "" in
        Fmt.str "%s%s" (Struct_name.to_string struct_name) maybe_type_param_str

type field_defn = TField of modifier option * type_expr option * Field_name.t

type param =
    | TParam of type_expr option * Var_name.t * borrowed_ref option * modifier option

let get_params_types params =
    List.map ~f:(fun (TParam (param_type, _, _, _)) -> param_type) params

(* BINARY OPERATORS *)

type bin_op =
    | BinOpPlus | BinOpMinus
    | BinOpMult | BinOpIntDiv | BinOpRem
    | BinOpPower | BinOpRoot
    | BinOpLessThan | BinOpLessThanEq | BinOpGreaterThan | BinOpGreaterThanEq
    | BinOpAnd | BinOpOr | BinOpLOr | BinOpLAnd
    | BinOpEq | BinOpNotEq

let string_of_bin_op = function
    | BinOpPlus          -> "+"
    | BinOpMinus         -> "-"
    | BinOpMult          -> "*"
    | BinOpIntDiv        -> "/"
    | BinOpRem           -> "%"
    | BinOpPower         -> "^"
    | BinOpRoot          -> "-/"
    | BinOpLessThan      -> "<"
    | BinOpLessThanEq    -> "<="
    | BinOpGreaterThan   -> ">"
    | BinOpGreaterThanEq -> ">="
    | BinOpAnd           -> "&"
    | BinOpLAnd          -> "&&"
    | BinOpOr            -> "|"
    | BinOpLOr           -> "||"
    | BinOpEq            -> "=="
    | BinOpNotEq         -> "!="

(* UNARY OPERATIONS *)

type un_op = 
    | UnOpNot | UnOpLNot 
    | UnOpAbs | UnOpNorm
    | UnOpNeg
    | UnOpRet

let string_of_un_op = function 
    | UnOpNot            -> "~" 
    | UnOpLNot           -> "!"
    | UnOpAbs            -> "|x|"
    | UnOpNorm           -> "||x||"
    | UnOpNeg            -> "-"
    | UnOpRet            -> "!<"

(* PIPE OPERATIONS *)
type pipe_op =
    | PipeOpLeft
    | PipeOpBindLeft
    | PipeOpRight
    | PipeOpBindRight 

let string_of_pipe_op = function
    | PipeOpLeft -> "<<"
    | PipeOpBindLeft -> "<="
    | PipeOpRight -> ">>"
    | PipeOpBindRight -> "=>"

(* Exceptions *)
exception NotDesugaredGenericType of string

(* Thrown if a later compiler stage encounters generic types when it expects it to be
   desugared *)

(* Pretty-print functions *)

module AstPP = struct
    open Core
    let indent_space = "    "

    let string_of_maybe_type = function
        | Some te -> string_of_type te
        | None -> "None"

    let pprint_modifier ppf ~indent modifier =
        Fmt.pf ppf "%sModifier: %s@." indent (string_of_maybe_modified modifier)

    let pprint_type_expr ppf ~indent type_expr =
        Fmt.pf ppf "%sType expr: %s@." indent (string_of_type type_expr)

    let pprint_maybe_type_expr ppf ~indent maybe_type_expr =
        Fmt.pf ppf "%sType expr: %s@." indent (string_of_maybe_type maybe_type_expr)

    let pprint_field_defn ppf ~indent (TField (modifier, type_field, field_name)) =
        Fmt.pf ppf "%sField Defn: %s@." indent (Field_name.to_string field_name);
        let new_indent = indent_space ^ indent in
        pprint_modifier ppf ~indent:new_indent modifier;
        pprint_maybe_type_expr ppf ~indent:new_indent type_field

    let pprint_param ppf ~indent = function
        | TParam (type_expr, param_name, maybe_borrowed, maybe_modified) ->
            Fmt.pf ppf "%s%sParam: %s%s@." indent
                (string_of_maybe_borrowed_ref maybe_borrowed)
                (Var_name.to_string param_name)
                (string_of_maybe_modified maybe_modified);
            let new_indent = indent_space ^ indent in
            pprint_maybe_type_expr ppf ~indent:new_indent type_expr

    let pp ppf ~indent params =
        match params with
        | [] -> Fmt.pf ppf "%sParam: %s@." indent (string_of_type TEEmpty)
        | params -> List.iter params ~f:(pprint_param ppf ~indent)
end




