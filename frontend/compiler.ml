(** Main compiler library module. *)
open Thin
open Lexing

type compiler = {
    ifiles   : string list;
    ofile    : string;

    debug    : bool;
    wall     : bool;
}

let version = "0.1.0"
let usage_msg = "entc <file1> [<file2>] ... (FLAGS)"
let help_msg = usage_msg ^ "\n" ^
    "-o [NAME]   --output            Output binary name with path. If this flag is not used, an default 'out' binary will be created in the same folder where the command was executed." ^
    "-v          --version           Prints the current compiler version and exit.\n" ^
    "-d          --debug             Prints all debug messages while compiling.\n" ^
    "-h          --help              Prints this help text and exit.\n" ^
    "-W          --warn-as-errors    Interprets all warnings as errors and exits when such appear."

let entc = {
    ifiles = [];
    ofile = "";

    debug = false;
    wall = false;
}

let ifiles = ref entc.ifiles
let ofile = ref entc.ofile
let debug = ref entc.debug
let wall = ref entc.wall

let anon_fun filename = ifiles := filename :: !ifiles

(* Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
    let pos = lexbuf.lex_curr_p in
    Fmt.str "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf =
    (* catch exception and turn into Error *)
    try Ok (Parser.program Lexer.read_token lexbuf) with
    | Lexer.SyntaxError msg ->
        let error_msg = Fmt.str "%s: %s@." (print_error_position lexbuf) msg in
        Error (Core.Error.of_string error_msg)
    | Parser.Error ->
        let error_msg = Fmt.str "%s: syntax error@." (print_error_position lexbuf) in
        Error (Core.Error.of_string error_msg)

(** Parsing each input file *)
let parse_file filename =
    let chan = open_in_bin filename in
    let lexbuf = from_channel chan in
    let result =
        try parse_program lexbuf with e ->
            close_in_noerr chan;
            raise e
    in
    close_in_noerr chan;
    result

(** Main compilation area. *)
let compile () = 
    let results = List.map parse_file !ifiles in
    List.iter (function
        | Ok ast -> 
            if !debug then 
                Past.PastPP.pprint_program Fmt.stdout ast
        | Error e -> 
            Core.Error.to_string_hum e |> prerr_endline
    ) results
