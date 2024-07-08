(** Main module that starts the compilation pipeline. *)
open Core

(** List of all flags for the compiler *)
let flist = [
    ("-v", (fun () -> print_endline Compiler.version) |> Arg.Unit, "Prints the current compiler version and exit.");
    ("-h", (fun () -> print_endline Compiler.help_msg) |> Arg.Unit, "Prints this help text and exit.");

    ("-o", Arg.Set_string Compiler.ofile, "Output binary name with path. If this flag is not used, a default out binary will be created in the same folder where the command was executed.");
    ("-d", Arg.Set Compiler.debug, "Prints all debug messages while compiling.");
    ("-W", Arg.Set Compiler.wall, "Interprets all warnings as errors and exits when such appear.");
]

(* let get_extension filename = 
    String.split_on_chars filename ~on:['.'] 
    |> List.last 
    |> Option.value ~default:"" *)

let () = Arg.parse flist Compiler.anon_fun Compiler.usage_msg |> Compiler.compile
