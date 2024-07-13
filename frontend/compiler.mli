(**  Main compiler library module. *)

(** 
    This module's functions are being called from the main executable file
    generated in main.exe. 
*)
val version     : string
val help_msg    : string
val usage_msg   : string

val ifiles      : string list ref
val ofile       : string ref
val debug       : bool ref
val wall        : bool ref
    
val anon_fun    : string -> unit
val compile     : unit -> unit
val parse_file  : string -> (Thin.Past.program, Base.Error.t) result 
