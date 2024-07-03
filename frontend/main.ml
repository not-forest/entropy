(* Main module that starts the compilation pipeline. *)

open Core
open Entropy_compiler

let geti_extension filename = 
    String.split_on_chars filename ~on:['.'] |> List.last |> Option.value ~default:""
