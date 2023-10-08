open Printf
open Lambdish.Parser
open Lambdish.Interpreter

let string_run ?(modl = default_modl) str =
  parse default_setts (String.to_seq str ())
  |> interpret modl |> List.map tok_to_str |> String.concat " " |> print_endline

let file_run ?(modl = default_modl) name args =
  let fh = open_in name in
  let rec inn modl =
    try
      string_run ~modl @@ input_line fh;
      inn { modl with line = modl.line + 1 }
    with End_of_file -> ()
  in
  List.iteri (fun i s -> string_run ~modl (sprintf "$%i := %s" i s)) args;
  inn modl

let run libfile =
  let rec inn modl =
    Ocamline.read ~brackets:[ ('(', ')') ] ~strings:[ '"' ] ()
    |> string_run ~modl;
    inn modl
  in
  let modl = default_modl in
  if libfile <> "" then file_run ~modl libfile [] else ();
  inn modl
