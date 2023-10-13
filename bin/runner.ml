open Printf
open Lambdish.Parser
open Lambdish.Interpreter

let string_run ?(print = print_endline) ?(modl = default_modl) str =
  try
    parse default_setts (String.to_seq str ())
    |> interpret modl |> List.map tok_to_str |> String.concat " " |> print
  with
  | Unbalanced_parens -> print_endline "Unbalanced Parenthesis"
  | Unfinished_fun -> print_endline "Unfinished Function"
  | Unfinished_str -> print_endline "Unfinished String"
  | Bad_int -> print_endline "Bad Number"
  | Undefined_var var -> print_endline ("Undefined Variable: " ^ var)
  | _ -> print_endline "Unknown Error"

let file_run ?(modl = default_modl) name args =
  let fh = open_in name in
  let rec inn modl =
    try
      string_run ~print:(function "" -> () | s -> print_endline s) ~modl
      @@ input_line fh;
      inn { modl with line = modl.line + 1 }
    with End_of_file -> ()
  in
  List.iteri
    (fun i s ->
      string_run ~print:(fun _ -> ()) ~modl (sprintf "$%i := \"%s\"" i s))
    (name :: args);
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
