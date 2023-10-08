open Runner

let usage_msg = "lambdish [(-e <expression>) | -f <filename> [args...]]"
let libfile = ref ""
let filename = ref ""
let sentence = ref ""
let args = ref []
let anon_fun name = libfile := name

let speclist =
  [
    ( "-f",
      Arg.Tuple
        [
          Arg.Set_string filename;
          (let _ = args := [ !filename ] in
           Arg.Rest (fun s -> args := s :: !args));
        ],
      "Interpret the file with args, useful for scripts" );
    ("-e", Arg.Set_string sentence, "Interpret the given expression");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  match (!filename, !sentence) with
  | "", "" -> run !libfile
  | name, "" -> file_run name !args
  | "", sentn -> string_run sentn
  | _, _ -> print_endline "Cannot have both -e and filename"
