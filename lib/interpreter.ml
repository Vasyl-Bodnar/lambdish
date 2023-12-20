open Parser

exception Undefined_var of variable

type modl = { defs : (variable, tok list) Hashtbl.t; line : int }

let default_modl = { defs = Hashtbl.create 20; line = 0 }

let rec substitute var tok toks =
  List.map
    (function
      | Var v when v = var -> tok
      | Fun (v, body) when v <> var -> Fun (v, substitute var tok body)
      | Parens body -> Parens (substitute var tok body)
      | a -> a)
    toks

let resolve modl var =
  match Hashtbl.find_opt modl.defs var with
  | Some value -> Parens value
  | None -> raise (Undefined_var var)

let rec rename modl var = function
  | Fun (var2, body) ->
      Fun
        ( (if var = var2 then var2 ^ "0" else var2),
          List.map (rename modl var) body )
  | Var var2 when var = var2 -> Var (var2 ^ "0")
  | Parens ptoks -> Parens (List.map (rename modl var) ptoks)
  | a -> a

let apply modl var body = function
  | Var bound_var -> substitute var (resolve modl bound_var) body
  | tok -> substitute var (rename modl var tok) body

let rec int = function
  | 0 -> [ Var "x" ]
  | n -> [ Var "f"; Parens (int (n - 1)) ]

let str s =
  String.to_seq s |> List.of_seq
  |> List.rev_map (fun c -> Integer (int_of_char c))
  |> List.fold_left
       (fun acc i -> Fun ("f", [ Var "f"; i; acc ]))
       (Fun ("x", [ Fun ("x0", [ Fun ("y", [ Var "x0" ]) ]) ]))

let rec eval modl = function
  | Def (name, body) ->
      Hashtbl.replace modl.defs name body;
      []
  | Parens [ Fun (var, body); tok ] -> apply modl var body tok
  | Parens ptoks -> interpret modl ptoks
  | Var var -> [ resolve modl var ]
  | Symb sym -> [ Symb sym ]
  | Integer i -> [ Fun ("f", [ Fun ("x", int i) ]) ]
  | Str s -> [ str s ]
  | t -> [ t ]

and interpret modl = function
  | (Fun (_, _) as f) :: tk :: ts ->
      let res = eval modl @@ Parens [ f; tk ] in
      interpret modl (res @ ts)
  | t :: ts -> (
      match eval modl t with
      | [] -> interpret modl ts
      | [ res ] ->
          if res = t then res :: interpret modl ts
          else interpret modl (res :: ts)
      | res ->
          if res = [ t ] then t :: interpret modl ts
          else interpret modl (res @ ts))
  | [] -> []

(* let test str = *)
(*   parse default_setts ((String.to_seq str) ()) |> interpret default_modl *)
