open Parser

type modl = { defs : (variable, tok list) Hashtbl.t; line : int }

let default_modl = { defs = Hashtbl.create 20; line = 0 }

let rec interpret modl toks =
  eval modl toks |> fun res -> if res = toks then res else interpret modl res

and eval modl = function
  | Def (name, body) :: toks ->
      Hashtbl.replace modl.defs name body;
      eval modl toks
  | Fun (var, body) :: tok :: toks -> apply modl var body tok @ eval modl toks
  | Parens ptoks :: toks -> eval modl ptoks @ eval modl toks
  | Var var :: toks -> resolve modl var :: eval modl toks
  | Integer intg :: toks -> Integer intg :: eval modl toks
  | t -> t

and apply modl var body = function
  | Var bound_var -> eval modl @@ substitute var (resolve modl bound_var) body
  | tok -> eval modl @@ substitute var (rename modl var tok) body

and substitute var tok toks =
  List.map
    (function
      | Var v when v = var -> tok
      | Fun (v, body) when v <> var -> Fun (v, substitute var tok body)
      | Parens body -> Parens (substitute var tok body)
      | a -> a)
    toks

and resolve modl var =
  match Hashtbl.find_opt modl.defs var with
  | Some value -> Parens value
  | None -> failwith @@ "Undefined Variable: " ^ var

and rename modl var = function
  | Fun (var2, body) ->
      Fun
        ( (if var = var2 then var2 ^ "0" else var2),
          List.map (rename modl var) body )
  | Var var2 when var = var2 -> Var (var2 ^ "0")
  | Parens ptoks -> Parens (List.map (rename modl var) ptoks)
  | a -> a

(* let test str = *)
(*   parse default_setts ((String.to_seq str) ()) |> interpret default_modl *)
