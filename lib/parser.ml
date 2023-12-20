(* This is essentially the result type, however Ok/Error makes less sense for my usecase *)
type ('a, 'b) either = Left of 'a | Right of 'b
type variable = string

exception Unbalanced_parens
exception Unfinished_fun
exception Unfinished_str
exception Bad_int

type tok =
  | Def of variable * tok list
  | Fun of variable * tok list
  | Var of variable
  | Symb of string
  | Parens of tok list
  | Integer of int
  | Str of string

let rec tok_to_str = function
  | Def (name, toks) ->
      Printf.sprintf "%s := %s" name
        (List.map tok_to_str toks |> String.concat " ")
  | Fun (var, toks) ->
      Printf.sprintf "(λ%s. %s)" var
        (List.map tok_to_str toks |> String.concat " ")
  | Var var -> var
  | Symb sym -> sym
  | Parens toks ->
      Printf.sprintf "(%s)" (List.map tok_to_str toks |> String.concat " ")
  | Integer i -> string_of_int i
  | Str s -> Printf.sprintf "\"%s\"" s

open Seq

type setts = { parens : int; lambda : bool; str : bool }

let default_setts = { parens = 0; lambda = false; str = false }
let incr_parens sett = { sett with parens = sett.parens + 1 }
let decr_parens sett = { sett with parens = sett.parens - 1 }

let rec parse sett = function
  | Cons ((' ' | '\n' | '\r' | '\t'), cs) -> parse sett (cs ())
  | Cons (c, cs) ->
      let toks, rest = expr sett (Cons (c, cs)) in
      toks @ parse sett rest
  | Nil -> []

and expr sett = function
  | Cons ('\\', cs) ->
      let name, (body, rest) = lambda sett (cs ()) in
      ([ Fun (String.of_seq (fun () -> name), body) ], rest)
  | Cons ('(', cs) -> (
      let toks, rest = expr (incr_parens sett) (cs ()) in
      match rest with
      | Cons (')', cs) when sett.parens = 0 -> ([ Parens toks ], cs ())
      | Cons (')', cs) when sett.parens = 1 ->
          let inn_toks, rest = expr sett (cs ()) in
          (Parens toks :: inn_toks, rest)
      | Cons (')', cs) when sett.parens > 1 ->
          let inn_toks, rest = expr (decr_parens sett) (cs ()) in
          (Parens toks :: inn_toks, rest)
      | _ -> raise Unbalanced_parens)
  | Cons (')', cs) when sett.parens > 0 -> ([], Cons (')', cs))
  | Cons (')', _) when sett.parens = 0 -> raise Unbalanced_parens
  | Cons ((' ' | '\n' | '\r' | '\t'), cs) -> expr sett (cs ())
  | Cons ('"', cs) ->
      let str, rest = str sett (cs ()) in
      let toks, rest = expr sett rest in
      (Str (String.of_seq (fun () -> str)) :: toks, rest)
  | Cons ('\'', cs) ->
      let sym, rest = symb sett (cs ()) in
      let toks, rest = expr sett rest in
      (Symb (String.of_seq (fun () -> sym)) :: toks, rest)
  | Cons (('0' .. '9' as c), cs) ->
      let intg, rest = integer sett (Cons (c, cs)) in
      let intg_len = length (fun () -> intg) in
      let toks, rest = expr sett rest in
      ( Integer
          (fold_lefti
             (fun acc i x ->
               acc
               + (x * (int_of_float @@ (10. ** float_of_int (intg_len - i - 1)))))
             0
             (fun () -> intg))
        :: toks,
        rest )
  | Cons (c, cs) when sett.lambda -> (
      match try_lambda sett (Cons (c, cs)) with
      | Left lamb, rest ->
          let body, rest = expr sett rest in
          ([ Fun (String.of_seq (fun () -> lamb), body) ], rest)
      | Right str, rest ->
          let toks, rest = expr sett rest in
          (Var (String.of_seq (fun () -> str)) :: toks, rest))
  | Cons (c, cs) -> (
      match try_def sett (Cons (c, cs)) with
      | Left def, rest ->
          let body, rest = expr sett rest in
          ([ Def (String.of_seq (fun () -> def), body) ], rest)
      | Right str, rest ->
          let toks, rest = expr sett rest in
          (Var (String.of_seq (fun () -> str)) :: toks, rest))
  | Nil -> ([], Nil)

and lambda sett = function
  | Cons ('.', cs) -> (Nil, expr { sett with lambda = true } (cs ()))
  | Cons ((' ' | '\n' | '\r' | '\t'), cs) -> lambda sett (cs ())
  | Cons (c, cs) ->
      let name, body = lambda sett (cs ()) in
      (Cons (c, fun () -> name), body)
  | _ -> raise Unfinished_fun

and try_lambda sett = function
  | Cons ('.', cs) -> (Left Nil, cs ())
  | Cons ((('(' | ')') as c), cs) -> (Right Nil, Cons (c, cs))
  | Cons ((' ' | '\n' | '\r' | '\t'), cs) -> (Right Nil, cs ())
  | Cons (c, cs) -> (
      let resl, rest = try_lambda sett (cs ()) in
      match resl with
      | Left lamb -> (Left (Cons (c, fun () -> lamb)), rest)
      | Right str -> (Right (Cons (c, fun () -> str)), rest))
  | Nil -> (Right Nil, Nil)

and try_def sett =
  let try_match c cs =
    let resl, rest = try_def sett (cs ()) in
    match resl with
    | Left def -> (Left (Cons (c, fun () -> def)), rest)
    | Right str -> (Right (Cons (c, fun () -> str)), rest)
  in
  function
  | Cons (':', cs) -> (
      match cs () with
      | Cons ('=', cs) -> (Left Nil, cs ())
      | _ -> try_match ':' cs)
  | Cons ((('(' | ')') as c), cs) -> (Right Nil, Cons (c, cs))
  | Cons ((' ' | '\n' | '\r' | '\t'), cs) -> (
      match
        (drop_while
           (function ' ' | '\n' | '\r' | '\t' -> true | _ -> false)
           cs)
          ()
      with
      | Cons (':', pc) -> (
          match pc () with
          | Cons ('=', cs) -> (Left Nil, cs ())
          | _ -> (Right Nil, cs ()))
      | _ -> (Right Nil, cs ()))
  | Cons (c, cs) -> try_match c cs
  | Nil -> (Right Nil, Nil)

and integer sett = function
  | Cons ((('(' | ')') as c), cs) -> (Nil, Cons (c, cs))
  | Cons ((' ' | '\n' | '\r' | '\t'), cs) -> (Nil, cs ())
  | Cons (('0' .. '9' as c), cs) ->
      let intg, rest = integer sett (cs ()) in
      (Cons (int_of_char c - 48, fun () -> intg), rest)
  | Nil -> (Nil, Nil)
  | _ -> raise Bad_int

and str sett = function
  | Cons ('"', cs) -> (Nil, cs ())
  | Cons (c, cs) ->
      let str, rest = str sett (cs ()) in
      (Cons (c, fun () -> str), rest)
  | Nil -> raise Unfinished_str

and symb sett = function
  | Cons ((' ' | '\n' | '\r' | '\t'), cs) -> (Nil, cs ())
  | Cons (c, cs) ->
      let sym, rest = symb sett (cs ()) in
      (Cons (c, fun () -> sym), rest)
  | Nil -> (Nil, Nil)

(* let test str = lex default_setts (String.to_seq str ()) *)
