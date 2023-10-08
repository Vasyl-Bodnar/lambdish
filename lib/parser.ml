type variable = string

(* This is essentially the result type, however Ok/Error makes less sense for my usecase *)
type ('a, 'b) either = Left of 'a | Right of 'b

type tok =
  | Def of variable * tok list
  | Fun of variable * tok list
  | Var of variable
  | Parens of tok list
  | Integer of int (* TEMP? *)

open Seq

type setts = { parens : int; lambda : bool }

let default_setts = { parens = 0; lambda = false }
let incr_parens sett = { sett with parens = sett.parens + 1 }
let decr_parens sett = { sett with parens = sett.parens - 1 }

let rec parse sett = function
  | Cons ((' ' | '\n'), cs) -> parse sett (cs ())
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
      | _ -> failwith "Unmatched Parens")
  | Cons (')', cs) when sett.parens > 0 -> ([], Cons (')', cs))
  | Cons (')', _) when sett.parens = 0 -> failwith "Unmatched Parens"
  | Cons ((' ' | '\n'), cs) -> expr sett (cs ())
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
  | Cons ((' ' | '\n'), cs) -> lambda sett (cs ())
  | Cons (c, cs) ->
      let name, body = lambda sett (cs ()) in
      (Cons (c, fun () -> name), body)
  | _ -> failwith "Unfinished Function"

and try_lambda sett = function
  | Cons ('.', cs) -> (Left Nil, cs ())
  | Cons ((('(' | ')') as c), cs) -> (Right Nil, Cons (c, cs))
  | Cons ((' ' | '\n'), cs) -> (Right Nil, cs ())
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
  | Cons ((' ' | '\n'), cs) -> (
      match (drop_while (function ' ' | '\n' -> true | _ -> false) cs) () with
      | Cons (':', pc) -> (
          match pc () with
          | Cons ('=', cs) -> (Left Nil, cs ())
          | _ -> (Right Nil, cs ()))
      | _ -> (Right Nil, cs ()))
  | Cons (c, cs) -> try_match c cs
  | Nil -> (Right Nil, Nil)

and integer sett = function
  | Cons ((('(' | ')') as c), cs) -> (Nil, Cons (c, cs))
  | Cons ((' ' | '\n'), cs) -> (Nil, cs ())
  | Cons (('0' .. '9' as c), cs) ->
      let intg, rest = integer sett (cs ()) in
      (Cons (int_of_char c - 48, fun () -> intg), rest)
  | Nil -> (Nil, Nil)
  | _ -> failwith "Bad Integer"

(* let test str = lex default_setts (String.to_seq str ()) *)