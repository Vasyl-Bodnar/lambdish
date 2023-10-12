open Js_of_ocaml
module Html = Dom_html
open Lambdish.Parser
open Lambdish.Interpreter

let string_run modl str =
  try
    parse default_setts (String.to_seq str ())
    |> interpret modl |> List.map tok_to_str |> String.concat " "
  with
  | Unbalanced_parens -> "Unbalanced Parenthesis"
  | Unfinished_fun -> "Unfinished Function"
  | Unfinished_str -> "Unfinished String"
  | Bad_int -> "Bad Number"
  | Undefined_var var -> "Undefined Variable: " ^ var
  | _ -> "Unknown Error"

let make_div d parent text =
  let div = Html.createDiv d in
  div##.innerHTML := text;
  Dom.appendChild parent div

let std_lib : string =
  {|
  (S := \x.y.z.x z(y z))
  (K := \x.y.x)
  (I := \x.x)

  (true := K)
  (false := S K)

  (cons := \f.x.y.f x y)
  (nil := \x.K)
  (car := \p.p K)
  (cdr := \p.p S K)
|}

let onload _ =
  let d = Html.document in
  let hist =
    Js.Opt.get
      (d##getElementById (Js.string "history"))
      (fun () -> assert false)
  in
  let interp =
    Js.Opt.get
      (d##getElementById (Js.string "interpreter"))
      (fun () -> assert false)
  in
  interp##focus;
  (* Initialize Empty Module and Import List Module *)
  let modl = default_modl in
  let _ = string_run modl std_lib in
  let match_key modl e =
    match e##.keyCode with
    | 13 ->
        (match Js.Opt.to_option interp##.textContent with
        | Some s ->
            let res = string_run modl (Js.to_string s) in
            make_div d hist
              ((Js.string "> ")##concat_2 s (Js.string ("<br/>" ^ res)));
            interp##.textContent := Js.Opt.return (Js.string "")
        | None -> ());
        Js._false
    | 76 when Js.to_bool e##.ctrlKey ->
        hist##.textContent := Js.Opt.return (Js.string "");
        Js._false
    | _ -> Js._true
  in
  ignore
  @@ Html.addEventListener interp Html.Event.keydown
       (Html.handler (match_key modl))
       Js._true;
  ignore
  @@ Html.addEventListener d Html.Event.mousedown
       (Html.handler (fun _ ->
            interp##focus;
            Js._false))
       Js._true;
  Js._false

let _ = Html.window##.onload := Html.handler onload
