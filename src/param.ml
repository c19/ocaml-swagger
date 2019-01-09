open Printf
open Util

type t = Swagger_j.parameter

let rec item_kind_to_string (items : Swagger_j.items option) = function
  | `String  -> "string"
  | `Number  -> "float"
  | `Integer -> "int"
  | `Boolean -> "bool"
  | `Array   ->
      let open Swagger_j in
      match items with
      | Some is -> item_kind_to_string is.items is.kind ^ " list"
      | None -> failwith ("item_kind_to_string: array type must have an "
                          ^ "'items' field")

let kind_to_string (p : t) =
  match some p.kind with
  | `String  -> "string"
  | `Number  -> "float"
  | `Integer -> "int"
  | `Boolean -> "bool"
  | `File -> "file"
  | `Array   ->
      let open Swagger_j in
      match p.items with
      | Some items -> item_kind_to_string items.items items.kind ^ " array"
      | None -> failwith ("Param.kind_to_string: array type must have an "
                          ^ "'items' field")

module SS = Set.Make(String)

let reserved_words = List.fold_right SS.add 
["and"; "as"; "assert"; "asr"; "begin"; "class";
 "constraint"; "do"; "done"; "downto"; "else";
 "end"; "exception"; "external"; "false"; "for";
 "fun"; "function"; "functor"; "if"; "in"; "include";
 "inherit"; "initializer"; "land"; "lazy"; "let";
 "lor"; "lsl"; "lsr"; "lxor"; "match"; "method";
 "mod"; "module"; "mutable"; "new"; "nonrec";
 "object"; "of"; "open"; "or"; "private"; "rec";
 "sig"; "struct"; "then"; "to"; "true"; "try";
 "type"; "val"; "virtual"; "when"; "while"; "with";
 "parser"; "value"] SS.empty

let is_keyword elt = SS.exists ((=) (String.lowercase_ascii elt)) reserved_words

let name n =
  let n =
    if n.[0] = '$' then String.sub n 1 (String.length n - 1)
    else n in
  let n = snake_case n |> String.lowercase_ascii in
  if is_keyword n then n ^ "_"
  else n

(* Unused values. *)
[@@@ocaml.warning "-32"]

let prefix_strings required =
  if required
  then ("", "~")
  else ("?", "?")

let kind = function
  | true -> `Named
  | false -> `Optional

[@@@end]

let string_of_location = function
  | `Query    -> "query"
  | `Header   -> "header"
  | `Path     -> "path"
  | `FormData -> "formData"
  | `Body     -> "body"

let create ?(duplicate = false) ~reference_base ~reference_root (p : t) =
  let t =
    match p.location with
    | `Body ->
        Schema.create ~reference_base ~reference_root (some p.schema)
        |> Schema.to_string
    | _     ->
        kind_to_string p in
  let n =
    let n = name p.name in
    let loc = string_of_location p.location in
    if duplicate && n <> loc
    then sprintf "%s_%s" loc n
    else n in
  let descr = p.description in
  let create_sig, create_impl =
    if p.required then Val.Sig.named, Val.Impl.named
    else Val.Sig.optional, Val.Impl.optional in
  (create_sig ?descr n t, create_impl n t ~origin:(Val.Impl.origin p))
