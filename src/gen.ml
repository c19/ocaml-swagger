open Printf

let unsnoc l =
  let rec go acc = function
    | [] -> None
    | [x] -> Some (List.rev acc, x)
    | x::xs -> go (x::acc) xs in
  go [] l

let some = function
  | Some x -> x
  | None -> failwith "some: None"

let default z = function
  | Some x -> x
  | None -> z

let snake_case =
  let re1 = Re_pcre.regexp "([A-Z]+)([A-Z][a-z]{2,})" in
  let re2 = Re_pcre.regexp "([a-z0-9])([A-Z])" in
  let re3 = Re.compile (Re_pcre.re "-") in
  let underscore re s =
    let replace groups = sprintf "%s_%s" (Re.Group.get groups 1) (Re.Group.get groups 2) in
    Re.replace re replace s in
  fun s ->
    let s = underscore re1 s in
    let s = underscore re2 s in
    let s = Re.replace_string re3 ~by:"_" s in
    String.lowercase_ascii s

let rec item_kind_to_string (items : Swagger_j.items option) = function
  | `String  -> "string"
  | `Number  -> "float"
  | `Integer -> "int"
  | `Boolean -> "bool"
  | `Array   ->
      let open Swagger_j in
      match items with
      | Some is -> item_kind_to_string is.items is.kind ^ " list"
      | None -> failwith "item_kind_to_string: array type must have an 'items' field"

module Type = struct
  module Sig = struct
    type t =
      | Abstract of string

    let abstract name = Abstract name

    let to_string ?(indent = 0) t =
      let pad = String.make indent ' ' in
      match t with
      | Abstract name -> sprintf "%stype %s\n" pad name
  end

  module Impl = struct
    type t =
      | Alias of string * string
      | Record of string * (string * string) list
      | Phantom of string

    let alias name target = Alias (name, target)
    let record name fields = Record (name, fields)
    let phantom name = Phantom name

    let to_string ?(indent = 0) t =
      let pad = String.make indent ' ' in
      match t with
      | Phantom name -> sprintf "%stype %s\n" pad name
      | Alias (name, target) -> sprintf "%stype %s = %s\n" pad name target
      | Record (name, fields) ->
          let s =
            List.fold_left
              (fun acc (fld, typ) -> acc ^ sprintf " %s : %s;" fld typ)
              ""
              fields in
          sprintf "%stype %s = {%s }\n" pad name s
  end

  type t =
    { signature : Sig.t
    ; implementation : Impl.t
    }

  let create signature implementation = { signature; implementation }
end

module Val = struct
  module Sig = struct
    type t = string * string list * string

    let create name params ret = (name, params, ret)

    let to_string ?(indent = 0) (name, params, ret) =
      let pad = String.make indent ' ' in
      let params =
        match params with
        | [] -> ["unit"]
        | _  -> params in
      sprintf "%sval %s : %s -> %s\n" pad name (String.concat " -> " params) ret
  end

  module Impl = struct
    type body = Record | Accessor | Id | Get | Put | Post | Delete | Head | Patch | Options
    type t = string * string list * body

    let create name params body = (name, params, body)

    let body_to_string (name, params, body) =
      let params =
        List.map
          (fun p ->
            if p.[0] = '~' || p.[0] = '?'
            then String.sub p 1 (String.length p - 1)
            else p)
          params in
      match body with
      | Record -> sprintf "{ %s }" (String.concat "; " params)
      | Accessor -> sprintf "t.%s" name
      | Id -> "t"
      | _ -> "assert false"

    let to_string ?(indent = 0) ((name, params, body) as value) =
      let pad = String.make indent ' ' in
      sprintf "%slet %s %s =\n%s%s\n"
        pad
        name
        (String.concat " " params)
        (String.make (indent + 2) ' ')
        (body_to_string value)
  end

  type t =
    { signature : Sig.t
    ; implementation : Impl.t
    }

  let create signature implementation = { signature; implementation }
end

module StringMap = Map.Make (struct
  type t = string
  let compare = compare
end)

module StringSet = Set.Make (struct
  type t = string
  let compare = compare
end)

let strip_base base path =
  let plen = String.length path in
  let blen = String.length base in
  if plen >= blen then
    if base = String.sub path 0 blen then
      String.sub path blen (plen - blen)
    else
      path
  else
    path

module Mod = struct
  type t =
    { name : string
    ; path : string list
    ; types : Type.t list
    ; values : Val.t list
    ; submodules : t StringMap.t
    ; deps : StringSet.t
    ; recursive : bool
    }

  let module_name s =
    let s =
      let last = String.length s - 1 in
      if s.[0] = '{' && s.[last] = '}' then "By_" ^ String.sub s 1 (last - 1)
      else s in
    s
    |> String.lowercase_ascii
    |> String.capitalize_ascii
    |> String.map (fun c -> if c = '-' then '_' else c)
    |> String.split_on_char '.'
    |> List.hd

  let create ~name ?(recursive = false) ?(path = []) ?(types = []) ?(submodules = StringMap.empty) ?(values = []) ?(deps = StringSet.empty) () =
    { name = module_name name
    ; path = List.map module_name path
    ; types
    ; values
    ; submodules
    ; deps
    ; recursive
    }

  let empty name ?(recursive = false) ?(path = []) () =
    create ~name ~recursive ~path ()

  let with_values name ?(recursive = false) ?(path = []) values =
    create ~name ~recursive ~path ~values ()

  let name m = m.name

  let submodules m =
    m.submodules
    |> StringMap.bindings
    |> List.map snd

  let add_type t m =
    { m with types = t :: m.types }

  let add_val v m =
    { m with values = v :: m.values }

  let add_types ts m =
    { m with types = m.types @ ts }

  let add_vals vs m =
    { m with values = m.values @ vs }

  let add_mod subm m =
    { m with submodules = StringMap.add subm.name subm m.submodules }

  let map_submodules f m =
    { m with submodules = StringMap.map f m.submodules }

  let has_submodules m =
    StringMap.is_empty m.submodules

  let find_submodule name m =
    StringMap.find_opt name m.submodules

  let iter f m =
    f m;
    StringMap.iter
      (fun _name sub -> f sub)
      m.submodules

  let path m =
    m.path

  let qualified_name m =
    match m.path with
    | [] -> m.name
    | p -> sprintf "%s.%s" (String.concat "." m.path) m.name

  let qualified_path m =
    m.path @ [m.name]

  let rec is_subpath p ~candidate =
    match p, candidate with
    | [], [] -> true
    | [], _ -> true
    | _, [] -> false
    | x::xs, y::ys when x = y -> is_subpath xs ~candidate:ys
    | _, _ -> false

  let rec depends_on other this =
    StringSet.exists (fun d -> is_subpath (qualified_path other) ~candidate:(String.split_on_char '.' d)) this.deps
    || StringMap.exists (fun _ m -> depends_on other m) this.submodules

  let sort_by_deps =
    List.sort
      (fun (_, m1) (_, m2) ->
        let d1 = depends_on m1 m2 in
        let d2 = depends_on m2 m1 in
        if d1 && d2 then 0
        else if d1 then -1
        else if d2 then 1
        else 0)

  let rec sig_to_string ?(indent = 0) m =
    let pad = String.make indent ' ' in
    let submods =
      m.submodules
      |> StringMap.bindings
      |> sort_by_deps
      |> List.fold_left
           (fun acc (name, m) ->
             let s = sig_to_string ~indent:(indent + 2) m in
             (* Definitions first to simplify references *)
             if name = "Definitions" then s ^ acc
             else acc ^ s)
           "" in
    sprintf "%smodule%s%s : sig\n%s%s%s%send\n"
      pad
      (if m.recursive then " rec " else " ")
      m.name
      submods
      (String.concat "\n" (List.map (fun t -> Type.Sig.to_string ~indent:(indent + 2) t.Type.signature) m.types))
      (String.concat "" (List.map (fun v -> Val.Sig.to_string ~indent:(indent + 2) v.Val.signature) m.values))
      pad

  let rec impl_to_string ?(indent = 0) m =
    let pad = String.make indent ' ' in
    let submods =
      m.submodules
      |> StringMap.bindings
      |> sort_by_deps
      |> List.fold_left
           (fun acc (name, m) ->
             let s = impl_to_string ~indent:(indent + 2) m in
             (* Definitions first to simplify references *)
             if name = "Definitions" then s ^ acc
             else acc ^ s)
           "" in
    let decl =
      if m.recursive
      then ""
      else sprintf "%smodule %s " pad m.name in

    sprintf "%s= struct\n%s%s%s%send\n"
      decl
      submods
      (String.concat "\n" (List.map (fun t -> Type.Impl.to_string ~indent:(indent + 2) t.Type.implementation) m.types))
      (String.concat "" (List.map (fun v -> Val.Impl.to_string ~indent:(indent + 2) v.Val.implementation) m.values))
      pad

  let to_string ?indent m =
    sprintf "%s %s"
      (sig_to_string ?indent m)
      (impl_to_string ?indent m)
end

let split_ref ref =
  ref
  |> String.split_on_char '.'
  |> List.filter ((<>)"")
  |> List.map Mod.module_name

let reference_module_path ~root ~base ref =
  let path =
    ref
    |> String.lowercase_ascii
    |> strip_base (String.lowercase_ascii base)
    |> split_ref in
  Mod.qualified_name root :: path

let reference_module ~root ~base ref =
  reference_module_path ~root ~base ref
  |> String.concat "."

let reference_type ~root ~base ref =
  reference_module ~root ~base ref ^ ".t"

module Schema = struct
  type t =
    { raw            : Swagger_j.schema
    ; reference_base : string
    }

  let create ~reference_base raw =
    { raw; reference_base }

  let rec depends ~root t =
    let base = t.reference_base in
    match t.raw.ref with
    | Some r -> Some (reference_module ~root ~base r)
    | None ->
        match some t.raw.kind with
        | `Array ->
            let open Swagger_j in
            (match t.raw.items with
            | Some s -> depends ~root (create ~reference_base:base s)
            | None -> failwith "Schema.depends: array type must have an 'items' field")
        | _ -> None

  let rec kind_to_string ~root t =
    let reference_base = t.reference_base in
    match t.raw.ref with
    | Some r -> reference_type ~root ~base:reference_base r
    | None ->
        match some t.raw.kind with
        | `String  -> "string"
        | `Number  -> "float"
        | `Integer -> "int"
        | `Boolean -> "bool"
        | `Object  ->
            let open Swagger_j in
            (match t.raw.additional_properties with
            | Some props -> sprintf "(string * %s) list" (kind_to_string ~root (create ~reference_base props))
            | None -> failwith "Schema.kind_to_string: object without additional_properties")
        | `Array   ->
            let open Swagger_j in
            match t.raw.items with
            | Some s -> kind_to_string ~root (create ~reference_base s) ^ " list"
            | None -> failwith "Schema.kind_to_string: array type must have an 'items' field"

  let to_string t =
    let reference_base = t.reference_base in
    match t.raw.ref with
    | Some r -> reference_type ~base:reference_base r
    | None -> kind_to_string t
end

module Param = struct
  type t = Swagger_j.parameter_or_reference

  let rec kind_to_string (p : t) =
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
        | None -> failwith "Param.kind_to_string: array type must have an 'items' field"

  let is_keyword = function
    | "object"
    | "to"
    | "type" -> true
    | _ -> false

  let name n =
    let n =
      if n.[0] = '$' then String.sub n 1 (String.length n - 1)
      else n in
    let n = snake_case n in
    if is_keyword n then n ^ "_"
    else n

  let prefixes required =
    if required
    then ("", "~")
    else ("?", "?")

  let to_string ~root ~reference_base (p : t) =
    let open Swagger_j in
    let sig_prefix, impl_prefix = prefixes p.required in
    let kind =
      match p.location with
      | `Body -> Schema.to_string ~root (Schema.create ~reference_base (some p.schema))
      | _     -> kind_to_string p in
    let sig_str = sprintf "%s%s:%s" sig_prefix (name p.name) kind in
    let impl_str = sprintf "%s%s" impl_prefix (name p.name) in
    (sig_str, impl_str)
end

let merge_params (ps1 : Swagger_j.parameter_or_reference list) (ps2 : Swagger_j.parameter_or_reference list) =
  let open Swagger_j in
  let rec merge acc = function
    | [] -> acc
    | (p : Swagger_j.parameter_or_reference)::ps ->
        if List.exists (fun (q : Swagger_j.parameter_or_reference) -> p.name = q.name) acc
        then merge acc ps
        else merge (p::acc) ps in
  merge ps2 ps1

let operation_params = function
  | None, None -> []
  | Some ps, None -> ps
  | None, Some ps -> ps
  | Some ps, Some ps' -> merge_params ps ps'

let resp_type ~root ~base (resp : Swagger_j.response_or_reference) =
  let ref_type = reference_type ~root ~base in
  match resp.ref with
  | Some r -> ref_type r
  | None ->
      match resp.schema with
      | Some s -> Schema.to_string ~root (Schema.create ~reference_base:base s)
      | None -> "unit"

let rec return_type ~root ~base resps =
  let is_error code =
    let code = int_of_string code in
    code < 200 || code >= 300 in
  let responses_match (r1 : Swagger_j.response_or_reference) (r2 : Swagger_j.response_or_reference) =
    match r1.schema, r2.schema with
    | Some s1, Some s2 -> s1 = s2
    | _ -> false in
  match resps with
  | [] ->
      "unit" (* XXX error? *)
  | (code, _)::rs when is_error code ->
      return_type ~root ~base rs (* ignore errors; assume strings *)
  | (_code, resp)::rs ->
      (* check all 2xx responses return the same type *)
      let rec check first = function
        | [] -> ()
        | (code, _)::res when is_error code -> check first res
        | (_code', resp')::rs when responses_match first resp' -> check first rs
        | (c, (r:Swagger_j.response_or_reference))::_ -> failwith @@ sprintf "multiple response types are not supported: %s - %s" (Swagger_j.string_of_response_or_reference resp) (Swagger_j.string_of_response_or_reference r) in
      check resp rs;
      sprintf "(%s, string) result" (resp_type ~root ~base resp)

let operation_body_type = function
  | "get" -> Val.Impl.Get
  | "put" -> Val.Impl.Put
  | "post" -> Val.Impl.Post
  | "delete" -> Val.Impl.Delete
  | "head" -> Val.Impl.Head
  | "patch" -> Val.Impl.Patch
  | "options" -> Val.Impl.Options
  | op -> failwith ("unknown operation: " ^ op)

let operation_val ~root ~reference_base name params = function
  | Some (op : Swagger_j.operation) ->
      let sig_params, impl_params =
        operation_params (params, op.parameters)
        |> List.map (Param.to_string ~root ~reference_base)
        |> List.split in
      (* TODO op.responses *)
      let ret = return_type ~root ~base:reference_base op.responses in
      let signature = Val.Sig.create name sig_params ret in
      let implementation = Val.Impl.create name impl_params (operation_body_type name) in
      Some (Val.create signature implementation)
  | None ->
      None

let rec keep_some = function
  | [] -> []
  | Some x :: xs -> x :: keep_some xs
  | None :: xs -> keep_some xs

let path_item_vals ~root ~reference_base (item : Swagger_j.path_item) : Val.t list =
  let get     = operation_val ~root ~reference_base "get"     item.parameters item.get in
  let put     = operation_val ~root ~reference_base "put"     item.parameters item.put in
  let post    = operation_val ~root ~reference_base "post"    item.parameters item.post in
  let delete  = operation_val ~root ~reference_base "delete"  item.parameters item.delete in
  let options = operation_val ~root ~reference_base "options" item.parameters item.options in
  let head    = operation_val ~root ~reference_base "head"    item.parameters item.head in
  let patch   = operation_val ~root ~reference_base "patch"   item.parameters item.patch in
  keep_some [get; put; post; delete; options; head; patch]

let definition_module ?parent_path ~root ~reference_base ~name (schema : Swagger_j.schema) : Mod.t =
  let required = default [] schema.required in
  let properties = default [] schema.properties in

  let update_deps deps = function
    | Some d -> StringSet.add d deps
    | None -> deps in

  let create_params =
    List.fold_left
      (fun (params, deps) (name, schema) ->
        let param_name = Param.name name in
        let sig_prefix, impl_prefix = Param.prefixes (List.mem name required) in
        let s = Schema.create ~reference_base schema in
        let schema_str = Schema.to_string ~root s in
        let sig_param = sprintf "%s%s:%s" sig_prefix param_name schema_str in
        let impl_param = sprintf "%s%s" impl_prefix param_name in
        let deps = update_deps deps (Schema.depends ~root s) in
        (sig_param, impl_param)::params, deps)
      ([], StringSet.empty) in

  let alias_type () =
    let tgt = Schema.kind_to_string ~root (Schema.create ~reference_base schema) in
    let typ = Type.create (Type.Sig.abstract "t") (Type.Impl.alias "t" tgt) in
    let create =
      Val.create
        (Val.Sig.create "create" [tgt] "t")
        (Val.Impl.create "create" ["t"] Val.Impl.Id) in
    [typ], [create], StringSet.empty in

  let record_type () =
    let params, deps = create_params properties in
    let sig_params, impl_params = params |> List.rev |> List.split in
    let create =
      Val.create
        (Val.Sig.create "create" sig_params "t")
        (Val.Impl.create "create" impl_params Val.Impl.Record) in
    let fields, values, deps =
      List.fold_left
        (fun (fields, values, deps) (name, schema) ->
          let opt = if List.mem name required then "" else " option" in
          let s = Schema.create ~reference_base schema in
          let ret = sprintf "%s%s" (Schema.to_string ~root s) opt in
          let deps = update_deps deps (Schema.depends ~root s) in
          let param_name = Param.name name in
          let field = (param_name, ret) in
          let value =
            Val.create
              (Val.Sig.create param_name ["t"] ret)
              (Val.Impl.create param_name ["t"] Val.Impl.Accessor) in
          (field :: fields, value :: values, deps))
        ([], [], deps)
        properties in
    let values = create :: List.rev values in
    let type_sig = Type.Sig.abstract "t" in
    let type_impl = Type.Impl.record "t" fields in
    let typ = Type.create type_sig type_impl in
    [typ], values, deps in

  let phantom_type () =
    let typ = Type.create (Type.Sig.abstract "t") (Type.Impl.phantom "t") in
    [typ], [], StringSet.empty in

  let path =
    match parent_path with
    | Some p -> String.split_on_char '.' p
    | None -> [] in

  let types, values, deps =
    match schema.kind, schema.properties with
    | Some _, _ -> alias_type ()
    | None, Some _ -> record_type ()
    | None, None -> phantom_type () in

  Mod.create ~name ~path ~types ~values ~deps ()

let insert_module m root paths =
  let rec insert acc root = function
    | [] -> Mod.add_mod m root
    | p::ps when Mod.name root = p -> insert (p::acc) root ps
    | p::ps ->
        match Mod.find_submodule p root with
        | Some subm ->
            Mod.add_mod (insert (p::acc) subm ps) root
        | None ->
            let path = p::acc in
            let pmod = insert path (Mod.empty p ~path:(List.rev acc) ()) ps in
            Mod.add_mod pmod root in
  insert [] root paths

let remove_base base segments =
  match base, segments with
  | Some base, s::ss when base = s -> ss
  | _ -> segments

let rec build_paths ~root ~path_base ~reference_base = function
  | [] -> root
  | (path, item) :: paths ->
      let parents_and_child =
        path
        |> strip_base path_base
        |> String.split_on_char '/'
        |> List.filter ((<>)"")
        |> unsnoc in
      match parents_and_child with
      | Some (parents, child) ->
          let child_module = Mod.with_values child (path_item_vals ~root ~reference_base item) in
          let root = insert_module child_module root parents in
          build_paths ~root ~path_base ~reference_base paths
      | None ->
          let root = Mod.add_vals (path_item_vals ~root ~reference_base item) root in
          build_paths ~root ~path_base ~reference_base paths

let rec build_definitions ~root ~definition_base ~reference_base l =
  match l with
  | [] -> root
  | (name, (schema : Swagger_j.schema)) :: defs when schema.ref = None ->
      let parents_and_child =
        name
        |> strip_base definition_base
        |> split_ref
        |> unsnoc in
      (match parents_and_child with
      | Some (parents, child) ->
          let parent_path = String.concat "." parents in
          let def = definition_module ~root ~reference_base ~parent_path ~name:child schema in
          let root = insert_module def root parents in
          build_definitions ~root ~definition_base ~reference_base defs
      | None ->
          let root = Mod.add_mod (definition_module ~root ~reference_base ~name schema) root in
          build_definitions ~root ~definition_base ~reference_base defs)
  (* XXX ignore schemas that are simply references? just use the referenced module?
   * in the kubernetes API this seems to be only for deprecated stuff. *)
  | (name, (schema : Swagger_j.schema)) :: defs ->
      build_definitions ~root ~definition_base ~reference_base defs

let of_swagger ?(path_base = "") ?(definition_base = "") ?(reference_base = "") s =
  let open Swagger_j in
  let definitions = default [] s.definitions in
  let title = s.info.title in
  let defs =
    build_definitions
      ~root:(Mod.empty "Definitions" ~path:[title] ())
      ~definition_base
      ~reference_base
      definitions in
  let defs = build_paths ~root:defs ~path_base ~reference_base s.paths in
  Mod.add_mod defs (Mod.empty ~recursive:true title ())

let to_string = Mod.to_string
