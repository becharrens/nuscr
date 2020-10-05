open! Base
open Printf
open Loc
open Err
open Names

type payload =
  | PValue of VariableName.t option * PayloadTypeName.t
  | PDelegate of ProtocolName.t * RoleName.t
[@@deriving sexp_of]

(* Ignoring variable names for now *)
let equal_payload p1 p2 =
  match (p1, p2) with
  | PValue (_, n1), PValue (_, n2) -> PayloadTypeName.equal n1 n2
  | PDelegate (pn1, rn1), PDelegate (pn2, rn2) ->
      ProtocolName.equal pn1 pn2 && RoleName.equal rn1 rn2
  | _, _ -> false

let equal_pvalue_payload p1 p2 =
  let var_name_equal = Option.equal VariableName.equal in
  match (p1, p2) with
  | PValue (v1, n1), PValue (v2, n2) ->
      var_name_equal v1 v2 && PayloadTypeName.equal n1 n2
  | _ -> equal_payload p1 p2

let compare_payload p1 p2 =
  match (p1, p2) with
  | PValue (_, ptn1), PValue (_, ptn2) -> PayloadTypeName.compare ptn1 ptn2
  | PValue _, PDelegate _ -> -1
  | PDelegate _, PValue _ -> 1
  | PDelegate (pn1, rn1), PDelegate (pn2, rn2) ->
      let comp_fst = ProtocolName.compare pn1 pn2 in
      if comp_fst = 0 then RoleName.compare rn1 rn2 else comp_fst

let show_payload = function
  | PValue (var, ty) ->
      let var =
        match var with
        | Some var -> VariableName.user var ^ ": "
        | None -> ""
      in
      sprintf "%s%s" var (PayloadTypeName.user ty)
  | PDelegate (proto, role) ->
      sprintf "%s @ %s" (ProtocolName.user proto) (RoleName.user role)

let pp_payload fmt p = Caml.Format.fprintf fmt "%s" (show_payload p)

let of_syntax_payload (payload : Syntax.payloadt) =
  let open Syntax in
  match payload with
  | PayloadName n -> PValue (None, PayloadTypeName.of_name n)
  | PayloadDel (p, r) ->
      PDelegate (ProtocolName.of_name p, RoleName.of_name r)
  | PayloadBnd (var, n) ->
      PValue (Some (VariableName.of_name var), PayloadTypeName.of_name n)

type message = {label: LabelName.t; payload: payload list}
[@@deriving eq, sexp_of, ord]

let show_message {label; payload} =
  sprintf "%s(%s)" (LabelName.user label)
    (String.concat ~sep:", " (List.map ~f:show_payload payload))

let pp_message fmt m = Caml.Format.fprintf fmt "%s" (show_message m)

let of_syntax_message (message : Syntax.message) =
  let open Syntax in
  match message with
  | Message {name; payload} ->
      { label= LabelName.of_name name
      ; payload= List.map ~f:of_syntax_payload payload }
  | MessageName name -> {label= LabelName.of_name name; payload= []}

module type S = sig
  type t [@@deriving sexp_of]

  val show : t -> string

  val create : RoleName.t -> RoleName.t -> message -> t

  include Comparable.S with type t := t
end

module GAction = struct
  module M = struct
    type t = RoleName.t * RoleName.t * message [@@deriving sexp_of, ord]

    let show ((r1, r2, m) : t) =
      sprintf "%s->%s:%s" (RoleName.user r1) (RoleName.user r2)
        (show_message m)

    let create r1 r2 m : t = (r1, r2, m)
  end

  include M
  include Comparable.Make (M)
end

type t =
  | MessageG of message * RoleName.t * RoleName.t * t
  | MuG of TypeVariableName.t * t
  | TVarG of TypeVariableName.t
  | ChoiceG of RoleName.t * t list
  | MixedChoiceG of t list
  | EndG
  | CallG of RoleName.t * ProtocolName.t * RoleName.t list * t

type global_t =
  ( ProtocolName.t
  , (RoleName.t list * RoleName.t list) * ProtocolName.t list * t
  , ProtocolName.comparator_witness )
  Map.t

let show =
  let indent_here indent = String.make (indent * 2) ' ' in
  let rec show_global_type_internal indent =
    let current_indent = indent_here indent in
    function
    | MessageG (m, r1, r2, g) ->
        sprintf "%s%s from %s to %s;\n%s" current_indent (show_message m)
          (RoleName.user r1) (RoleName.user r2)
          (show_global_type_internal indent g)
    | MuG (n, g) ->
        sprintf "%srec %s {\n%s%s}\n" current_indent
          (TypeVariableName.user n)
          (show_global_type_internal (indent + 1) g)
          current_indent
    | TVarG n ->
        sprintf "%scontinue %s;\n" current_indent (TypeVariableName.user n)
    | EndG -> sprintf "%send\n" current_indent
    | ChoiceG (r, gs) ->
        let pre =
          sprintf "%schoice at %s {\n" current_indent (RoleName.user r)
        in
        let intermission = sprintf "%s} or {\n" current_indent in
        let post = sprintf "%s}\n" current_indent in
        let choices =
          List.map ~f:(show_global_type_internal (indent + 1)) gs
        in
        let gs = String.concat ~sep:intermission choices in
        pre ^ gs ^ post
    | MixedChoiceG gs ->
        let pre = sprintf "%smchoice {\n" current_indent in
        let intermission = sprintf "%s} or {\n" current_indent in
        let post = sprintf "%s}\n" current_indent in
        let choices =
          List.map ~f:(show_global_type_internal (indent + 1)) gs
        in
        let gs = String.concat ~sep:intermission choices in
        pre ^ gs ^ post
    | CallG (caller, proto_name, roles, g) ->
        sprintf "%s%s calls %s(%s);\n%s" current_indent
          (RoleName.user caller)
          (ProtocolName.user proto_name)
          (String.concat ~sep:", " (List.map ~f:RoleName.user roles))
          (show_global_type_internal indent g)
  in
  show_global_type_internal 0

let call_label caller protocol roles =
  let str_roles = List.map ~f:RoleName.user roles in
  let roles_str = String.concat ~sep:"," str_roles in
  (* Current label is a bit arbitrary - find better one? *)
  let label_str =
    sprintf "call(%s, %s(%s))" (RoleName.user caller)
      (ProtocolName.user protocol)
      roles_str
  in
  LabelName.create label_str (ProtocolName.where protocol)

let show_global_t (g : global_t) =
  let show_aux ~key ~data acc =
    let (roles, new_roles), nested_protocols, g_ = data in
    let roles_str = List.map ~f:RoleName.user roles in
    let new_roles_str = List.map ~f:RoleName.user new_roles in
    let str_proto_names = List.map ~f:ProtocolName.user nested_protocols in
    let names_str = String.concat ~sep:", " str_proto_names in
    let proto_str =
      sprintf "protocol %s(%s) {\n\nNested Protocols: %s\n\n%s\n}"
        (ProtocolName.user key)
        (Symtable.show_roles (roles_str, new_roles_str))
        (if String.length names_str = 0 then "-" else names_str)
        (show g_)
    in
    proto_str :: acc
  in
  String.concat ~sep:"\n\n" (List.rev (Map.fold ~init:[] ~f:show_aux g))

let of_protocol (global_protocol : Syntax.global_protocol) =
  let open Syntax in
  let {Loc.value= {roles; interactions; _}; _} = global_protocol in
  let roles = List.map ~f:RoleName.of_name roles in
  let assert_empty l =
    if not @@ List.is_empty l then unimpl "Non tail-recursive protocol"
  in
  let check_role r =
    if not @@ List.mem roles r ~equal:RoleName.equal then
      uerr @@ UnboundRole r
  in
  let rec conv_interactions rec_names
      (interactions : global_interaction list) =
    match interactions with
    | [] -> EndG
    | {value; _} :: rest -> (
      match value with
      | MessageTransfer {message; from_role; to_roles; _} ->
          let from_role = RoleName.of_name from_role in
          let to_roles = List.map ~f:RoleName.of_name to_roles in
          check_role from_role ;
          let init = conv_interactions rec_names rest in
          let f to_role acc =
            check_role to_role ;
            if RoleName.equal from_role to_role then
              uerr (ReflexiveMessage from_role) ;
            MessageG (of_syntax_message message, from_role, to_role, acc)
          in
          List.fold_right ~f ~init to_roles
      | Recursion (rname, interactions) ->
          let rname = TypeVariableName.of_name rname in
          if List.mem rec_names rname ~equal:TypeVariableName.equal then
            unimpl "Alpha convert recursion names"
          else assert_empty rest ;
          MuG (rname, conv_interactions (rname :: rec_names) interactions)
      | Continue name ->
          let name = TypeVariableName.of_name name in
          if List.mem rec_names name ~equal:TypeVariableName.equal then (
            assert_empty rest ; TVarG name )
          else uerr (UnboundRecursionName name)
      | Choice (role, interactions_list) ->
          let role = RoleName.of_name role in
          assert_empty rest ;
          check_role role ;
          ChoiceG
            ( role
            , List.map ~f:(conv_interactions rec_names) interactions_list )
      | MixedChoice interactions_list ->
          assert_empty rest ;
          MixedChoiceG
            (List.map ~f:(conv_interactions rec_names) interactions_list)
      | Do (protocol, _, roles, _) ->
          (* Previously this would've been a violation *)
          let fst_role = RoleName.of_name @@ List.hd_exn roles in
          let role_names = List.map ~f:RoleName.of_name roles in
          let cont = conv_interactions rec_names rest in
          CallG (fst_role, ProtocolName.of_name protocol, role_names, cont)
      | Calls (caller, proto, _, roles, _) ->
          let caller_role = RoleName.of_name caller in
          let role_names = List.map ~f:RoleName.of_name roles in
          let cont = conv_interactions rec_names rest in
          CallG (caller_role, ProtocolName.of_name proto, role_names, cont) )
  in
  conv_interactions [] interactions

let global_t_of_module (scr_module : Syntax.scr_module) =
  let open Syntax in
  let split_role_names (roles, new_roles) =
    let role_names = List.map ~f:RoleName.of_name roles in
    let new_role_names = List.map ~f:RoleName.of_name new_roles in
    (role_names, new_role_names)
  in
  let rec add_protocol protocols (protocol : global_protocol) =
    let nested_protocols = protocol.value.nested_protocols in
    let protocols =
      List.fold ~init:protocols ~f:add_protocol nested_protocols
    in
    let proto_name = ProtocolName.of_name protocol.value.name in
    let g = of_protocol protocol in
    let roles = split_role_names protocol.value.split_roles in
    let nested_protocol_names =
      List.map
        ~f:(fun {Loc.value= {name; _}; _} -> ProtocolName.of_name name)
        nested_protocols
    in
    Map.add_exn protocols ~key:proto_name
      ~data:(roles, nested_protocol_names, g)
  in
  let all_protocols = scr_module.protocols @ scr_module.nested_protocols in
  List.fold
    ~init:(Map.empty (module ProtocolName))
    ~f:add_protocol all_protocols

let rec flatten = function
  | ChoiceG (role, choices) ->
      let choices = List.map ~f:flatten choices in
      let lift = function
        | ChoiceG (role_, choices_) when RoleName.equal role role_ ->
            choices_
        | ChoiceG (role_, _choices) ->
            uerr (InconsistentNestedChoice (role, role_))
        | g -> [g]
      in
      ChoiceG (role, List.concat_map ~f:lift choices)
  | MixedChoiceG choices ->
      let choices = List.map ~f:flatten choices in
      let lift = function MixedChoiceG choices_ -> choices_ | g -> [g] in
      MixedChoiceG (List.concat_map ~f:lift choices)
  | g -> g

let recursion_protocol_name protocol rec_var =
  sprintf "%s_%s"
    (ProtocolName.user protocol)
    (TypeVariableName.user rec_var)

let rec get_participants rec_protocols participants = function
  | MuG (_, gtype) -> get_participants rec_protocols participants gtype
  | MessageG (_, sender, recv, gtype) ->
      let participants = Set.add participants sender in
      let participants = Set.add participants recv in
      get_participants rec_protocols participants gtype
  | ChoiceG (_, gtypes) ->
      List.fold ~init:participants ~f:(get_participants rec_protocols) gtypes
  | MixedChoiceG gtypes ->
      List.fold ~init:participants ~f:(get_participants rec_protocols) gtypes
  | CallG (caller, _, roles, gtype) ->
      let participants = Set.add participants caller in
      let participants = List.fold ~init:participants ~f:Set.add roles in
      get_participants rec_protocols participants gtype
  | EndG -> participants
  | TVarG rec_var -> (
    match Map.find rec_protocols rec_var with
    | None ->
        (* If the participants this variable's protocol have not been
           computed it means that either it is currently being computed or a
           recursion variable above it is being computed. In both cases all
           its participants will be included in the current computation *)
        participants
    | Some (_, roles) ->
        let roles_set = Set.of_list (module RoleName) roles in
        let all_participants = Set.union participants roles_set in
        all_participants )

let rec convert_recursion_to_protocols protocol rec_protocols
    (global_t, name_gen) = function
  (* Return name gen *)
  | MuG (rec_var, gtype') -> (
      let roles_set =
        get_participants rec_protocols (Set.empty (module RoleName)) gtype'
      in
      let roles = Set.to_list roles_set in
      match roles with
      | [] -> ((global_t, name_gen), EndG)
      | fst_role :: _ ->
          let protocol_name = recursion_protocol_name protocol rec_var in
          let name_gen, protocol_name =
            Namegen.unique_name name_gen protocol_name
          in
          let rec_protocol_name = ProtocolName.of_string protocol_name in
          let rec_protocols =
            Map.add_exn rec_protocols ~key:rec_var
              ~data:(rec_protocol_name, roles)
          in
          let (global_t, name_gen), new_gtype =
            convert_recursion_to_protocols protocol rec_protocols
              (global_t, name_gen) gtype'
          in
          let global_t =
            Map.add_exn global_t ~key:rec_protocol_name
              ~data:((roles, []), [], new_gtype)
          in
          ( (global_t, name_gen)
          , CallG (fst_role, rec_protocol_name, roles, EndG) ) )
  | TVarG rec_var ->
      let rec_protocol, roles = Map.find_exn rec_protocols rec_var in
      let caller = List.hd_exn roles in
      ((global_t, name_gen), CallG (caller, rec_protocol, roles, EndG))
  | MessageG (msg, sender, recv, gtype') ->
      let (global_t, name_gen), new_gtype =
        convert_recursion_to_protocols protocol rec_protocols
          (global_t, name_gen) gtype'
      in
      ((global_t, name_gen), MessageG (msg, sender, recv, new_gtype))
  | ChoiceG (choice_role, gtypes) ->
      let (global_t, name_gen), new_gtypes =
        List.fold_map gtypes ~init:(global_t, name_gen)
          ~f:(convert_recursion_to_protocols protocol rec_protocols)
      in
      ((global_t, name_gen), ChoiceG (choice_role, new_gtypes))
  | MixedChoiceG gtypes ->
      let (global_t, name_gen), new_gtypes =
        List.fold_map gtypes ~init:(global_t, name_gen)
          ~f:(convert_recursion_to_protocols protocol rec_protocols)
      in
      ((global_t, name_gen), MixedChoiceG new_gtypes)
  | EndG -> ((global_t, name_gen), EndG)
  | CallG (caller, protocol, participants, gtype') ->
      let (global_t, name_gen), new_gtype =
        convert_recursion_to_protocols protocol rec_protocols
          (global_t, name_gen) gtype'
      in
      ( (global_t, name_gen)
      , CallG (caller, protocol, participants, new_gtype) )

let replace_recursion_with_nested_protocols global_t =
  let replace_recursion_in_protocol ~key:protocol
      ~data:(roles, nested_protocols, gtype) (global_t, name_gen) =
    let (global_t, name_gen), gtype =
      convert_recursion_to_protocols protocol
        (Map.empty (module TypeVariableName))
        (global_t, name_gen) gtype
    in
    let global_t =
      Map.add_exn global_t ~key:protocol
        ~data:(roles, nested_protocols, gtype)
    in
    (global_t, name_gen)
  in
  let name_gen = Namegen.create () in
  let name_gen =
    Map.fold global_t ~init:name_gen
      ~f:(fun ~key:protocol ~data:_ name_gen ->
        let name_gen, _ =
          Namegen.unique_name name_gen (ProtocolName.user protocol)
        in
        name_gen)
  in
  let global_t, _ =
    Map.fold global_t
      ~init:(Map.empty (module ProtocolName), name_gen)
      ~f:replace_recursion_in_protocol
  in
  global_t

let rec substitute g tvar g_sub =
  match g with
  | TVarG tvar_ when TypeVariableName.equal tvar tvar_ -> g_sub
  | TVarG _ -> g
  | MuG (tvar_, _) when TypeVariableName.equal tvar tvar_ -> g
  | MuG (tvar_, g_) -> MuG (tvar_, substitute g_ tvar g_sub)
  | EndG -> EndG
  | MessageG (m, r1, r2, g_) -> MessageG (m, r1, r2, substitute g_ tvar g_sub)
  | ChoiceG (r, g_) ->
      ChoiceG (r, List.map ~f:(fun g__ -> substitute g__ tvar g_sub) g_)
  | MixedChoiceG g_ ->
      MixedChoiceG (List.map ~f:(fun g__ -> substitute g__ tvar g_sub) g_)
  | CallG (caller, protocol, roles, g_) ->
      CallG (caller, protocol, roles, substitute g_ tvar g_sub)

let rec unfold = function
  | MuG (tvar, g_) as g -> substitute g_ tvar g
  | g -> g

let rec normalise = function
  | MessageG (m, r1, r2, g_) -> MessageG (m, r1, r2, normalise g_)
  | ChoiceG (r, g_) ->
      let g_ = List.map ~f:normalise g_ in
      flatten (ChoiceG (r, g_))
  | MixedChoiceG g_ ->
      let g_ = List.map ~f:normalise g_ in
      flatten (MixedChoiceG g_)
  | (EndG | TVarG _) as g -> g
  | MuG (tvar, g_) -> unfold (MuG (tvar, normalise g_))
  | CallG (caller, protocol, roles, g_) ->
      CallG (caller, protocol, roles, normalise g_)

let normalise_global_t (global_t : global_t) =
  let normalise_protocol ~key ~data acc =
    let all_roles, nested_protocols, g = data in
    Map.add_exn acc ~key ~data:(all_roles, nested_protocols, normalise g)
  in
  Map.fold
    ~init:(Map.empty (module ProtocolName))
    ~f:normalise_protocol global_t

let rename_tvars tvars new_tvar gtype =
  let rec rename = function
    | MessageG (m, r1, r2, g_) -> MessageG (m, r1, r2, rename g_)
    | ChoiceG (r, gtypes) ->
        let gtypes = List.map ~f:rename gtypes in
        ChoiceG (r, gtypes)
    | MixedChoiceG gtypes ->
        let gtypes = List.map ~f:rename gtypes in
        MixedChoiceG gtypes
    | EndG -> EndG
    | TVarG tvar as g -> if Set.mem tvars tvar then TVarG new_tvar else g
    | MuG (tvar, g_) -> MuG (tvar, rename g_)
    | CallG (caller, protocol, roles, g_) ->
        CallG (caller, protocol, roles, rename g_)
  in
  rename gtype

let make_unique_tvars gtype =
  let rec make_unique name_gen = function
    | MessageG (m, r1, r2, g_) ->
        let name_gen, g = make_unique name_gen g_ in
        (name_gen, MessageG (m, r1, r2, g))
    | ChoiceG (r, gtypes) ->
        let name_gen, gtypes =
          List.fold_map ~init:name_gen ~f:make_unique gtypes
        in
        (name_gen, ChoiceG (r, gtypes))
    | MixedChoiceG gtypes ->
        let name_gen, gtypes =
          List.fold_map ~init:name_gen ~f:make_unique gtypes
        in
        (name_gen, MixedChoiceG gtypes)
    | (EndG | TVarG _) as g -> (name_gen, g)
    | MuG (tvar, g_) ->
        let curr_tvar_str = TypeVariableName.user tvar in
        let name_gen, new_tvar_str =
          Namegen.unique_name name_gen curr_tvar_str
        in
        let g_, tvar =
          if not (String.equal curr_tvar_str new_tvar_str) then
            let new_tvar = TypeVariableName.rename tvar new_tvar_str in
            let tvars = Set.singleton (module TypeVariableName) tvar in
            (rename_tvars tvars new_tvar g_, new_tvar)
          else (g_, tvar)
        in
        let name_gen, g_ = make_unique name_gen g_ in
        (name_gen, MuG (tvar, g_))
    | CallG (caller, protocol, roles, g_) ->
        let name_gen, g_ = make_unique name_gen g_ in
        (name_gen, CallG (caller, protocol, roles, g_))
  in
  let name_gen = Namegen.create () in
  let _, gtype = make_unique name_gen gtype in
  gtype

let build_tvar_mapping gtype =
  let rec build_mapping mapping = function
    | MessageG (_, _, _, g_) -> build_mapping mapping g_
    | ChoiceG (_, gtypes) -> List.fold ~init:mapping ~f:build_mapping gtypes
    | MixedChoiceG gtypes -> List.fold ~init:mapping ~f:build_mapping gtypes
    | EndG | TVarG _ -> mapping
    | MuG (tvar, g_) as g ->
        build_mapping (Map.add_exn mapping ~key:tvar ~data:g) g_
    | CallG (_, _, _, g_) -> build_mapping mapping g_
  in
  build_mapping (Map.empty (module TypeVariableName)) gtype

let rec flatten_recursion tvar flattened_tvars = function
  | MuG (tvar_, gtype) ->
      flatten_recursion tvar (Set.add flattened_tvars tvar_) gtype
  | g ->
      if Set.length flattened_tvars > 0 then
        rename_tvars flattened_tvars tvar g
      else g

let first_actions tvar_mapping gtype =
  let rec get_first_actions expanded_tvars = function
    | MessageG (m, r1, r2, _) ->
        let actions = Set.empty (module GAction) in
        Set.add actions (GAction.create r1 r2 m)
    | ChoiceG (_, gtypes) ->
        List.fold
          ~init:(Set.empty (module GAction))
          ~f:(fun actions g ->
            Set.union actions (get_first_actions expanded_tvars g))
          gtypes
    | MixedChoiceG gtypes ->
        List.fold
          ~init:(Set.empty (module GAction))
          ~f:(fun actions g ->
            Set.union actions (get_first_actions expanded_tvars g))
          gtypes
    | EndG -> Set.empty (module GAction)
    | TVarG tvar ->
        if Set.mem expanded_tvars tvar then Set.empty (module GAction)
        else
          let rec_type = Map.find_exn tvar_mapping tvar in
          get_first_actions (Set.add expanded_tvars tvar) rec_type
    | MuG (_, g) -> get_first_actions expanded_tvars g
    | CallG _ ->
        Violation "First actions of protocol call are not defined" |> raise
  in
  get_first_actions (Set.empty (module TypeVariableName)) gtype

let normalise_recursion g =
  let rec flatten_gtype = function
    | MessageG (m, r1, r2, g_) -> MessageG (m, r1, r2, flatten_gtype g_)
    | ChoiceG (r, gtypes) ->
        let g_ = List.map ~f:flatten_gtype gtypes in
        flatten (ChoiceG (r, g_))
    | MixedChoiceG gtypes ->
        let gtypes = List.map ~f:flatten_gtype gtypes in
        flatten (MixedChoiceG gtypes)
    | (EndG | TVarG _) as g -> g
    | MuG (tvar, g_) ->
        let gtype =
          flatten_recursion tvar (Set.empty (module TypeVariableName)) g_
        in
        let gtype = flatten_gtype gtype in
        MuG (tvar, gtype)
    | CallG (caller, protocol, roles, g_) ->
        CallG (caller, protocol, roles, flatten_gtype g_)
  in
  let rec remove_idle_choices tvar_mapping = function
    | MessageG (m, r1, r2, g_) ->
        MessageG (m, r1, r2, remove_idle_choices tvar_mapping g_)
    | ChoiceG (r, gtypes) ->
        let non_idle_gtypes =
          List.filter gtypes ~f:(fun g ->
              Set.length (first_actions tvar_mapping g) > 0)
        in
        if List.is_empty non_idle_gtypes then EndG
        else ChoiceG (r, non_idle_gtypes)
    | MixedChoiceG gtypes ->
        let non_idle_gtypes =
          List.filter gtypes ~f:(fun g ->
              Set.length (first_actions tvar_mapping g) > 0)
        in
        if List.is_empty non_idle_gtypes then EndG
        else MixedChoiceG non_idle_gtypes
    | (EndG | TVarG _) as g -> g
    | MuG (tvar, g_) ->
        let gtype = remove_idle_choices tvar_mapping g_ in
        MuG (tvar, gtype)
    | CallG (caller, protocol, roles, g_) ->
        CallG (caller, protocol, roles, remove_idle_choices tvar_mapping g_)
  in
  let g = flatten_gtype g in
  let mapping = build_tvar_mapping g in
  remove_idle_choices mapping g

type union_find_elem = (t list * int) UnionFind.elem

type union_find =
  { leaders: (Int.t, union_find_elem, Int.comparator_witness) Map.t
  ; elems: (RoleName.t, union_find_elem, RoleName.comparator_witness) Map.t
  ; uid: int }

let split_mchoice_branches tvar_mapping gtypes =
  (* Assume that all gtypes have some actions *)
  let add_gtype ({leaders; elems; uid} as ufind) gtype =
    let add_branch root =
      let gtypes, uid = UnionFind.get root in
      UnionFind.set root (gtype :: gtypes, uid)
    in
    let fst_actions = first_actions tvar_mapping gtype in
    if Set.length fst_actions > 1 then
      uerr
        (InvalidMixedChoice
           "Every branch in a mixed choice should have at most one first \
            action")
      |> raise ;
    let r1, r2, _ = Set.choose_exn fst_actions in
    let elem1 = Map.find elems r1 in
    let elem2 = Map.find elems r2 in
    match (elem1, elem2) with
    | Some e1, Some e2 ->
        let root1 = UnionFind.find e1 in
        let root2 = UnionFind.find e2 in
        if UnionFind.eq root1 root2 then (add_branch root1 ; ufind)
        else
          let gtypes1, uid1 = UnionFind.get root1 in
          let gtypes2, uid2 = UnionFind.get root2 in
          let new_root = UnionFind.union root1 root2 in
          let _, uid = UnionFind.get new_root in
          UnionFind.set new_root (gtype :: (gtypes1 @ gtypes2), uid) ;
          if uid = uid1 then
            let leaders = Map.remove leaders uid2 in
            {ufind with leaders}
          else
            let leaders = Map.remove leaders uid1 in
            {ufind with leaders}
    | Some e1, None ->
        let root = UnionFind.find e1 in
        add_branch root ;
        let elems = Map.add_exn elems ~key:r2 ~data:root in
        {ufind with elems}
    | None, Some e2 ->
        let root = UnionFind.find e2 in
        add_branch root ;
        let elems = Map.add_exn elems ~key:r1 ~data:root in
        {ufind with elems}
    | None, None ->
        let root = UnionFind.make ([gtype], uid) in
        let elems = Map.add_exn elems ~key:r1 ~data:root in
        let elems = Map.add_exn elems ~key:r2 ~data:root in
        let leaders = Map.add_exn leaders ~key:uid ~data:root in
        {leaders; elems; uid= uid + 1}
  in
  let ufind =
    { leaders= Map.empty (module Int)
    ; elems= Map.empty (module RoleName)
    ; uid= 0 }
  in
  let {leaders; _} = List.fold ~init:ufind ~f:add_gtype gtypes in
  let ufind_elems = Map.data leaders in
  let gtypes =
    List.map ufind_elems ~f:(fun e ->
        let gtypes, _ = UnionFind.get e in
        gtypes)
  in
  gtypes
