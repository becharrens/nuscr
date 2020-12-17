open! Base
open Printf
open Ltype
open Names
open Graph

type action =
  | SendA of RoleName.t * Gtype.message
  | RecvA of RoleName.t * Gtype.message
  | InviteA of RoleName.t list * RoleName.t list * ProtocolName.t
  | AcceptA of
      RoleName.t
      * ProtocolName.t
      * RoleName.t list
      * RoleName.t list
      * RoleName.t
  | Epsilon
[@@deriving ord, sexp_of]

let show_action = function
  | SendA (r, msg) ->
      sprintf "%s!%s" (RoleName.user r) (Gtype.show_message msg)
  | RecvA (r, msg) ->
      sprintf "%s?%s" (RoleName.user r) (Gtype.show_message msg)
  | InviteA (invite_roles, _, protocol) ->
      let str_invite_roles = List.map ~f:RoleName.user invite_roles in
      sprintf "!%s(%s)"
        (ProtocolName.user protocol)
        (String.concat ~sep:"," str_invite_roles)
  | AcceptA (role, protocol, roles, _, caller) ->
      let str_participant_roles = List.map ~f:RoleName.user roles in
      sprintf "%s?%s@%s(%s)" (RoleName.user caller) (RoleName.user role)
        (ProtocolName.user protocol)
        (String.concat ~sep:"," str_participant_roles)
  | Epsilon -> "ε"

module Label = struct
  module M = struct
    type t = action

    let compare = compare_action

    let sexp_of_t = sexp_of_action
  end

  include M
  include Comparator.Make (M)

  let default = Epsilon
end

module G = Persistent.Digraph.ConcreteLabeled (Int) (Label)

type t = G.t

type state = int

module Display = struct
  include G

  let vertex_name = Int.to_string

  let graph_attributes _ = []

  let default_vertex_attributes _ = []

  let vertex_attributes _ = []

  let default_edge_attributes _ = []

  let edge_attributes (_, a, _) = [`Label (show_action a)]

  let get_subgraph _ = None
end

module DotOutput = Graphviz.Dot (Display)

type conv_env =
  {g: G.t; tyvars: (TypeVariableName.t * int) list; non_deterministic: bool}

let init_conv_env = {g= G.empty; tyvars= []; non_deterministic= false}

(* (* Redirect all edges into st_remove to st_base, then remove st_remove *)
 * let merge_state g st_base st_remove =
 *   let f (src, action, _dst) g_ =
 *     let g_ = G.add_edge_e g_ (src, action, st_base) in
 *     g_
 *   in
 *   let g = G.fold_pred_e f g st_remove g in
 *   let g = G.remove_vertex g st_remove in
 *   g
 *
 * (* Redirect all edges from st_remove to st_base, then remove st_remove *)
 * let merge_state_rev st_base g st_remove =
 *   let f (_src, action, dst) g_ =
 *     let g_ = G.add_edge_e g_ (st_base, action, dst) in
 *     g_
 *   in
 *   let g = G.fold_succ_e f g st_remove g in
 *   let g = G.remove_vertex g st_remove in
 *   g
 *)

(** Construct the epsilon closure for a given NDA *)

(* let epsilon_closure g = let one_shot = let f node acc = let f edge acc =
   match edge with _, Epsilon, dst -> Set.add acc dst | _ -> acc in let data
   = G.fold_succ_e f g node (Set.singleton (module Int) node) in Map.add_exn
   ~key:node ~data acc in G.fold_vertex f g (Map.empty (module Int)) in let
   iterate_once curr = let updated = ref false in let f ~key:_ ~data = let f
   acc node = let nexts = Map.find_exn curr node in let f acc new_node = if
   Set.mem acc new_node then acc else ( updated := true ; Set.add acc
   new_node ) in Set.fold ~init:acc ~f nexts in Set.fold ~init:data ~f data
   in (Map.mapi ~f curr, not !updated) in let compute_fixpoint () = let rec
   aux curr = let curr, complete = iterate_once curr in if complete then curr
   else aux curr in aux one_shot in compute_fixpoint () *)

module IntSet = struct
  module M = struct
    type t = Set.M(Int).t

    let compare = Set.compare_direct

    let sexp_of_t = Set.sexp_of_m__t (module Int)
  end

  include M
  include Comparator.Make (M)
end

let epsilon_closures g =
  let rec compute_closure visited state =
    let edges = G.succ_e g state in
    List.fold edges ~init:visited ~f:(fun visited -> function
      | _, Epsilon, dst when not (Set.mem visited dst) ->
          let visited = Set.add visited dst in
          compute_closure visited dst
      | _ -> visited)
  in
  G.fold_vertex
    (fun v closures ->
      Map.add_exn closures ~key:v
        ~data:(compute_closure (Set.singleton (module Int) v) v))
    g
    (Map.empty (module Int))

let powerset_construction (start, old_g) =
  let epsilons = epsilon_closures old_g in
  let count = ref 0 in
  let fresh () =
    let n = !count in
    count := n + 1 ;
    n
  in
  let rec aux (g, state_map) states =
    match Map.find state_map states with
    | Some _ -> (g, state_map)
    | None ->
        let st = fresh () in
        let g = G.add_vertex g st in
        let state_map = Map.add_exn ~key:states ~data:st state_map in
        let f acc old_node =
          let edges = G.succ_e old_g old_node in
          let f acc (_, lbl, dst) =
            match lbl with
            | Epsilon -> acc
            | _ ->
                let dst = Map.find_exn epsilons dst in
                let f = function
                  | None -> dst
                  | Some states -> Set.union states dst
                in
                Map.update acc lbl ~f
          in
          List.fold ~init:acc ~f edges
        in
        let out_edges =
          Set.fold ~init:(Map.empty (module Label)) ~f states
        in
        let f ~key:label ~data:states (g, state_map) =
          let g, state_map = aux (g, state_map) states in
          let g =
            G.add_edge_e g (st, label, Map.find_exn state_map states)
          in
          (g, state_map)
        in
        Map.fold out_edges ~init:(g, state_map) ~f
  in
  let start_states = Map.find_exn epsilons start in
  Stdio.print_endline
  @@ ( String.concat ~sep:"," @@ List.map ~f:Int.to_string
     @@ Set.to_list start_states )
  ^ " " ^ Int.to_string start ;
  let g, state_map = aux (G.empty, Map.empty (module IntSet)) start_states in
  (Map.find_exn state_map start_states, g)

let show g =
  let buffer = Buffer.create 4196 in
  let formatter = Caml.Format.formatter_of_buffer buffer in
  DotOutput.fprint_graph formatter g ;
  Caml.Format.pp_print_flush formatter () ;
  Buffer.contents buffer

(* let convert_to_dfa (old_start, old_g) = let count = ref 0 in let fresh ()
   = let n = !count in count := n + 1 ; n in let epsilons = epsilon_closures
   old_g in let rec add_edges g all_states state state_key = let out_edges =
   Set.fold state_key ~init:(Map.empty (module Label)) ~f:(fun transitions v
   -> let out_edges = G.succ_e old_g v in List.fold out_edges
   ~init:transitions ~f:(fun transitions edge -> match edge with | _,
   Epsilon, _ -> transitions | _, label, dst -> let epsilon_dst =
   Map.find_exn epsilons dst in Map.update transitions label ~f:(function |
   None -> epsilon_dst | Some next_states -> Set.union next_states
   epsilon_dst))) in Map.fold out_edges ~init:(g, all_states) ~f:(fun
   ~key:label ~data:next_state_key (g, all_states) -> match Map.find
   all_states next_state_key with | None -> let next_state = fresh () in let
   g = G.add_edge_e g (state, label, next_state) in let all_states =
   Map.add_exn all_states ~key:next_state_key ~data:next_state in let g,
   all_states = add_edges g all_states next_state next_state_key in (g,
   all_states) | Some next_state -> let g = G.add_edge_e g (state, label,
   next_state) in (g, all_states)) in let g = G.empty in let start = fresh ()
   in let state_key = Map.find_exn epsilons old_start in let g = G.add_vertex
   g start in let all_states = Map.empty (module IntSet) in let g, _ =
   add_edges g all_states start state_key in (start, g) *)

let of_local_type lty =
  let count = ref 0 in
  let fresh () =
    let n = !count in
    count := n + 1 ;
    n
  in
  let rec conv_ltype_aux env =
    let {g; tyvars; _} = env in
    function
    | RecvL (m, n, l) ->
        let curr = fresh () in
        let env, next = conv_ltype_aux env l in
        let g = env.g in
        let g = G.add_vertex g curr in
        let e = (curr, RecvA (n, m), next) in
        let g = G.add_edge_e g e in
        ({env with g}, curr)
    | SendL (m, n, l) ->
        let curr = fresh () in
        let env, next = conv_ltype_aux env l in
        let e = (curr, SendA (n, m), next) in
        let g = env.g in
        let g = G.add_vertex g curr in
        let g = G.add_edge_e g e in
        ({env with g}, curr)
    | ChoiceL (_r, ltys) ->
        let curr = fresh () in
        let env, nexts = List.fold_map ~f:conv_ltype_aux ~init:env ltys in
        let g = env.g in
        let es = List.map ~f:(fun n -> (curr, Epsilon, n)) nexts in
        let g = G.add_vertex g curr in
        let g = List.fold ~f:G.add_edge_e ~init:g es in
        ({env with g; non_deterministic= true}, curr)
    | EndL ->
        let curr = fresh () in
        let g = G.add_vertex g curr in
        ({env with g}, curr)
    | MuL (tv, l) ->
        let new_st = fresh () in
        let g = G.add_vertex g new_st in
        let env =
          {tyvars= (tv, new_st) :: tyvars; g; non_deterministic= true}
        in
        let env, curr = conv_ltype_aux env l in
        let g = env.g in
        let g = G.add_edge_e g (new_st, Epsilon, curr) in
        ({env with g}, curr)
    | TVarL tv ->
        (env, List.Assoc.find_exn ~equal:TypeVariableName.equal env.tyvars tv)
    | AcceptL (role, protocol, roles, new_roles, caller, l) ->
        let curr = fresh () in
        let env, next = conv_ltype_aux env l in
        let e =
          (curr, AcceptA (role, protocol, roles, new_roles, caller), next)
        in
        let g = env.g in
        let g = G.add_vertex g curr in
        let g = G.add_edge_e g e in
        ({env with g}, curr)
    | InviteCreateL (invite_roles, create_roles, protocol, l) ->
        let curr = fresh () in
        let env, next = conv_ltype_aux env l in
        let e =
          (curr, InviteA (invite_roles, create_roles, protocol), next)
        in
        let g = env.g in
        let g = G.add_vertex g curr in
        let g = G.add_edge_e g e in
        ({env with g}, curr)
  in
  let env, start = conv_ltype_aux init_conv_env lty in
  let g = env.g in
  if env.non_deterministic then powerset_construction (start, g)
  else (start, g)

let rec normalise_ltype ltype =
  let rec has_tvar tvar = function
    | EndL -> false
    | TVarL tvar' -> TypeVariableName.equal tvar tvar'
    | RecvL (_, _, l) -> has_tvar tvar l
    | SendL (_, _, l) -> has_tvar tvar l
    | AcceptL (_, _, _, _, _, l) -> has_tvar tvar l
    | InviteCreateL (_, _, _, l) -> has_tvar tvar l
    | ChoiceL (_, ltypes) ->
        let open Container.Continue_or_stop in
        List.fold_until ltypes ~init:false
          ~f:(fun _ l ->
            if has_tvar tvar l then Stop true else Continue false)
          ~finish:(fun has_var -> has_var)
    | MuL (_, l) -> has_tvar tvar l
  in
  match ltype with
  | (EndL | TVarL _) as g -> g
  | RecvL (msg, sender, l) -> RecvL (msg, sender, normalise_ltype l)
  | SendL (msg, recv, l) -> RecvL (msg, recv, normalise_ltype l)
  | AcceptL (role, protocol, roles, new_roles, caller, l) ->
      AcceptL (role, protocol, roles, new_roles, caller, normalise_ltype l)
  | InviteCreateL (invite_roles, create_roles, protocol, l) ->
      InviteCreateL (invite_roles, create_roles, protocol, normalise_ltype l)
  | ChoiceL (role, ltypes) ->
      ChoiceL (role, List.map ltypes ~f:normalise_ltype)
  | MuL (tvar, l) ->
      let l = normalise_ltype l in
      if has_tvar tvar l then MuL (tvar, l) else l

let to_local_type role state g =
  let count = ref 0 in
  let fresh () =
    let n = !count in
    count := n + 1 ;
    n
  in
  let rec_var_name rec_variables state =
    let tvar_name id = TypeVariableName.of_string @@ sprintf "t%d" id in
    match Map.find rec_variables state with
    | Some tvar_id -> (rec_variables, tvar_name tvar_id)
    | None ->
        let tvar_id = fresh () in
        let rec_variables =
          Map.add_exn rec_variables ~key:state ~data:tvar_id
        in
        (rec_variables, tvar_name tvar_id)
  in
  let rec is_recursive g state target visited =
    let open Container.Continue_or_stop in
    if Set.mem visited state then (visited, false)
    else
      let visited = Set.add visited state in
      let next_states = G.succ g state in
      let visited, is_rec =
        List.fold_until next_states ~init:(visited, false)
          ~f:(fun (visited, _) next_state ->
            if target = next_state then Stop (visited, true)
            else
              let visited, is_rec =
                is_recursive g next_state target visited
              in
              if is_rec then Stop (visited, is_rec)
              else Continue (visited, false))
          ~finish:(fun acc -> acc)
      in
      (visited, is_rec)
  in
  let rec to_ltype visited g rec_variables state =
    if Set.mem visited state then
      let rec_variables, tvar = rec_var_name rec_variables state in
      (rec_variables, TVarL tvar)
    else
      let visited = Set.add visited state in
      let succ_edges = G.succ_e g state in
      let rec_variables, branches =
        List.fold_map succ_edges ~init:rec_variables
          ~f:(fun rec_variables (_, lbl, dst) ->
            let rec_variables, cont = to_ltype visited g rec_variables dst in
            match lbl with
            | SendA (r, msg) -> (rec_variables, SendL (msg, r, cont))
            | RecvA (r, msg) -> (rec_variables, RecvL (msg, r, cont))
            | InviteA (invite_roles, create_roles, protocol) ->
                ( rec_variables
                , InviteCreateL (invite_roles, create_roles, protocol, cont)
                )
            | AcceptA (role, protocol, roles, new_roles, caller) ->
                ( rec_variables
                , AcceptL (role, protocol, roles, new_roles, caller, cont) )
            | Epsilon ->
                Err.Violation "DFA should not contain epsilon transitions"
                |> raise)
      in
      let ltype =
        match branches with
        | [] -> EndL
        | [l] -> l
        | SendL _ :: _ -> ChoiceL (role, branches)
        | InviteCreateL _ :: _ -> ChoiceL (role, branches)
        | AcceptL (_, _, _, _, caller, _) :: _ -> ChoiceL (caller, branches)
        | RecvL (_, r, _) :: _ -> ChoiceL (r, branches)
        | _ ->
            Err.Violation
              "Branches of a choice should start with a message exchange"
            |> raise
      in
      let _, is_rec = is_recursive g state state (Set.empty (module Int)) in
      if is_rec then
        let rec_variables, tvar = rec_var_name rec_variables state in
        (rec_variables, MuL (tvar, ltype))
      else (rec_variables, ltype)
  in
  let _, ltype =
    to_ltype (Set.empty (module Int)) g (Map.empty (module Int)) state
  in
  normalise_ltype ltype
