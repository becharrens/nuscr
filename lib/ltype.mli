(** Local type management *)

(** This module defines local types and basic operations on them. *)
open! Base

open Names

module type RoleSet = sig
  type t = Set.M(RoleName).t

  val equal : t -> t -> bool

  val sexp_of_t : t -> Sexp.t
end

(** Module which defines equal and sexp_of functions for a set of role names *)
module RoleNameSet : RoleSet

(** Local types. *)
type t =
  | RecvL of Gtype.message * RoleName.t * t
      (** [RecvL (msg, name, t)] waits for message [msg] from [name] and
          continues as [t] *)
  | SendL of Gtype.message * RoleName.t * t
      (** [SendL (msg, name, t)] sends message [msg] to [name] and continues
          as [t] *)
  | ChoiceL of RoleName.t * t list
      (** [ChoiceL (name, ts)] is a choice (internal or external) from [name]
          between the [ts] *)
  | UnmergedMixedChoiceL of mIndependentChoice list
      (** [UnmergedMixedChoiceL id_choices] is a mixed choice between
          different independent choices [id_choices] where the all the
          branches in each independent choice have not been merged *)
  (* | UnmergedMixedChoiceL of mIndependentChoice list *)
  | MixedChoiceL of t list
      (** [MixedChoiceL ts] is a mixed choice between the [ts] after merging
          all the branches*)
  | TVarL of TypeVariableName.t  (** Recursive variable *)
  | MuL of TypeVariableName.t * t  (** Fixpoint *)
  | EndL  (** Empty type *)
  | InviteCreateL of RoleName.t list * RoleName.t list * ProtocolName.t * t
      (** Send invitations to existing roles and set up/create dynamic
          pariticipants *)
  | AcceptL of
      RoleName.t
      * ProtocolName.t
      * RoleName.t list
      * RoleName.t list
      * RoleName.t
      * t  (** accept role'\@Proto(roles...; new roles'...) from X; t *)

(** An independent choice groups all branches of a mixed choice where the
    participants of the first interactions of any two branches have at least
    1 participant in common (transitively) - e.g [a->b + b->c + c->d] would
    all be under the same independent choice *)
and mIndependentChoice =
  (t * RoleNameSet.t) list * (t * RoleNameSet.t) list * bool

module type S = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  val create : ProtocolName.t -> RoleName.t -> t

  include Comparable.S with type t := t
end

(** Unique id identifying a local protocol *)
module LocalProtocolId : S

(** Mapping of local protocol id to the protocol's roles and local type *)
type local_t =
  ( LocalProtocolId.t
  , RoleName.t list * t
  , LocalProtocolId.comparator_witness )
  Map.t

val show : t -> string
(** Converts a local type to a string. *)

val show_local_t : local_t -> string

val project : RoleName.t -> Gtype.t -> t
(** Project a global type into a particular role. *)

val project_global_t : Gtype.global_t -> local_t
(** Generate the local protocols for a given global_t *)

val unmerged_project :
     (TypeVariableName.t, Gtype.t, TypeVariableName.comparator_witness) Map.t
  -> RoleName.t
  -> Gtype.t
  -> t
(** Project a global type into a particular role without merging the branches
    of the mixed choices *)

(** Mapping from local protocol ids to their unique local protocol names *)
type local_proto_name_lookup =
  ( LocalProtocolId.t
  , LocalProtocolName.t
  , LocalProtocolId.comparator_witness )
  Map.t

val build_local_proto_name_lookup : local_t -> local_proto_name_lookup
(** Builds a map containing the unique string representations for the unique
    local protocol ids *)

val show_lookup_table : local_proto_name_lookup -> string
(** Converts a local protocol name lookup table to a string *)

val lookup_local_protocol :
     local_proto_name_lookup
  -> ProtocolName.t
  -> RoleName.t
  -> LocalProtocolName.t
(** Return the unique local protocol name for a (role, protocol) pair *)

val lookup_protocol_id :
  local_proto_name_lookup -> LocalProtocolId.t -> LocalProtocolName.t
(** Look up the unique name for a local protocol id *)
