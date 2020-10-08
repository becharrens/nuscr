open! Base
open Names

(** Global types *)

type payload =
  | PValue of VariableName.t option * PayloadTypeName.t
  | PDelegate of ProtocolName.t * RoleName.t
[@@deriving eq, sexp_of, show, ord]

type message = {label: LabelName.t; payload: payload list}
[@@deriving eq, sexp_of, show, ord]

val equal_pvalue_payload : payload -> payload -> bool

module type S = sig
  type t [@@deriving sexp_of]

  val show : t -> string

  val create : RoleName.t -> RoleName.t -> message -> t

  val participants : t -> (RoleName.t, RoleName.comparator_witness) Set.t

  include Comparable.S with type t := t
end

module GAction : S

(** The type of global types *)
type t =
  | MessageG of message * RoleName.t * RoleName.t * t
      (** [MessageG (msg, sender, receiver, t)] starts by sending message
          [msg] from [sender] to [receiver] and continues as [t] *)
  | MuG of TypeVariableName.t * t  (** Fixpoint *)
  | TVarG of TypeVariableName.t  (** Recursive variable *)
  | ChoiceG of RoleName.t * t list
      (** [ChoiceG (name, ts)] expresses a choice located at participant
          [name] between the [ts] *)
  | MixedChoiceG of t list
      (** [ChoiceG (ts)] expresses a mixed choice between the [ts] *)
  | EndG  (** Empty global type *)
  | CallG of RoleName.t * ProtocolName.t * RoleName.t list * t
      (** [CallG (caller, protocol, participants, t)] - [caller] calls
          [protocol], inviting [participants] to carry out the roles in
          [protocol] (dynamic roles in nested protocols are not included) *)

(** Mapping of protocol name to the roles ('static' participants, dynamic
    participants) participating in the protocol, the names of the nested
    protocols defined inside it and its global type*)
type global_t =
  ( ProtocolName.t
  , (RoleName.t list * RoleName.t list) * ProtocolName.t list * t
  , ProtocolName.comparator_witness )
  Map.t

val show : t -> string
(** Provides a textual representation of a global type *)

val show_global_t : global_t -> string
(** Provides a textual representation of a global type with nested protocols *)

val call_label :
  RoleName.t -> ProtocolName.t -> RoleName.t list -> LabelName.t
(** Generates a unique label for a protocol call based on the caller, the
    protocol called and the participants involved *)

val of_protocol : Syntax.global_protocol -> t
(** Turn a raw protocol (from the parser) into a global type *)

val global_t_of_module : Syntax.scr_module -> global_t
(** Turn scribble module (from the parser) into a global type *)

val normalise : t -> t
(** Normalise a global type. This mainly collapses nested choice on the same
    participant and unfolds fixpoints *)

val normalise_global_t : global_t -> global_t
(** Apply normalisation to all protocols in global_t *)

val replace_recursion_with_nested_protocols : global_t -> global_t
(** Replace the MuG type with explicit calls to nested protocols*)

val normalise_and_flatten : t -> t
(** Replace consecutive recursive constructs by a single one (renaming tvars)
    and flatten directed and mixed choices, while removing branches idle
    branches (branches with no actions) *)

val make_unique_tvars : t -> t
(** Rename recursive variables in the protocol to ensure that they are
    globally unique *)

val build_tvar_mapping :
  t -> (TypeVariableName.t, t, TypeVariableName.comparator_witness) Map.t
(**  *)

val first_actions :
     (TypeVariableName.t, t, TypeVariableName.comparator_witness) Map.t
  -> t
  -> (GAction.t, GAction.comparator_witness) Set.t
(** Return the set of first actions which can be carried out in the global
    type *)

val get_decision_roles :
     (TypeVariableName.t, t, TypeVariableName.comparator_witness) Map.t
  -> t
  -> (RoleName.t, RoleName.comparator_witness) Set.t

val split_mchoice_branches :
     (TypeVariableName.t, t, TypeVariableName.comparator_witness) Map.t
  -> t list
  -> t list list
(** Given a list of the global types in a mixed choice, *)
