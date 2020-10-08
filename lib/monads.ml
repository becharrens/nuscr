open Err

type 'a res = Yes of 'a | No of user_error

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val fail : Err.user_error -> 'a t

  val map : ('a -> 'b t) -> 'a list -> 'b list t
end

module type READER = sig
  include MONAD

  type r

  val run : 'a t -> r -> 'a res

  val local : (r -> r) -> 'a t -> 'a t

  val ask : r t
end

module Monad (* : MONAD *) = struct
  type 'a t = 'a res

  let return x = Yes x

  let bind = function Yes x -> fun f -> f x | No err -> fun _ -> No err

  let ( >>= ) = bind

  let ( let* ) = bind

  let fail err = No err

  let rec map (f : 'a -> 'b t) (l : 'a list) : 'b list t =
    match l with
    | x :: xs ->
        let* x' = f x in
        let* xs' = map f xs in
        return (x' :: xs')
    | [] -> return []
end

module Reader (Q : sig
  type r
end) =
(* : READER *)
struct
  type r = Q.r

  type 'a t = T of (r -> 'a res)

  let run = function T r -> r

  let return x = T (fun _ -> Yes x)

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    T
      (fun env ->
        match run m env with Yes m' -> run (f m') env | No err -> No err)

  let ( >>= ) = bind

  let ( let* ) = bind

  let fail err = T (fun _ -> No err)

  let local f m = T (fun env -> run m (f env))

  let ask = T (fun r -> Yes r)

  let rec map (f : 'a -> 'b t) (l : 'a list) : 'b list t =
    match l with
    | x :: xs ->
        let* x' = f x in
        let* xs' = map f xs in
        return (x' :: xs')
    | [] -> return []
end

module State (Q : sig
  type s
end) =
struct
  type s = Q.s

  type 'a t = T of (s -> s * 'a res)

  let run = function T r -> r

  let return x = T (fun s -> (s, Yes x))

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    T
      (fun s ->
        match run m s with
        | s, Yes m' -> run (f m') s
        | s, No err -> (s, No err))

  let ( >>= ) = bind

  let ( let* ) = bind

  let fail err = T (fun s -> (s, No err))

  let local f m =
    T
      (fun s ->
        let _, r = run m (f s) in
        (s, r))

  let get = T (fun s -> (s, Yes s))

  let set s = T (fun _ -> (s, Yes ()))

  let rec map (f : 'a -> 'b t) (l : 'a list) : 'b list t =
    match l with
    | x :: xs ->
        let* x' = f x in
        let* xs' = map f xs in
        return (x' :: xs')
    | [] -> return []
end

module ReaderState (Q : sig
  type st

  type rd
end) =
struct
  type s = Q.st (* state type *)

  type r = Q.rd (* reader type *)

  type 'a t = T of (s * r -> s * 'a res)

  let run = function T r -> r

  let return x = T (fun (s, _) -> (s, Yes x))

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    T
      (fun (s, r) ->
        match run m (s, r) with
        | s', Yes m' -> run (f m') (s', r)
        | s', No err -> (s', No err))

  let ( >>= ) = bind

  let ( let* ) = bind

  let fail err = T (fun (s, _) -> (s, No err))

  let local f m =
    T
      (fun (s, r) ->
        let s', r' = run m (s, f r) in
        (s', r'))

  let ask = T (fun (s, r) -> (s, Yes r))

  let get = T (fun (s, _) -> (s, Yes s))

  let set s = T (fun _ -> (s, Yes ()))

  let rec map (f : 'a -> 'b t) (l : 'a list) : 'b list t =
    match l with
    | x :: xs ->
        let* x' = f x in
        let* xs' = map f xs in
        return (x' :: xs')
    | [] -> return []

  let rec concat ll = List.concat ll |> return
end
