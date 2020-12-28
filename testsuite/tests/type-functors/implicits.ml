(* TEST
   * expect
*)

module type Monad = sig
  type _ t
  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
end;;

[%%expect{|
module type Monad =
  sig
    type _ t
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end
|}]

let return {M : Monad} x = M.return x;;

[%%expect{|
val return : {M : Monad} -> 'a -> 'a M.t = <fun>
|}]

let return_fail_no_instances () = return {_} 15;;

[%%expect{|
Line 1, characters 42-43:
1 | let return_fail_no_instances () = return {_} 15;;
                                              ^
Error: This implicit argument is ambiguous.
       No candidate instances were found.
       Hint: Consider passing the desired instance directly.
|}]

let return_one_instance {M : Monad} = return {_} 15;;

[%%expect{|
val return_one_instance : {M : Monad} -> int M.t = <fun>
|}]

let return_fail_two_instances {M1 : Monad} {M2 : Monad} = return {_} 15;;

[%%expect{|
Line 1, characters 66-67:
1 | let return_fail_two_instances {M1 : Monad} {M2 : Monad} = return {_} 15;;
                                                                      ^
Error: This implicit argument is ambiguous.
       Could not choose between the candidates: M2 M1.
       Hint: Consider passing the desired instance directly.
|}]

let return_several {M : Monad} =
  let x = return {_} 15 in
  let y = return {_} () in
  let z = return {_} [1; 2; 3] in
  (x, y, z);;

[%%expect{|
val return_several : {M : Monad} -> int M.t * unit M.t * int list M.t = <fun>
|}]

let map {M : Monad} = M.map;;

[%%expect{|
val map : {M : Monad} -> 'a M.t -> f:('a -> 'b) -> 'b M.t = <fun>
|}]

let map_resolve {M : Monad} (x : _ M.t) =
  let map = map {_} ~f:((+) 1) in
  ignore (map : int M.t -> int M.t);
  map x;;

[%%expect{|
val map_resolve : {M : Monad} -> int M.t -> int M.t = <fun>
|}]

module type Option_monad = Monad with type 'a t = 'a option

[%%expect{|
module type Option_monad =
  sig
    type 'a t = 'a option
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end
|}]

let type_resolve {M : Option_monad} =
  let x = return {_} 15 in
  let y = return {_} () in
  let z = return {_} [1; 2; 3] in
  match x, y, z with
  | Some x, Some y, Some z -> Some (x, y, z)
  | _ -> None;;

[%%expect{|
val type_resolve : {M : Option_monad} -> (int * unit * int list) option =
  <fun>
|}]

module Option_m = struct
  type 'a t = 'a option
  let return x = Some x
  let map x ~f = Option.map f x
  let bind x ~f = Option.bind x f
end

[%%expect{|
module Option_m :
  sig
    type 'a t = 'a option
    val return : 'a -> 'a option
    val map : 'a option -> f:('a -> 'b) -> 'b option
    val bind : 'a option -> f:('a -> 'b option) -> 'b option
  end
|}]

let apply_return_several = return_several {Option_m};;

[%%expect{|
val apply_return_several :
  int Option_m.t * unit Option_m.t * int list Option_m.t =
  (Some 15, Some (), Some [1; 2; 3])
|}]

let apply_type_resolve = type_resolve {Option_m};;

[%%expect{|
val apply_type_resolve : (int * unit * int list) option =
  Some (15, (), [1; 2; 3])
|}]

module Empty = struct end;;

[%%expect{|
module Empty : sig end
|}]

let apply_fail = return_several {Empty};;

[%%expect{|
Line 1, characters 17-39:
1 | let apply_fail = return_several {Empty};;
                     ^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match: sig end is not included in Monad
       The value `bind' is required but not provided
       The value `map' is required but not provided
       The value `return' is required but not provided
       The type `t' is required but not provided
|}]

let type_mismatch = (return_several {_} : bool);;

[%%expect{|
Line 1, characters 21-39:
1 | let type_mismatch = (return_several {_} : bool);;
                         ^^^^^^^^^^^^^^^^^^
Error: This expression has type int ?M.t * unit ?M.t * int list ?M.t
       but an expression was expected of type bool
|}]

let level_lower_excludes_candidates {M : Monad} =
  let x = ref None in
  fun {N : Monad} ->
    (* This assignment lowers the level of the implicit argument's [t] below
       that of N, so it should be excluded as a candidate.
    *)
    x := Some (return {_} 15);
    !x;;

[%%expect{|
val level_lower_excludes_candidates :
  {M : Monad} -> {N : Monad} -> int M.t option = <fun>
|}]

let level_lower_excludes_candidates_fail () =
  let x = ref None in
  fun {N : Monad} ->
    (* This assignment lowers the level of the implicit argument's [t] below
       that of N, so it should be excluded as a candidate.
    *)
    x := Some (return {_} 15);
    !x;;

[%%expect{|
Line 7, characters 23-24:
7 |     x := Some (return {_} 15);
                           ^
Error: This implicit argument is ambiguous.
       No candidate instances were found.
       Hint: Consider passing the desired instance directly.
|}]

let level_lower_expands_type () =
  let x = ref None in
  fun {N : Option_monad} ->
    (* This assignment lowers the level of the implicit argument's [t] below
       that of N, but it should still be accepted as a candidate by expanding
       ['a t = 'a option].
    *)
    x := Some (return {_} 15);
    !x;;

[%%expect{|
val level_lower_expands_type :
  unit -> {N : Option_monad} -> int option option = <fun>
|}]

let map {M : Monad} = M.map;;

let bind {M : Monad} = M.bind;;

[%%expect{|
val map : {M : Monad} -> 'a M.t -> f:('a -> 'b) -> 'b M.t = <fun>
val bind : {M : Monad} -> 'a M.t -> f:('a -> 'b M.t) -> 'b M.t = <fun>
|}]

let expression_disambiguation {M : Monad} {O : Option_monad} =
  return {_} 15 = Some 15;;

[%%expect{|
val expression_disambiguation : {M : Monad} -> {O : Option_monad} -> bool =
  <fun>
|}]

let expression_disambiguation_fail {M : Monad} =
  return {_} 15 = Some 15;;

[%%expect{|
Line 2, characters 10-11:
2 |   return {_} 15 = Some 15;;
              ^
Error: This implicit argument is ambiguous.
       No candidate instances were found.
       Considered constraints:
         int ?M.t = int option.
       Hint: Consider passing the desired instance directly.
|}]

let nested_types {M : Monad} {N : Monad} (x : int M.t N.t M.t) = x;;

[%%expect{|
val nested_types :
  {M : Monad} -> {N : Monad} -> int M.t N.t M.t -> int M.t N.t M.t = <fun>
|}]

let apply_nested_types {M : Monad} = nested_types {_} {_};;

[%%expect{|
val apply_nested_types : {M : Monad} -> int M.t M.t M.t -> int M.t M.t M.t =
  <fun>
|}]

let apply_nested_types_option {O : Option_monad} = nested_types {_} {_};;

[%%expect{|
val apply_nested_types_option :
  {O : Option_monad} -> int O.t O.t O.t -> int O.t O.t O.t = <fun>
|}]

let applied_nested_types_option =
  apply_nested_types_option {Option_m} (Some (Some (Some 15)));;

[%%expect{|
val applied_nested_types_option : int Option_m.t Option_m.t Option_m.t =
  Some (Some (Some 15))
|}]

let applied_nested_types_option_fail =
  apply_nested_types_option {Option_m} (Some (Some 15));;

[%%expect{|
Line 2, characters 51-53:
2 |   apply_nested_types_option {Option_m} (Some (Some 15));;
                                                       ^^
Error: This expression has type int but an expression was expected of type
         int Option_m.t = int option
|}]

module type Functor = sig
  type _ t
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module Free_monad (F : Functor) = struct
  type 'a t = Pure of 'a | Free of 'a t F.t

  let return x = Pure x

  let rec map t ~f =
    match t with
    | Pure x -> Pure (f x)
    | Free tf -> Free (F.map tf ~f:(map ~f))

  let rec bind t ~f =
    match t with
    | Pure x -> f x
    | Free tf -> Free (F.map tf ~f:(bind ~f))
end

[%%expect{|
module type Functor = sig type _ t val map : 'a t -> f:('a -> 'b) -> 'b t end
module Free_monad :
  functor (F : Functor) ->
    sig
      type 'a t = Pure of 'a | Free of 'a t F.t
      val return : 'a -> 'a t
      val map : 'a t -> f:('a -> 'b) -> 'b t
      val bind : 'a t -> f:('a -> 'b t) -> 'b t
    end
|}]

let map2 {F : Functor} x y ~f =
  let module M = Free_monad(F) in
  bind {M} x ~f:(fun x -> map {M} y ~f:(f x))

[%%expect{|
val map2 :
  {F : Functor} ->
  'a Free_monad(F).t ->
  'b Free_monad(F).t -> f:('a -> 'b -> 'c) -> 'c Free_monad(F).t = <fun>
|}]

module type Functor_list = Functor with type 'a t = 'a list;;

[%%expect{|
module type Functor_list =
  sig type 'a t = 'a list val map : 'a t -> f:('a -> 'b) -> 'b t end
|}]

let pure {F : Functor_list} x =
  let module M = Free_monad(F) in
  M.Pure x;;

let free {F : Functor_list} x =
  let module M = Free_monad(F) in
  M.Free x;;

[%%expect{|
val pure : {F : Functor_list} -> 'a -> 'a Free_monad(F).t = <fun>
val free : {F : Functor_list} -> 'a Free_monad(F).t F.t -> 'a Free_monad(F).t =
  <fun>
|}]

let apply_functor_type {F : Functor_list} =
  map2 {_} ~f:(fun x y -> x + y)
    (free {_} [pure {_} 1; pure {_} 2; pure {_} 3])
    (free {_} [pure {_} 5; pure {_} 6; pure {_} 7]);;

[%%expect{|
val apply_functor_type : {F : Functor_list} -> int Free_monad(F).t = <fun>
|}]

module List_f = struct
  type 'a t = 'a list
  let rec map xs ~f =
    match xs with
    | [] -> []
    | x :: xs -> f x :: map xs ~f
end

let applied_functor_type = apply_functor_type {List_f};;

[%%expect{|
module List_f :
  sig type 'a t = 'a list val map : 'a list -> f:('a -> 'b) -> 'b list end
val applied_functor_type : int Free_monad(List_f).t =
  Free_monad(List_f).Free
   [Free_monad(List_f).Free
     [Free_monad(List_f).Pure 6; Free_monad(List_f).Pure 7;
      Free_monad(List_f).Pure 8];
    Free_monad(List_f).Free
     [Free_monad(List_f).Pure 7; Free_monad(List_f).Pure 8;
      Free_monad(List_f).Pure 9];
    Free_monad(List_f).Free
     [Free_monad(List_f).Pure 8; Free_monad(List_f).Pure 9;
      Free_monad(List_f).Pure 10]]
|}]

let apply_functor_type_fail {F : Functor} =
  map2 {_} ~f:(fun x y -> x + y)
    (free {_} [pure {_} 1; pure {_} 2; pure {_} 3])
    (free {_} [pure {_} 5; pure {_} 6; pure {_} 7]);;

[%%expect{|
Line 3, characters 11-12:
3 |     (free {_} [pure {_} 1; pure {_} 2; pure {_} 3])
               ^
Error: This implicit argument is ambiguous.
       No candidate instances were found.
       Considered constraints:
         'a Free_monad(?F).t = int Free_monad(F).t
         int Free_monad(?F/2).t = 'a Free_monad(?F/1).t
         int Free_monad(?F/3).t = 'a Free_monad(?F/1).t
         int Free_monad(?F/4).t = 'a Free_monad(?F/1).t.
       Hint: Consider passing the desired instance directly.
|}]
