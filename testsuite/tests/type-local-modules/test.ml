(* TEST
   * expect
*)

module type Monad = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val return : 'a -> 'a t
end;;

module Option = struct
  type 'a t = 'a option

  let map x ~f =
    match x with
    | Some x -> Some (f x)
    | None -> None

  let bind x ~f =
    match x with
    | Some x -> f x
    | None -> None

  let return x = Some x
end;;

module List = struct
  type 'a t = 'a list

  let map x ~f = List.map f x

  let bind x ~f =
    List.concat (List.map f x)

  let return x = [x]
end;;

module Int = struct
  type 'a t = int

  let map x ~f:_ = x * 3

  let bind x ~f:_ = x + 2

  let return _ = 1
end;;

module Float = struct
  type 'a t = float

  let map x ~f:_ = x *. 3.

  let bind x ~f:_ = x +. 2.

  let return _ = 1.
end;;

[%%expect{|
module type Monad =
  sig
    type 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
  end
module Option :
  sig
    type 'a t = 'a option
    val map : 'a option -> f:('a -> 'b) -> 'b option
    val bind : 'a option -> f:('a -> 'b option) -> 'b option
    val return : 'a -> 'a option
  end
module List :
  sig
    type 'a t = 'a list
    val map : 'a list -> f:('a -> 'b) -> 'b list
    val bind : 'a list -> f:('a -> 'b list) -> 'b list
    val return : 'a -> 'a list
  end
module Int :
  sig
    type 'a t = int
    val map : int -> f:'a -> int
    val bind : int -> f:'a -> int
    val return : 'a -> int
  end
module Float :
  sig
    type 'a t = float
    val map : float -> f:'a -> float
    val bind : float -> f:'a -> float
    val return : 'a -> float
  end
|}]

let choose (f : {M : Monad} -> 'a M.t -> 'b M.t)
    (g : {N : Monad} -> 'a N.t -> 'b N.t) b =
  if b then f else g;;

[%%expect{|
val choose :
  ({M : Monad} -> 'a M.t -> 'b M.t) ->
  ({N : Monad} -> 'a N.t -> 'b N.t) ->
  bool -> {N : Monad} -> 'a N.t -> 'b N.t = <fun>
|}]

let choose_param_unify (f : {M : Monad} -> 'a M.t -> 'b M.t)
    (g : {N : Monad} -> 'a N.t -> 'a N.t) b =
  if b then f else g;;

[%%expect{|
val choose_param_unify :
  ({M : Monad} -> 'b M.t -> 'b M.t) ->
  ({N : Monad} -> 'b N.t -> 'b N.t) ->
  bool -> {N : Monad} -> 'b N.t -> 'b N.t = <fun>
|}]

let choose_whole_unify (f : {M : Monad} -> 'a M.t -> 'b M.t) g b =
  if b then f else g;;

[%%expect{|
val choose_whole_unify :
  ({M : Monad} -> 'a M.t -> 'b M.t) ->
  ({M : Monad} -> 'a M.t -> 'b M.t) ->
  bool -> {M : Monad} -> 'a M.t -> 'b M.t = <fun>
|}]

type 'a module_param = {M : Monad} -> 'a;;

[%%expect{|
type 'a module_param = {M : Monad} -> 'a
|}]

let no_escape_variable (x : {M : Monad} -> _ M.t) : _ module_param = x;;

[%%expect{|
Line 1, characters 69-70:
1 | let no_escape_variable (x : {M : Monad} -> _ M.t) : _ module_param = x;;
                                                                         ^
Error: This expression has type {M : Monad} -> 'a M.t
       but an expression was expected of type
         'b module_param = {M : Monad} -> 'b
       The type constructor M.t would escape its scope
|}]

let apply_option_module (f : {M : Monad} -> 'a M.t -> int M.t) = f {Option};;

[%%expect{|
val apply_option_module :
  ({M : Monad} -> 'a M.t -> int M.t) -> 'a Option.t -> int Option.t = <fun>
|}]

let apply_int_module (f : {M : Monad} -> 'a M.t -> int M.t) = f {Int};;

[%%expect{|
val apply_int_module : ({M : Monad} -> 'a M.t -> int M.t) -> 'a Int.t -> int =
  <fun>
|}]

let apply_full_option_module (f : {M : Monad} -> 'a M.t -> int M.t) =
  f {Option} (Some true);;

[%%expect{|
val apply_full_option_module :
  ({M : Monad} -> bool M.t -> int M.t) -> int Option.t = <fun>
|}]

let apply_int_module (f : {M : Monad} -> 'a M.t -> int M.t) =
  f {Int} 15;;

[%%expect{|
val apply_int_module : ({M : Monad} -> 'a M.t -> int M.t) -> int = <fun>
|}]

let new_fun {M : Monad} (x : 'a M.t) (y : 'b M.t) =
  M.bind x ~f:(fun x -> M.map y ~f:(fun y -> (x, y)))

[%%expect{|
val new_fun : {M : Monad} -> 'a M.t -> 'b M.t -> ('a * 'b) M.t = <fun>
|}]

let new_fun_list_weak = new_fun {List};;

[%%expect{|
val new_fun_list_weak :
  '_weak1 List.t -> '_weak2 List.t -> ('_weak1 * '_weak2) List.t = <fun>
|}]

let new_fun_list x y = new_fun {List} x y;;

[%%expect{|
val new_fun_list : 'a List.t -> 'b List.t -> ('a * 'b) List.t = <fun>
|}]

module type Carries_sig = sig
  type t

  module type S
end

module Carries_monad = struct
  type t = int

  module type S = Monad
end

[%%expect{|
module type Carries_sig = sig type t module type S end
module Carries_monad : sig type t = int module type S = Monad end
|}]

let carries_param {T : Carries_sig} (f : (module T.S) -> T.t)
    (x : (module T.S)) =
  f x;;

[%%expect{|
val carries_param :
  {T : Carries_sig} -> ((module T.S) -> T.t) -> (module T.S) -> T.t = <fun>
|}]

let apply_carries_param =
  carries_param {Carries_monad} (fun (module M : Carries_monad.S) ->
    let x = ref 80 in
    let y = M.return 15 in
    ignore @@ M.map y ~f:(fun y -> x := !x + y; y);
    !x);;

[%%expect{|
val apply_carries_param : (module Carries_monad.S) -> Carries_monad.t = <fun>
|}]

module type Carries_type_module_fn = sig
  val bind : {M : Monad} -> 'a M.t -> f:('a -> 'b M.t) -> 'b M.t
end

[%%expect{|
module type Carries_type_module_fn =
  sig val bind : {M : Monad} -> 'a M.t -> f:('a -> 'b M.t) -> 'b M.t end
|}]

module Carries_type_module_fn : Carries_type_module_fn = struct
  let bind {M : Monad} = M.bind
end

[%%expect{|
module Carries_type_module_fn : Carries_type_module_fn
|}]

let escape_check (x : 'a) {M : Monad} = M.map x ~f:(fun x -> 15);

[%%expect{|
Line 1, characters 46-47:
1 | let escape_check (x : 'a) {M : Monad} = M.map x ~f:(fun x -> 15);
                                                  ^
Error: This expression has type 'a but an expression was expected of type
         'b M.t
       The type constructor M.t would escape its scope
|}]

let reference = ref None

let reference_escape_check {M : Monad} x =
  reference := Some (M.return x)

[%%expect{|
val reference : '_weak3 option ref = {contents = None}
Line 4, characters 20-32:
4 |   reference := Some (M.return x)
                        ^^^^^^^^^^^^
Error: This expression has type 'a M.t but an expression was expected of type
         'weak3
       The type constructor M.t would escape its scope
|}]

let modules_lambdas_compose {M : Monad} =
  (fun {M : Monad} (f : {M : Monad} -> 'a -> 'a M.t) ->
      f {M})
    {M} (fun {M : Monad} x -> M.return x);;

[%%expect{|
val modules_lambdas_compose : {M : Monad} -> 'a -> 'a M.t = <fun>
|}]

let monad_test {M : Monad} x f1 f2 =
  M.bind ~f:f1 (M.map ~f:f2 (M.return x));;

[%%expect{|
val monad_test : {M : Monad} -> 'a -> ('b -> 'c M.t) -> ('a -> 'b) -> 'c M.t =
  <fun>
|}]

let monad_option_test =
  monad_test {Option} 1 (fun x -> Some (25 + x)) ((+) 15);;

[%%expect{|
val monad_option_test : int Option.t = Some 41
|}]

let monad_list =
  let rec list_init i j = if j > 0 then i :: list_init (i+1) (j-1) else [] in
  monad_test {List} 1 (list_init 1) ((+) 15);;

[%%expect{|
val monad_list : int List.t =
  [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16]
|}]

let monad_int_test = monad_test {Int} 1 (fun x -> 30) ((+) 15);;

[%%expect{|
val monad_int_test : 'a Int.t = 5
|}]

let monad_float_test =
  monad_test {Float} 1 (fun x -> float_of_int x) ((+) 15);;

[%%expect{|
val monad_float_test : 'a Float.t = 5.
|}]

class ['a, 'b] monad = object
  method map {M : Monad} (x : 'a M.t) ~(f : 'a -> 'b) =
    M.map x ~f
  method bind {M : Monad} (x : 'a M.t) ~(f : 'a -> 'b M.t) =
    M.bind x ~f
  method return {M : Monad} (x : 'a) = M.return x
end;;

[%%expect{|
class ['a, 'b] monad :
  object
    method bind : {M : Monad} -> 'a M.t -> f:('a -> 'b M.t) -> 'b M.t
    method map : {M : Monad} -> 'a M.t -> f:('a -> 'b) -> 'b M.t
    method return : {M : Monad} -> 'a -> 'a M.t
  end
|}]

let test_class =
  let m = new monad in
  m#map {Option} (m#return {Option} 15) ~f:((+) 8);;

[%%expect{|
val test_class : int Option.t = Some 23
|}]

class ['a, 'b] module_class_parameter {M : Monad} = object end;;

[%%expect{|
Line 1, characters 38-49:
1 | class ['a, 'b] module_class_parameter {M : Monad} = object end;;
                                          ^^^^^^^^^^^
Error: Modules are not allowed in this pattern.
|}]

let () = (fun (f : {M : Monad} -> 'a -> 'a) -> f) (fun x y -> y);;

[%%expect{|
Line 1, characters 50-64:
1 | let () = (fun (f : {M : Monad} -> 'a -> 'a) -> f) (fun x y -> y);;
                                                      ^^^^^^^^^^^^^^
Error: This function should have type {M : Monad} -> 'a -> 'a
       but its first argument is not labelled
|}]

let () = (fun (f : 'a -> 'b -> 'b) -> f) (fun {M : Monad} y -> y);;

[%%expect{|
Line 1, characters 41-65:
1 | let () = (fun (f : 'a -> 'b -> 'b) -> f) (fun {M : Monad} y -> y);;
                                             ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function should have type 'a -> 'b -> 'b
       but its first argument is a module
|}]

type 'a record = {f : {M : Monad} -> 'a -> 'a M.t};;

[%%expect{|
type 'a record = { f : {M : Monad} -> 'a -> 'a M.t; }
|}]

let apply_to_record {M : Monad} {f} x = f {M} x;;

[%%expect{|
val apply_to_record : {M : Monad} -> 'a record -> 'a -> 'a M.t = <fun>
|}]

let create_record = {f= fun {M : Monad} -> M.return};;

[%%expect{|
val create_record : 'a record = {f = <fun>}
|}]

let disallowed_record = {f= fun {M : Monad} -> M.bind ~f:(fun x -> x)};;

[%%expect{|
Line 1, characters 47-69:
1 | let disallowed_record = {f= fun {M : Monad} -> M.bind ~f:(fun x -> x)};;
                                                   ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type {M : Monad} -> 'a M.t M.t -> 'a M.t
       but an expression was expected of type {M : Monad} -> 'b -> 'b M.t
       The type constructor M.t would escape its scope
|}]

type poly_record = {f_poly : 'a. {M : Monad} -> 'a -> 'a M.t};;

[%%expect{|
type poly_record = { f_poly : 'a. {M : Monad} -> 'a -> 'a M.t; }
|}]

let apply_to_poly_record {M : Monad} {f_poly} x = f_poly {M} x;;

[%%expect{|
val apply_to_poly_record : {M : Monad} -> poly_record -> 'a -> 'a M.t = <fun>
|}]

let create_poly_record = {f_poly= fun {M : Monad} -> M.return};;

[%%expect{|
val create_poly_record : poly_record = {f_poly = <fun>}
|}]

let must_apply_path (f : {M : Monad} -> 'a) = f (module Option);;

[%%expect{|
Line 1, characters 48-63:
1 | let must_apply_path (f : {M : Monad} -> 'a) = f (module Option);;
                                                    ^^^^^^^^^^^^^^^
Error: The function applied to this argument has type {M : Monad} -> 'a
This argument cannot be applied without label
|}]

let apply_variable a = a {Option};;

[%%expect{|
Line 1, characters 25-33:
1 | let apply_variable a = a {Option};;
                             ^^^^^^^^
Error: The function applied to this argument has type 'a
This argument cannot be applied as a module
|}]

let apply_ignore = ignore {Option};;

[%%expect{|
Line 1, characters 26-34:
1 | let apply_ignore = ignore {Option};;
                              ^^^^^^^^
Error: The function applied to this argument has type 'a -> unit
This argument cannot be applied as a module
|}]

let apply_non_function = 15 {Option};;

[%%expect{|
Line 1, characters 25-27:
1 | let apply_non_function = 15 {Option};;
                             ^^
Error: This expression has type int
       This is not a function; it cannot be applied.
|}]

let printing_unambiguous (f : {M : Monad} -> 'a M.t)
    (g : {M : Monad} -> 'b M.t) b =
  if b then f else g;;

[%%expect{|
val printing_unambiguous :
  ({M : Monad} -> 'a M.t) ->
  ({M : Monad} -> 'a M.t) -> bool -> {M : Monad} -> 'a M.t = <fun>
|}]

let printing_ambiguous {M : Monad} (f : {M : Monad} -> 'a M.t)
    (g : {M : Monad} -> 'b M.t) b =
  if b then f {M} else g {M};;

[%%expect{|
val printing_ambiguous :
  {M/1 : Monad} ->
  ({M/2 : Monad} -> 'a M/2.t) ->
  ({M/2 : Monad} -> 'a M/2.t) -> bool -> 'a M/1.t = <fun>
|}]
