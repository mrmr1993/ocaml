(* TEST
   * expect
*)

module type S_with_implicit = sig
  implicit module M : sig end
end;;

[%%expect{|
module type S_with_implicit = sig implicit module M : sig end end
|}]

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
let map {M : Monad} = M.map;;
let bind {M : Monad} = M.bind;;

[%%expect{|
val return : {M : Monad} -> 'a -> 'a M.t = <fun>
val map : {M : Monad} -> 'a M.t -> f:('a -> 'b) -> 'b M.t = <fun>
val bind : {M : Monad} -> 'a M.t -> f:('a -> 'b M.t) -> 'b M.t = <fun>
|}]

module Option_monad = struct
  type 'a t = 'a option
  let return x = Some x
  let map x ~f =
    match x with
    | Some x -> Some (f x)
    | None -> None
  let bind x ~f =
    match x with
    | Some x -> f x
    | None -> None
end;;

module List_monad = struct
  type 'a t = 'a list
  let return x = [x]
  let rec map xs ~f =
    match xs with
    | x :: xs -> f x :: map xs ~f
    | [] -> []
  let rec concat xss =
    match xss with
    | [] -> []
    | [] :: xss -> concat xss
    | (x :: xs) :: xss -> x :: concat (xs :: xss)
  let bind xs ~f = concat (map xs ~f)
end;;

[%%expect{|
module Option_monad :
  sig
    type 'a t = 'a option
    val return : 'a -> 'a option
    val map : 'a option -> f:('a -> 'b) -> 'b option
    val bind : 'a option -> f:('a -> 'b option) -> 'b option
  end
module List_monad :
  sig
    type 'a t = 'a list
    val return : 'a -> 'a list
    val map : 'a list -> f:('a -> 'b) -> 'b list
    val concat : 'a list list -> 'a list
    val bind : 'a list -> f:('a -> 'b list) -> 'b list
  end
|}]

module One_instance = struct
  implicit module Option_monad = Option_monad

  let return_one_instance = return {_} 15

  let return_several =
    let x = return {_} 15 in
    let y = return {_} () in
    let z = return {_} [1; 2; 3] in
    (x, y, z)
end;;

[%%expect{|
module One_instance :
  sig
    implicit module Option_monad = Option_monad
    val return_one_instance : int Option_monad.t
    val return_several :
      int Option_monad.t * unit Option_monad.t * int list Option_monad.t
  end
|}]

module Two_instances_fail = struct
  implicit module Option_monad = Option_monad

  implicit module List_monad = List_monad

  let return_fail_two_instances = return {_} 15
end;;

[%%expect{|
Line 6, characters 42-43:
6 |   let return_fail_two_instances = return {_} 15
                                              ^
Error: This implicit argument is ambiguous.
       Could not choose between the candidates: List_monad Option_monad.
       Hint: Consider passing the desired instance directly.
|}]

module Two_instances = struct
  implicit module Option_monad = Option_monad

  implicit module List_monad = List_monad

  let map_option = map {_} ~f:((+) 1) (Some 15)

  let map_list = map {_} ~f:((+) 1) [1; 2; 3]

  let return_option : (int option * unit option * int list option) =
    return {_} 15, return {_} (), return {_} [1; 2; 3]
end;;

[%%expect{|
module Two_instances :
  sig
    implicit module Option_monad = Option_monad
    implicit module List_monad = List_monad
    val map_option : int Option_monad.t
    val map_list : int List_monad.t
    val return_option : int option * unit option * int list option
  end
|}]

let module_expression () =
  let module M = struct
    implicit module Option_monad = Option_monad

    let x = return {_} 15
  end in
  M.x;;

[%%expect{|
val module_expression : unit -> int option = <fun>
|}]

module One_instance_open = struct
  open One_instance

  let map_option = map {_} ~f:((+) 1) (return {_} 15)
end;;

[%%expect{|
module One_instance_open :
  sig val map_option : int One_instance.Option_monad.t end
|}]

module One_instance_implicit_open = struct
  open implicit One_instance

  let map_option = map {_} ~f:((+) 1) (return {_} 15)
end;;

[%%expect{|
module One_instance_implicit_open :
  sig val map_option : int One_instance.Option_monad.t end
|}]

let map_option_fail = map {_} ~f:((+) 1) (return {_} 15)

[%%expect{|
Line 1, characters 27-28:
1 | let map_option_fail = map {_} ~f:((+) 1) (return {_} 15)
                               ^
Error: This implicit argument is ambiguous.
       No candidate instances were found.
       Considered constraints:
         int ?M.t = int ?M/2.t.
       Hint: Consider passing the desired instance directly.
|}]

module Two_instances_open = struct
  open Two_instances

  let map_option = map {_} ~f:((+) 1) (Some 15)
end;;

[%%expect{|
module Two_instances_open :
  sig val map_option : int Two_instances.Option_monad.t end
|}]

module Two_instances_implicit_open = struct
  open implicit Two_instances

  let map_option = map {_} ~f:((+) 1) (Some 15)

  let map_list = map {_} ~f:((+) 1) [1; 2; 3]
end;;

[%%expect{|
module Two_instances_implicit_open :
  sig
    val map_option : int Two_instances.Option_monad.t
    val map_list : int Two_instances.List_monad.t
  end
|}]

module M = struct
  module Two_instances_M = Two_instances
  implicit module Option_monad = Option_monad
end;;

module Implicit_open_ignores_modules_fail = struct
  open implicit M

  module Two_instances = Two_instances_M
end;;

[%%expect{|
module M :
  sig
    module Two_instances_M = Two_instances
    implicit module Option_monad = Option_monad
  end
Line 9, characters 25-40:
9 |   module Two_instances = Two_instances_M
                             ^^^^^^^^^^^^^^^
Error: Unbound module Two_instances_M
Hint: Did you mean Two_instances?
|}]

module Implicit_open_ignores_values_fail = struct
  open implicit Two_instances

  let x = map_option
end;;

[%%expect{|
Line 4, characters 10-20:
4 |   let x = map_option
              ^^^^^^^^^^
Error: Unbound value map_option
|}]

let expression_open =
  let open Two_instances in
  let res =
    [Some 1; None; Some 3]
    |> map {_} ~f:(map {_} ~f:((+) 1))
    |> bind {_} ~f:(function | Some x -> [x] | None -> [])
  in
  res;;

[%%expect{|
val expression_open : int Two_instances.List_monad.t = [2; 4]
|}]

let nested_types {M1 : Monad} {M2 : Monad} (x : 'a M1.t M2.t M1.t)
    (f : 'a M1.t M2.t -> 'a M2.t M1.t) =
  bind {_} x ~f;;

[%%expect{|
val nested_types :
  {M1 : Monad} ->
  {M2 : Monad} ->
  'a M1.t M2.t M1.t -> ('a M1.t M2.t -> 'a M2.t M1.t) -> 'a M2.t M1.t = <fun>
|}]

include struct
  open implicit Two_instances

  let apply_nested_types =
    nested_types {_} {_} [Some [15]; None; Some [1; 2; 3]]
      (function
        | Some xs -> map {_} xs ~f:(fun x -> Some x)
        | None -> [])
end;;

[%%expect{|
val apply_nested_types :
  int Two_instances.Option_monad.t Two_instances.List_monad.t =
  [Some 15; Some 1; Some 2; Some 3]
|}]

let apply_nested_types_coerce_fail : string option list = apply_nested_types;;

[%%expect{|
Line 1, characters 58-76:
1 | let apply_nested_types_coerce_fail : string option list = apply_nested_types;;
                                                              ^^^^^^^^^^^^^^^^^^
Error: This expression has type
         int Two_instances.Option_monad.t Two_instances.List_monad.t =
           int Two_instances.Option_monad.t list
       but an expression was expected of type string option list
       Type int Two_instances.Option_monad.t = int option
       is not compatible with type string option
       Type int is not compatible with type string
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

let pure {F : Functor} x =
  let module M = Free_monad(F) in
  M.Pure x;;

let free {F : Functor} x =
  let module M = Free_monad(F) in
  M.Free x;;

[%%expect{|
val pure : {F : Functor} -> 'a -> 'a Free_monad(F).t = <fun>
val free : {F : Functor} -> 'a Free_monad(F).t F.t -> 'a Free_monad(F).t =
  <fun>
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

let implicit_in_open =
  let open struct
    implicit module Option_monad = Option_monad
    let x =
      map2 {_} (pure {_} 15) (pure {_} true)
        ~f:(fun x b -> if b then 2 * x else x)
  end in
  x;;

[%%expect{|
val implicit_in_open : int Free_monad(Option_monad).t =
  Free_monad(Option_monad).Pure 30
|}]

module Implicit_sig_erase : sig module M : sig end end = struct
  implicit module M = struct end
end;;

[%%expect{|
module Implicit_sig_erase : sig module M : sig end end
|}]

module Implicit_sig_introduce_fail
 : sig implicit module M : sig end end =
struct
  module M = struct end
end;;

[%%expect{|
Lines 3-5, characters 0-3:
3 | struct
4 |   module M = struct end
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig module M : sig end end
       is not included in
         sig implicit module M : sig end end
       Module type declarations do not match:
         module M : sig ... end
       does not match
         implicit module M : sig ... end

|}]

module Implicit_sig : sig implicit module M : sig end end = struct
  implicit module M = struct end
end;;

[%%expect{|
module Implicit_sig : sig implicit module M : sig end end
|}]

module Implicit_no_sig = struct
  implicit module M = struct end
end;;

[%%expect{|
module Implicit_no_sig : sig implicit module M : sig end end
|}]
