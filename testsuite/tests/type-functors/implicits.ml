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
