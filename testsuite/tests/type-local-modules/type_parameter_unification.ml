(* TEST
   * expect
*)

module type Monad = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val return : 'a -> 'a t
end;;

[%%expect{|
module type Monad =
  sig
    type 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
  end
|}]

type 'a module_args = {M : Monad} -> 'a M.t -> 'a M.t;;

[%%expect{|
type 'a module_args = {M : Monad} -> 'a M.t -> 'a M.t
|}]

let unify_parameters (x : {M : Monad} -> int M.t -> int M.t) : _ module_args = x;;

[%%expect{|
val unify_parameters : ({M : Monad} -> int M.t -> int M.t) -> int module_args =
  <fun>
|}]

let unify_parameters_fail (x : {M : Monad} -> int M.t -> bool M.t) : _ module_args = x;;

[%%expect{|





Line 1, characters 85-86:
1 | let unify_parameters_fail (x : {M : Monad} -> int M.t -> bool M.t) : _ module_args = x;;
                                                                                         ^
Error: This expression has type {M/1 : Monad} -> int M/1.t -> bool M/1.t
       but an expression was expected of type
         int module_args = {M/2 : Monad} -> int M/2.t -> int M/2.t
       Type bool is not compatible with type int
|}]
