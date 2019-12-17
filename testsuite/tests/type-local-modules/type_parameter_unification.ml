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

let unify_parameters_fail
  (x : {M : Monad} -> int M.t -> bool M.t) : _ module_args = x;;

[%%expect{|
Line 2, characters 61-62:
2 |   (x : {M : Monad} -> int M.t -> bool M.t) : _ module_args = x;;
                                                                 ^
Error: This expression has type {M/1 : Monad} -> int M/1.t -> bool M/1.t
       but an expression was expected of type
         int module_args = {M/2 : Monad} -> int M/2.t -> int M/2.t
       Type bool is not compatible with type int
|}]

type ('a, 'b) constructor =
  | A of ({M : Monad} -> 'a) | B of ('b -> {M : Monad} -> 'b);;

[%%expect{|
type ('a, 'b) constructor =
    A of ({M/1 : Monad} -> 'a)
  | B of ('b -> {M/2 : Monad} -> 'b)
|}]

let failed_constructor_A : _ constructor =
  A (fun {M : Monad} -> M.return 15);;

[%%expect{|
Line 2, characters 24-35:
2 |   A (fun {M : Monad} -> M.return 15);;
                            ^^^^^^^^^^^
Error: This expression has type {M : Monad} -> int M.t
       but an expression was expected of type {M : Monad} -> 'a
       The type constructor M.t would escape its scope
|}]

let constructor_A : _ constructor = A (fun {M : Monad} -> 15);;

[%%expect{|
val constructor_A : (int, 'a) constructor = A <fun>
|}]

let constructor_B : _ constructor = B (fun x {M : Monad} -> x);;

[%%expect{|
val constructor_B : ('a, 'b) constructor = B <fun>
|}]

type _ add_module =
  | Type : 'a -> 'a add_module
  | Add_module : 'a add_module -> ({M : Monad} -> 'a) add_module;;

[%%expect{|
type _ add_module =
    Type : 'a -> 'a add_module
  | Add_module : 'a add_module -> ({M : Monad} -> 'a) add_module
|}]

let test_add_module (x : _ add_module) =
  match x with
  | Type _ -> assert false
  | Add_module _ -> assert false;;

[%%expect{|
val test_add_module : ({M : Monad} -> 'a) add_module -> 'b = <fun>
|}]

let test_removing_module (x : ({M : Monad} -> 'a M.t -> 'a) add_module) =
  match x with
  | Type _ -> assert false
  | Add_module _ -> .;;

[%%expect{|
Line 4, characters 4-16:
4 |   | Add_module _ -> .;;
        ^^^^^^^^^^^^
Error: This pattern matches values of type ({M : Monad} -> 'b) add_module
       but a pattern was expected which matches values of type
         ({M : Monad} -> 'a M.t -> 'a) add_module
       The type constructor M.t would escape its scope
|}, Principal{|
Line 4, characters 4-16:
4 |   | Add_module _ -> .;;
        ^^^^^^^^^^^^
Error: This pattern matches values of type ({M : Monad} -> 'a) add_module
       but a pattern was expected which matches values of type
         ({M : Monad} -> 'b M.t -> 'c) add_module
       The type constructor M.t would escape its scope
|}]
