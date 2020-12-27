(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2020 Matthew Ryan.                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type error =
  | Ambiguous_functor_argument of Path.t list

exception Error of Location.t * Env.t * error

val resolve_implicits:
  Env.t -> (Ident.t * Location.t * Typedtree.module_expr) list
