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

open Typedtree

type error =
  | Ambiguous_functor_argument of
      Path.t list * (Types.type_desc * Types.type_expr) list

exception Error of Location.t * Env.t * error

val wrap_constraint:
  (Env.t -> module_expr -> Types.module_type -> module_expr) ref

val resolve_implicits:
  Env.t -> (Ident.t * Location.t * Typedtree.module_expr) list
