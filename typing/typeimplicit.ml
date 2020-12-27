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

let resolve_implicits env =
  List.iter (fun (loc, _id) ->
      raise (Error (loc, env, Ambiguous_functor_argument [])))
    (Env.implicit_module_instances env);
  []

open Format

let report_error ~loc _env = function
  | Ambiguous_functor_argument paths ->
      Location.error_of_printer ~loc (fun ppf () ->
        fprintf ppf "This implicit argument is ambiguous.@.";
        begin match paths with
          | [] ->
            fprintf ppf "No candidate instances were found."
          | _ ->
            fprintf ppf "Could not choose between the candidates:@ %a."
              (pp_print_list ~pp_sep:pp_print_space
                (fun ppf path -> pp_print_string ppf (Path.name path)))
              paths
        end;
        fprintf ppf "@.Hint: Consider passing the desired instance directly."
      ) ()

let report_error ~loc env err =
  Printtyp.wrap_printing_env ~error:true env
    (fun () -> report_error ~loc env err)

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, env, err) ->
        Some (report_error ~loc env err)
      | _ ->
        None
    )
