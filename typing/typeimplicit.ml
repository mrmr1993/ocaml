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
  | Ambiguous_functor_argument of Path.t list

exception Error of Location.t * Env.t * error

let wrap_constraint:
    (Env.t -> module_expr -> Types.module_type -> module_expr) ref =
  ref (fun _env _me _mty -> assert false)

let resolve_implicits env =
  let implicit_modules =
    List.map (fun path -> (path, Env.find_module path env))
      (Env.implicit_modules env)
  in
  List.map (fun (loc, id, mty) ->
      let candidates =
        List.filter_map
          (fun (path, md) ->
            try
              let mod_expr =
                { mod_desc=
                    Tmod_ident (path, Location.mknoloc (Longident.Lident "_"))
                ; mod_loc= loc
                ; mod_type= md.Types.md_type
                ; mod_env= env
                ; mod_attributes= [] }
              in
              Some (path, !wrap_constraint env mod_expr mty)
            with _ -> None )
          implicit_modules
      in
      match candidates with
      | [(path, modl)] ->
          Ident.set_instantiation id path;
          (id, loc, modl)
      | _ ->
          let candidates = List.map fst candidates in
          raise (Error (loc, env, Ambiguous_functor_argument candidates)) )
    (Env.implicit_module_instances env)

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
