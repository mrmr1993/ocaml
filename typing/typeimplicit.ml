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

let wrap_constraint:
    (Env.t -> module_expr -> Types.module_type -> module_expr) ref =
  ref (fun _env _me _mty -> assert false)

let run_implicit_deferred = function
  | Types.Idfr_scope_escape run -> run ()
  | Types.Idfr_update_level run -> run ()
  | Types.Idfr_unify (_ty1, _ty2, run) -> run ()

let resolve_implicits env =
  let implicit_instances =
    List.map (fun path -> (path, Env.find_module path env))
      (Env.implicit_instances env)
  in
  List.map (fun ({Types.ihl_loc= loc; ihl_ident= id} as implicit_hole) ->
      let candidates =
        List.filter_map
          (fun (path, md) ->
            let snap = Btype.snapshot () in
            try
              let mod_expr =
                { mod_desc=
                    Tmod_ident (path, Location.mknoloc (Longident.Lident "_"))
                ; mod_loc= loc
                ; mod_type= md.Types.md_type
                ; mod_env= env
                ; mod_attributes= [] }
              in
              let modl =
                !wrap_constraint env mod_expr implicit_hole.ihl_module_type
              in
              Btype.set_ident_instance id path;
              Btype.set_ident_scope id (Path.scope path);
              List.iter run_implicit_deferred implicit_hole.ihl_deferreds;
              Btype.backtrack snap;
              Some (path, modl)
            with _ -> Btype.backtrack snap; None )
          implicit_instances
      in
      match candidates with
      | [(path, modl)] ->
          Btype.set_ident_instance id path;
          Btype.set_ident_scope id (Path.scope path);
          List.iter run_implicit_deferred implicit_hole.ihl_deferreds;
          (id, loc, modl)
      | _ ->
          let candidates = List.map fst candidates in
          let equalities =
            List.filter_map (function
                | Types.Idfr_unify (ty1, ty2, _run) -> Some (ty1, ty2)
                | _ -> None)
              implicit_hole.ihl_deferreds
          in
          raise
            (Error
              (loc, env, Ambiguous_functor_argument (candidates, equalities))))
    (Env.implicit_holes env)

open Format

let report_error ~loc _env = function
  | Ambiguous_functor_argument (paths, equalities) ->
      Location.error_of_printer ~loc (fun ppf () ->
        fprintf ppf "@[<v>This implicit argument is ambiguous.@ ";
        begin match paths with
          | [] ->
            fprintf ppf "No candidate instances were found."
          | _ ->
            fprintf ppf
              "Could not choose between the candidates:@[<hv2>@ %a.@]"
              (pp_print_list ~pp_sep:pp_print_space
                (fun ppf path -> pp_print_string ppf (Path.name path)))
              paths
        end;
        begin match equalities with
          | [] -> ()
          | _ ->
            fprintf ppf
              "@ @[<v2>Considered constraints:@ %a.@]"
              (pp_print_list ~pp_sep:pp_print_space
                (fun ppf (ty1, ty2) ->
                  fprintf ppf "@[<hv>%a@ =@ %a@]"
                    Printtyp.type_expr (Btype.newgenty ty1)
                    Printtyp.type_expr ty2 ))
              equalities
        end;
        fprintf ppf "@ Hint: Consider passing the desired instance directly.@]"
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
