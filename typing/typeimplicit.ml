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
  | Types.Idfr_marker -> ()

let raise_ambiguous ({Types.ihl_loc= loc; _} as implicit_hole) candidates env =
  let candidates = List.map fst candidates in
  let equalities =
    List.filter_map (function
        | Types.Idfr_unify (ty1, ty2, _run) -> Some (ty1, ty2)
        | _ -> None)
      implicit_hole.Types.ihl_deferreds
  in
  raise (Error (loc, env, Ambiguous_functor_argument (candidates, equalities)))

let resolve_implicits env =
  let implicit_instances =
    List.map (fun path -> (path, Env.find_module path env))
      (Env.implicit_instances env)
  in
  let instantiate_implicit implicit_hole path =
    Btype.set_ident_instance implicit_hole.Types.ihl_ident path;
    Btype.set_ident_scope implicit_hole.Types.ihl_ident (Path.scope path);
    List.iter run_implicit_deferred implicit_hole.Types.ihl_deferreds
  in
  let implicit_holes = Env.implicit_holes env in
  List.iter (fun {Types.ihl_ident= id; _} ->
      Env.add_implicit_deferred_check id Types.Idfr_marker env )
    implicit_holes;
  let candidates =
    List.map (fun ({Types.ihl_loc= loc; ihl_ident= id; _} as implicit_hole) ->
        let candidates =
          List.filter_map
            (fun (path, md) ->
              let snap = Btype.snapshot () in
              try
                assert (Path.scope path <= Ident.scope id);
                let mod_expr =
                  { mod_desc=
                      Tmod_ident
                        (path, Location.mknoloc (Longident.Lident "_"))
                  ; mod_loc= loc
                  ; mod_type= md.Types.md_type
                  ; mod_env= env
                  ; mod_attributes= [] }
                in
                let modl =
                  !wrap_constraint env mod_expr implicit_hole.ihl_module_type
                in
                instantiate_implicit implicit_hole path;
                Btype.backtrack snap;
                Some (path, modl)
              with _ -> Btype.backtrack snap; None )
            implicit_instances
        in
        begin match candidates with
        | [(path, _modl)] ->
            (* We re-fetch the hole from the environment, to ensure that we
               evaluate any constraints that were added this round.
            *)
            let implicit_hole = Env.find_implicit_hole id env in
            begin try instantiate_implicit implicit_hole path
            with _ -> raise_ambiguous implicit_hole [] env
            end
        | [] -> raise_ambiguous implicit_hole [] env
        | _ -> ()
        end;
        candidates )
    implicit_holes
  in
  let rec go candidates =
    (* Invariant: if candidates = [_] then it has already been instantiated. *)
    let is_finished = ref true in
    let implicit_holes = Env.implicit_holes env in
    List.iter (fun {Types.ihl_ident= id; _} ->
        Env.add_implicit_deferred_check id Idfr_marker env )
      implicit_holes;
    let candidates =
      List.map2
        (fun ({Types.ihl_ident= id; _} as implicit_hole) candidates ->
          match candidates with
          | [_] -> candidates
          | _ ->
              let candidates =
                List.filter_map
                  (fun (path, modl) ->
                    let snap = Btype.snapshot () in
                    try
                      instantiate_implicit implicit_hole path;
                      Btype.backtrack snap;
                      Some (path, modl)
                    with _ -> Btype.backtrack snap; None )
                  candidates
              in
              begin match candidates with
              | [(path, _modl)] ->
                  (* Do another loop to check whether resolving this instance
                     adds new constraints to a module we've already checked
                     this round.
                  *)
                  is_finished := false;
                  (* We re-fetch the hole from the environment, to ensure that
                     we evaluate any constraints that were added this round.
                  *)
                  let implicit_hole = Env.find_implicit_hole id env in
                  begin try instantiate_implicit implicit_hole path
                  with _ -> raise_ambiguous implicit_hole [] env
                  end
              | [] -> raise_ambiguous implicit_hole [] env
              | _ ->
                  match implicit_hole.ihl_deferreds with
                  | Types.Idfr_marker :: _ ->
                      (* No further progress was made for this hole. *)
                      ()
                  | _ ->
                      (* Additional constraints were found. *)
                      is_finished := false
              end;
              candidates )
        implicit_holes candidates
      in
      if !is_finished then
        List.map2
          (fun
              ({Types.ihl_loc= loc; ihl_ident= id; _} as implicit_hole)
              candidates ->
            match candidates with
            | [(_path, modl)] -> (id, loc, modl)
            | _ -> raise_ambiguous implicit_hole candidates env )
          implicit_holes candidates
      else go candidates
  in
  go candidates

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
