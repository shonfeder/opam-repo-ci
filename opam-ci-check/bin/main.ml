(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

open Cmdliner
open Opam_ci_check
module Distro = Dockerfile_opam.Distro

(* This is Cmdliner.Term.map, which is not available in Cmdliner 1.1.1 *)
let map_term f x = Term.app (Term.const f) x

let to_exit_code : (unit, string) result Term.t -> Cmd.Exit.code Term.t =
  map_term @@ function
  | Ok () -> 0
  | Error msg ->
      Printf.eprintf "%s%!" msg;
      1

let lint (changed_pkgs, new_pkgs) dir =
  print_endline @@ Printf.sprintf "Linting opam-repository at %s ..." dir;
  match Lint.check ~new_pkgs ~changed_pkgs dir with
  | [] ->
      print_endline "No errors";
      Ok ()
  | errors ->
      errors |> List.map Lint.msg_of_error |> String.concat "\n"
      |> Printf.sprintf "%s\n" |> Result.error

type build_spec = BuildTest | BuildLowerBounds

let make_config pkg type_ =
  let variant =
    let distro =
      (Distro.resolve_alias Distro.master_distro :> Distro.t)
      |> Distro.tag_of_distro
    in
    let latest_major, latest_minor =
      Ocaml_version.(major Releases.latest, minor Releases.latest)
    in
    let compiler =
      Ocaml_version.(to_string Ocaml_version.(v latest_major latest_minor))
    in
    Variant.v ~distro ~compiler:(compiler, None) ~arch:`X86_64
  in
  let opam_version = `Dev in
  match type_ with
  | BuildTest ->
      Spec.opam ~variant ~lower_bounds:false ~with_tests:true ~opam_version pkg
  | BuildLowerBounds ->
      Spec.opam ~variant ~lower_bounds:true ~with_tests:false ~opam_version pkg

let build_run_spec spec_type pkg opam_repository =
  let pkg = OpamPackage.of_string pkg in
  let config = make_config pkg spec_type in
  let base = Spec.Docker ("ocaml/opam:" ^ Variant.docker_tag config.variant) in
  let status = Test.build_run_spec ~opam_repository ~base config in
  if status > 0 then Error "Failed to build and test the package" else Ok ()

let make_abs_path s =
  if Filename.is_relative s then Filename.concat (Sys.getcwd ()) s else s

let opam_repo_dir =
  let parse s =
    if Sys.file_exists s then
      let repo_file = Filename.concat s "repo" in
      let packages_dir = Filename.concat s "packages" in
      if Sys.file_exists repo_file && Sys.is_directory packages_dir then
        Ok (Some (make_abs_path s))
      else
        Error
          (`Msg
            "The specified directory does not look like an opam repository. It \
             doesn't contain required 'repo' file or 'packages' directory.")
    else Error (`Msg "The specified directory does not exist.")
  in
  let print fmt = function Some s -> Format.fprintf fmt "%s" s | None -> () in
  Arg.conv (parse, print)

let pkg_term =
  let info = Arg.info [] ~doc:"Package name + version" in
  Arg.required (Arg.pos 0 (Arg.some Arg.string) None info)

let local_opam_repo_term =
  let info =
    Arg.info [ "r"; "opam-repository" ]
      ~doc:
        "Path to local clone of Opam Repository. This is optional and only \
         required if we wish to test a version of a package not released on \
         the opam repository."
  in
  Arg.required (Arg.opt opam_repo_dir None info)

let changed_pkgs_term =
  let info =
    Arg.info
      [ "c"; "changed-packages" ]
      ~doc:"List of changed package name + version"
  in
  Arg.value (Arg.opt (Arg.list Arg.string) [] info)

let newly_published_pkgs_term =
  let info =
    Arg.info [ "n"; "newly-published" ]
      ~doc:"List of newly published package name + version"
  in
  Arg.value (Arg.opt (Arg.list Arg.string) [] info)

let packages_term =
  let create_term changed_pkgs newly_published_pkgs =
    if changed_pkgs = [] && newly_published_pkgs = [] then
      `Error
        ( false,
          "You must provide at least one changed or newly published package." )
    else `Ok (changed_pkgs, newly_published_pkgs)
  in
  Term.(ret (const create_term $ changed_pkgs_term $ newly_published_pkgs_term))

let lint_cmd =
  let doc = "Lint the opam repository directory" in
  let term =
    Term.(const lint $ packages_term $ local_opam_repo_term) |> to_exit_code
  in
  let info =
    Cmd.info "lint" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let build_test_cmd =
  let doc = "Build and test a package" in
  let term =
    Term.(
      const build_run_spec $ const BuildTest $ pkg_term $ local_opam_repo_term)
    |> to_exit_code
  in
  let info =
    Cmd.info "build-test" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let lower_bounds_cmd =
  let doc = "Build a package with lower bounds of dependencies" in
  let term =
    Term.(
      const build_run_spec $ const BuildLowerBounds $ pkg_term
      $ local_opam_repo_term)
    |> to_exit_code
  in
  let info =
    Cmd.info "lower-bounds" ~doc ~sdocs:"COMMON OPTIONS"
      ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let cmd : Cmd.Exit.code Cmd.t =
  let doc = "A tool to list revdeps and test the revdeps locally" in
  let exits = Cmd.Exit.defaults in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info = Cmd.info "opam-ci-check" ~doc ~sdocs:"COMMON OPTIONS" ~exits in
  Cmd.group ~default info [ lint_cmd; build_test_cmd; lower_bounds_cmd ]

let () = exit (Cmd.eval' cmd)
