(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

module H = Dune_helpers

type error = OpamPackage.t * exn

let error_to_string : error -> string =
 fun (pkg, err) ->
  Printf.sprintf "Failed to install %s\nError: %s\n"
    (OpamPackage.to_string pkg)
    (Printexc.to_string err)

let or_raise = function Ok () -> () | Error (`Msg m) -> raise (Failure m)

let build_run_spec ~opam_repository ~base config =
  let spec = Opam_build.build_spec ~local:true ~for_docker:true ~base config in
  let dockerfile =
    Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false ~os:`Unix spec
  in
  (* FIXME: Clone/copy the repo to a tmp dir and build in that? *)
  let dir = Fpath.v opam_repository in
  let dockerignore = ".git" in
  Bos.OS.File.write Fpath.(dir / "Dockerfile") (dockerfile ^ "\n") |> or_raise;
  Bos.OS.File.write Fpath.(dir / ".dockerignore") dockerignore |> or_raise;
  let cmd =
    [ "docker"; "build"; "--no-cache"; "--progress=plain"; "--"; "." ]
  in
  let command = String.concat " " cmd in
  (* Execute the command with dir as CWD *)
  Sys.chdir opam_repository;
  Sys.command command
