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

let (let*) = Result.bind

let docker_build = Bos.Cmd.(v "docker" %  "build")

let build_run_spec ?(use_cache=true) ~opam_repository ~base config =
  let spec = Opam_build.build_spec ~local:true ~for_docker:true ~base config in
  let dockerfile =
    Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false ~os:`Unix spec
  in
  (* TODO: Maybe docker ability to build form git repos to default to official opam repo
     https://docs.docker.com/build/concepts/context/#git-repositories *)
  let dir = Fpath.v opam_repository in
  let dockerignore = ".git\nREADME.md\n" in
  ()
  |> Bos.OS.Dir.with_tmp "opam-ci-check-build-%s" (
    (* Create the docker file and ignore file in the tmp dir to avoid polluting the opam-repo  *)
    fun tmp_dir () ->
      let dockerfile_path = Fpath.(tmp_dir / "Dockerfile") in
      let* () =
        (* See https://docs.docker.com/build/concepts/context/#filename-and-location *)
        Bos.OS.File.write Fpath.(tmp_dir / "Dockerfile.dockerignore") dockerignore
      in
      let* () = Bos.OS.File.write dockerfile_path dockerfile in
      let* () = Bos.OS.Dir.set_current dir in
      let cmd =
        let with_cache = if use_cache then Bos.Cmd.empty else Bos.Cmd.v "--no-cache" in
        Bos.Cmd.(docker_build
                 %% with_cache
                 % "--progress=plain"
                 % "--file" % Fpath.to_string dockerfile_path
                 % "--" % "." )
      in
      match Bos.OS.Cmd.(in_string dockerfile |> run_in cmd) with
      | Ok () -> Ok ()
      | Error (`Msg err) -> Error (`Msg ("Failed to build and test the package: " ^ err))
  )
  |> Result.join
