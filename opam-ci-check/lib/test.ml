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
