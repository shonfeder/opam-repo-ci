(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

type error = OpamPackage.t * exn
(** A package and the exception recording its failure to install or pass tests. *)

val error_to_string : error -> string

