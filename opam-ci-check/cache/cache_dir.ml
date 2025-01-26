(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2025 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

module OS = Bos.OS

let (let*) = Result.bind
let (let+) x f = Result.map f x

type 'a or_error = ('a, [`Msg of string]) result

type t =
  { dir : Fpath.t
  ; pid : int
  }

module type App_id = sig
  val qualifier : string
  val organization : string
  val application : string
end

module Make (App : App_id) = struct
  module P = Directories.Project_dirs (App)

  let cache_dir
    : Fpath.t or_error
    = match P.cache_dir with
      | None     -> Fmt.error_msg "no cache directory can be found. HOME or USERPROFILE is probably not set."
      | Some dir ->
      let dir = Fpath.v dir in
      match OS.Dir.create ~path:true dir with
      | Error (`Msg m) -> Fmt.error_msg "unable to create create cache directory due to %s" m
      | Ok _           -> Ok dir

  let cache
    : unit -> t or_error
    = fun () ->
      let+ cache_dir in
      { dir = cache_dir
      ; pid = Unix.getpid ()
      }

  let lock_file
    : t -> Fpath.t
    = fun cache -> Fpath.(cache.dir / "lock")

  let lock
    : float -> t -> unit or_error
    = let sleep_duration = 0.01 in
      fun timeout cache ->
      let l = lock_file cache in
      let rec wait waited =
        if waited > timeout then
          Fmt.error_msg "lock timeout expired after %f seconds" waited
        else
          let* exists = OS.File.exists l in
          if exists then
            (Unix.sleepf sleep_duration;
             wait (waited +. sleep_duration))
          else
            Ok ()
      in
      let* () = wait 0.0 in
      OS.File.write l (Int.to_string cache.pid)

  let unlock
    : t -> unit or_error
    = fun cache ->
      let* lock_pid_string = OS.File.read (lock_file cache) in
      let* lock_pid = try Ok (int_of_string lock_pid_string) with Failure _ -> Fmt.error_msg "Invalid PID in lock, expected an int found %s" lock_pid_string in
      if lock_pid <> cache.pid then
        Fmt.error_msg "Tried to unlock cache with PID %d but the lock is owned by %d" cache.pid lock_pid
      else
        Result.ok @@
        match OS.File.delete (lock_file cache) with
        | Error (`Msg m) -> Fmt.epr "error: trying to remove the lock file %s" m
        | Ok () -> ()

  let with_cache
    : ?timeout:float -> (t -> 'a or_error) -> 'a or_error
    = fun ?(timeout=60.) f ->
      let* cache = cache () in
      let* () = lock timeout cache in
      let* result = f cache in
      let+ () = unlock cache in
      result

  let must_be_relative path =
    if Fpath.is_abs path then invalid_arg ("absolute path" ^ (Fpath.to_string path))

  let write_file ?timeout
    : string -> Fpath.t ->  unit or_error
    = fun content path ->
      must_be_relative path;
      with_cache ?timeout @@
      fun cache -> OS.File.write Fpath.(cache.dir // path) content

  let read_file ?timeout
    : Fpath.t -> string or_error
    = fun path ->
      must_be_relative path;
      with_cache ?timeout @@
      fun cache -> OS.File.read Fpath.(cache.dir // path)

  let within_cache ?timeout
    : ('a -> 'b or_error) -> 'a -> 'b or_error
    = fun f a ->
      Result.join @@ with_cache ?timeout @@
      fun cache -> OS.Dir.with_current cache.dir f a
end
