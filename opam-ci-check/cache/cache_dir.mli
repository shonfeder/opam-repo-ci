type 'a or_error = ('a, [`Msg of string]) result

module type App_id = sig
  val qualifier : string
  val organization : string
  val application : string
end

module Make (_ : App_id) : sig
  val write_file : ?timeout:float -> string -> Fpath.t -> unit or_error
  val read_file : ?timeout:float -> Fpath.t -> string or_error
  val within_cache : ?timeout:float -> ('a -> 'b or_error) -> 'a -> 'b or_error
end
