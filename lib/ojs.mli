(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** Binding with JS values. *)

type 'a t
(** Type of Javascript values.  The type parameter is used to
      allow the usage of type variables in type definitions processed by gen_js_api.  *)

type top

type any = top t
(** A universal type representing arbitrary JS values. *)

(** {2 Mapper for built-in types} *)

external t_of_js: 'a t -> 'a t = "%identity"
external t_to_js: 'a t -> 'a t = "%identity"

external string_of_js: 'a t -> string = "caml_js_to_string"
external string_to_js: string -> 'a t = "caml_js_from_string"

external int_of_js: 'a t -> int = "%identity"
external int_to_js: int -> 'a t = "%identity"

external bool_of_js: 'a t -> bool = "caml_js_to_bool"
external bool_to_js: bool -> 'a t = "caml_js_from_bool"

external float_of_js: 'a t -> float = "%identity"
external float_to_js: float -> 'a t = "%identity"

val array_of_js: ('a -> 'b) -> 'c t -> 'b array
val array_to_js: ('a -> 'b) -> 'a array -> 'c t

val list_of_js: ('a -> 'b) -> 'c t -> 'b list
val list_to_js: ('a -> 'b) -> 'a list -> 'c t

val array_of_js_from: ('a -> 'b) -> 'c t -> int -> 'b array
val list_of_js_from: ('a -> 'b) -> 'c t -> int -> 'b list

val option_of_js: ('a t -> 'b) -> 'a t -> 'b option
(** Both [null] and [undefined] are mapped to [None]. *)

val option_to_js: ('a -> 'b t) -> 'a option -> 'b t
(** [None] is mapped to [null]. *)


(** {2 Wrap OCaml functions as JS functions} *)

external fun_to_js: int -> ('a t -> 'b) -> 'c t = "caml_js_wrap_callback_strict"
(** Wrap an OCaml function of known arity (>=1) into a JS function.
    Extra arguments are discarded and missing argument are filled with
    'undefined'.
*)

external fun_to_js_args: ('a t -> 'b) -> 'c t = "caml_ojs_wrap_fun_arguments"
(** Wrap an OCaml function taking JS arguments as a JS array. *)


(** {2 JS objects} *)

external get: 'a t -> string -> 'b t = "caml_js_get"

external set: 'a t -> string -> 'b t -> unit = "caml_js_set"

external obj: (string * top) array -> 'b = "caml_js_object"

val empty_obj: unit -> 'a t

val has_property: 'a t -> string -> bool
external iter_properties: 'a t -> (string -> unit) -> unit = "caml_ojs_iterate_properties"

(** {2 Calling JS functions} *)

external call: 'a t -> string -> 'b array -> 'c t = "caml_js_meth_call"
(** Call a method on an object (binding 'this' to the object). *)

external apply: 'a t -> 'b array -> 'c t = "caml_js_fun_call"
(** Call a function. *)

external new_obj: 'a t -> any array -> 'b t = "caml_js_new"
(** Call a constructor *)

val call_arr: 'a t -> string -> 'a t -> 'b t
(** Variant of [Ojs.call] where the arguments are passed as an already
    built JS array. *)

val apply_arr: 'a t -> 'b t -> 'c t
(** Variant of [Ojs.apply] where the arguments are passed as an already
    built JS array. *)

external new_obj_arr: 'a t -> any -> 'b t = "caml_ojs_new_arr"
(** Variant of [Ojs.new_obj] where the arguments are passed as an already
    built JS array. *)


(** {2 Arrays} *)

val array_make: int -> 'a t
val array_get: 'a -> int -> 'b
val array_set: 'a -> int -> 'b -> unit


(** {2 Misc} *)

val global: 'a t
val null: 'a t

external variable: string -> 'a t = "caml_js_var"

val type_of: 'a t -> string

class obj: any ->
  object
    method to_js: any
  end

external delete: 'a t -> string -> unit = "caml_js_delete"

val is_null: 'a t -> bool

val obj_type: 'a t -> string
  (** Returns:
      "[object Array]"
      "[object Object]"
      "[object Number]"
      "[object String]"
      "[object Null]"
      "[object Boolean]"
  *)
