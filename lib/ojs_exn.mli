(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** OCaml view on JS exceptions *)

type 'a t

val name: 'a t -> string
val message: 'a t -> string
val stack: 'a t -> string option
val to_string: 'a t -> string

exception Error of Ojs.any
