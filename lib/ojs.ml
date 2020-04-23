(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(* This module (mostly) abstracts away from js_of_ocaml encoding of
   OCaml values.  It serves as a support library for the code generated
   by gen_js_api.

   The module could mostly be implemented on top of js_of_ocaml's Js module
   (and in particular Js.Unsafe), but we prefer to drop the dependency
   to js_of_ocaml's library and to rely only on its compiler and JS
   runtime code.
*)


type 'a t

type top

type any = top t

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

external obj: (string * top) array -> 'b = "caml_js_object"

external variable: string -> 'a t = "caml_js_var"

external internal_get: 'a -> 'b -> 'c = "caml_js_get"
external internal_set: 'a -> 'b -> 'c -> unit = "caml_js_set"

external get: 'a t -> string -> 'b t = "caml_js_get"
external set: 'a t -> string -> 'b t -> unit = "caml_js_set"

external internal_type_of: 'a t -> string t = "caml_js_typeof"
let type_of x = string_of_js (internal_type_of x)

external pure_js_expr: string -> 'a = "caml_pure_js_expr"
let null = pure_js_expr "null"
let undefined = pure_js_expr "undefined"

external equals: 'a -> 'b -> bool = "caml_js_equals"

let global = pure_js_expr "joo_global_object"

external new_obj: 'a t -> any array -> 'b t = "caml_js_new"

external call: 'a t -> string -> 'b array -> 'c t = "caml_js_meth_call"
external apply: 'a t -> 'b array -> 'c t = "caml_js_fun_call"

let array_make n = new_obj (get global "Array") [|int_to_js n|]
let array_get t i = internal_get t (int_to_js i)
let array_set t i x = internal_set t (int_to_js i) x

let array_of_js_from f objs start =
  let n = int_of_js (get objs "length") in
  Array.init (n - start) (fun i -> f (array_get objs (start + i)))

let array_of_js f objs = array_of_js_from f objs 0

let array_to_js f arr =
  let n = Array.length arr in
  let a = array_make n in
  for i = 0 to n - 1 do
    array_set a i (f arr.(i))
  done;
  a

let list_of_js_from f objs start = Array.to_list (array_of_js_from f objs start)

let list_of_js f objs = list_of_js_from f objs 0

let list_to_js f l =
  array_to_js f (Array.of_list l)

let option_of_js f x =
  if equals x null || x == undefined then None
  else Some (f x)

let option_to_js f = function
  | Some x -> f x
  | None -> null

class obj (x: any) =
  object
    method to_js: any = x
  end

external fun_to_js: int -> ('a t -> 'b) -> 'c t = "caml_js_wrap_callback_strict"
external fun_to_js_args: ('a t -> 'b) -> 'c t = "caml_ojs_wrap_fun_arguments"

let has_property o x = not (get o x == undefined)
external iter_properties: 'a t -> (string -> unit) -> unit = "caml_ojs_iterate_properties"

let empty_obj () = new_obj (get global "Object") [||]

let apply_arr o arr = call o "apply" [| null; arr |]
let call_arr o s arr = call (get o s) "apply" [| o; arr |]
external new_obj_arr: 'a t -> any -> 'b t = "caml_ojs_new_arr"

external delete: 'a t -> string -> unit = "caml_js_delete"

let is_null x =
  equals x null

let obj_type x =
  string_of_js (call (pure_js_expr "Object.prototype.toString") "call" [|x|])
