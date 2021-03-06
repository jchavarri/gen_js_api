gen_js_api: low-level binding to Javascript
===========================================

The code generated by gen_js_api relies on a `Ojs` module (the runtime
support library).  In the same way that OCaml `Obj` module exposes
(unsafe) operations to manipulate arbitrary OCaml values (after
casting them to a universal type `Obj.t`), `Ojs` allows to manipulate
arbitrary Javascript values through an `Ojs.t` universal type.

`Ojs` encourages to think of native JS values as being "foreign"
values, even though in practice, all OCaml values are represented as
JS values when the OCaml code is compiled with js_of_ocaml.  In
particular, `Ojs` does not expose a function allowing to cast an
arbitrary OCaml value to `Ojs.t` (this can always be done with
`Obj.magic`).

`Ojs.t` is similar to `Js.Unsafe.any` type, but it abstracts away from
specific properties of how js_of_ocaml represents OCaml values.  For
instance the fact, that OCaml integers are encoded directly as JS
numbers is not apparent in `Ojs`, and if this property was to change,
client code would be unaffected.

Abstracting away from js_of_ocaml encoding would also make it easy to
change the way OCaml and JS are connected (either because of changes
in js_of_ocaml's encoding of OCaml values, or because an entirely
different technology is used, such as an OCaml bytecode interpreter
written in Javascript or a Javascript engine linked with native OCaml
code).

Note that code generated by gen_js_api doesn't depend on js_of_ocaml's
standard library (`Js` module), only on js_of_ocaml's runtime system.
Our local `Ojs` interface maps directly to primitives provided by
js_of_ocaml's runtime.


Users of gen_js_api would not use `Ojs` very often, except to define
"opaque sub-types" of `Ojs.t` in order to represent JS "classes" or
"interfaces":

```ocaml
type t = private Ojs.t
```

Occasionnaly, it it useful to go down to `Ojs` in order to define
**custom mappings** between JS and OCaml.  For instance, one can
define a type for association lists indexed on strings in OCaml that
are mapped to JS objects:

```ocaml
module Dict : sig
  type 'a t = (string * 'a) list
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t
end = struct
  type 'a t = (string * 'a) list

  let t_to_js ml2js l =
    let o = Ojs.empty_obj () in
    List.iter (fun (k, v) -> Ojs.set o k (ml2js v)) l;
    o

  let t_of_js js2ml o =
    let l = ref [] in
    Ojs.iter_properties o
      (fun k -> l := (k, js2ml (Ojs.get o k)) :: !l);
    !l
end
```
