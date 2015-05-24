(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 * Copyright (c) 2015 Luke Dunstan <LukeDunstan81@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open ReqtraceTypes
open Parsetree
open Typedtree

type state = {
  mutable refs : reqref list;
}

let read_attribute state attribute =
  let open Parsetree in
  let open Asttypes in
  match attribute with
  | ({ txt = "req"; loc = attr_loc}, payload) ->
    begin match payload with
      | (* Should have a single structure item, which is evaluation of a constant string. *)
        PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
        (* Store it *)
        state.refs <- { reqid=sym; loc=attr_loc } :: state.refs;
        ()
      | _ ->
        raise (Location.Error (
            Location.error ~loc:attr_loc "[@req] accepts a string, e.g. [@req \"6372.s9_p1_c2\"]"))
    end
  | _ -> ()


let read_structure str =
  let state = { refs = [] } in
  let module MyIteratorArgument = struct
    include TypedtreeIter.DefaultIteratorArgument

    let enter_expression expr =
      List.iter (read_attribute state) expr.exp_attributes
  end in
  let module MyIterator = TypedtreeIter.MakeIterator(MyIteratorArgument) in
  MyIterator.iter_structure str;
  { ReqtraceTypes.refs = state.refs }


let read_cmt filename =
  let open Cmi_format in
  let open Cmt_format in
  try
    let cmt_info = read_cmt filename in
    match cmt_info.cmt_annots with
    | Implementation impl -> begin
      match cmt_info.cmt_interface_digest with
      | Some digest ->
        (*
        let name = cmt_info.cmt_modname in
        let root = root_fn name digest in
        let (id, doc, items) = DocOckCmt.read_implementation root name impl in
        let imports =
          List.filter (fun (name', _) -> name <> name') cmt_info.cmt_imports
        in
        let imports = List.map (fun (s, d) -> Unresolved(s, d)) imports in
        let source =
          match cmt_info.cmt_sourcefile, cmt_info.cmt_source_digest with
          | Some file, Some digest ->
            let open Source in
            let build_dir = cmt_info.cmt_builddir in
            Some {file; digest; build_dir}
          | _, _ -> None
        in
        Ok {id; doc; digest; imports; source; items}
        *)
        `Ok (read_structure impl)
      | None -> `Error "corrupted interface"
    end
    | _ -> `Error "not an interface"
  with
  | Cmi_format.Error (Not_an_interface _) -> `Error "not an interface"
  | Cmi_format.Error (Wrong_version_interface _) -> `Error "wrong version interface"
  | Cmi_format.Error (Corrupted_interface _) -> `Error "corrupted interface"
  | Cmt_format.Error (Not_a_typedtree _) -> `Error "not a typedtree"

