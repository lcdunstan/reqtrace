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

type doc_let = string * string

type state = {
  mutable reftype : reftype;
  mutable doc : string;
  mutable doc_lets : doc_let list;
  mutable refs : reqref list;
}

let read_reftype state payload attr_loc =
  let open Asttypes in
  match payload with
  | (* Must be an identifier. *)
    PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_construct ({ txt=Longident.Lident ident; loc=ident_loc }, None) }, _)}] ->
    let reftype = match ident with
      | "Impl" -> Impl
      | "Test" -> Test
      | _ -> 
        raise (Location.Error (
            Location.error ~loc:attr_loc ("@reftype accepts an identifier (Impl or Test)")
          ))
    in
    state.reftype <- reftype
  | _ ->
    raise (Location.Error (
        Location.error ~loc:attr_loc ("@reftype accepts an identifier (Impl or Test)")
      ))

let read_reqdoc state payload attr_loc =
  let open Asttypes in
  match payload with
  | (* The simplest format is an evaluation of a constant string. *)
    PStr [{ pstr_desc =
              Pstr_eval ({ pexp_desc = Pexp_constant (Const_string (literal, None))}, _)}] ->
      state.doc <- literal

  | (* Could also be a let binding. *)
    PStr [{ pstr_desc = Pstr_value (rec_flag, [{
        pvb_pat = { ppat_desc = Ppat_var { txt=var_txt; loc=var_loc } };
        pvb_expr = { pexp_desc = Pexp_constant (Const_string (literal, None)) };
      }])}] ->
    state.doc_lets <- (var_txt, literal) :: state.doc_lets;
    if state.doc = "" then
      state.doc <- literal
  | _ ->
    raise (Location.Error (
        Location.error ~loc:attr_loc ("@reqdoc accepts a string or let-binding")
      ))

let split_reqid str =
  try
    let i = String.index str ':' in
    String.sub str 0 i, String.sub str (i + 1) (String.length str - i - 1)
  with
  | Not_found -> "", str

let read_ref reftype state payload attr_loc =
  let open Asttypes in
  let docid, reqid = match payload with
    | (* The simplest format is an evaluation of a constant string. *)
      PStr [{ pstr_desc = Pstr_eval ({
          pexp_desc = Pexp_constant (Const_string (literal, None))
        }, _)}] ->
      split_reqid literal

    | (* Could also be an evaluation of a "function" application. *)
      PStr [{ pstr_desc = Pstr_eval ({
          pexp_desc = Pexp_apply (
              { pexp_desc=Pexp_ident { txt=Longident.Lident ident; loc=ident_loc } },
              [(label, { pexp_desc=Pexp_constant (Const_string (literal, None))})]
            )
        }, _)}] ->
      begin
        try
          (* [@req ident "reqid"] requires a previous attribute
             [@reqdoc let ident = "docid"] and is equivalent to
             [@req "docid:reqid"],
             but hopefully ident is easier to remember than RFC numbers *)
          (List.assoc ident state.doc_lets), literal
        with
        | Not_found -> raise (Location.Error (
            Location.error ~loc:ident_loc (ident ^ " was not defined by @reqdoc")
          ))
      end
    | _ ->
      raise (Location.Error (
          Location.error ~loc:attr_loc ("@req accepts a string or @reqdoc application")
        ))
  in
  let ref = { docid; reqid; loc=attr_loc; reftype; } in
  if not (List.mem ref state.refs) then
    state.refs <- ref :: state.refs

let read_attribute state attribute =
  let open Parsetree in
  let open Asttypes in
  let { txt; loc }, payload = attribute in
  match txt with
  | "reftype" -> read_reftype state payload loc
  | "reqdoc" -> read_reqdoc state payload loc
  | "req" -> read_ref state.reftype state payload loc
  (* Not sure if this is useful
  | "impl" -> read_ref Impl state payload loc
  | "test" -> read_ref Test state payload loc
  *)
  | _ -> ()


let loc_str {Location.loc_start={Lexing.pos_fname=filename; Lexing.pos_lnum=linenum}} =
  Printf.sprintf "%s:%d" filename linenum

let error_str {Location.loc=loc; Location.msg=msg} =
  (loc_str loc) ^ ": " ^ msg

let read_structure str =
  let state = { reftype=Unknown; doc=""; doc_lets=[]; refs=[] } in
  let module MyIteratorArgument = struct
    include TypedtreeIter.DefaultIteratorArgument

    let enter_structure_item item =
      match item.str_desc with
      | Tstr_attribute attr -> read_attribute state attr
      | _ -> ()

    let enter_expression expr =
      List.iter (read_attribute state) expr.exp_attributes
  end in
  let module MyIterator = TypedtreeIter.MakeIterator(MyIteratorArgument) in
  MyIterator.iter_structure str;
  {
    ReqtraceTypes.doc = state.doc;
    ReqtraceTypes.refs = state.refs;
  }


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
  | Location.Error error -> `Error (error_str error)
  | Cmi_format.Error (Not_an_interface _) -> `Error "not an interface"
  | Cmi_format.Error (Wrong_version_interface _) -> `Error "wrong version interface"
  | Cmi_format.Error (Corrupted_interface _) -> `Error "corrupted interface"
  | Cmt_format.Error (Not_a_typedtree _) -> `Error "not a typedtree"

