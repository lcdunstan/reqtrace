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
open Asttypes
open Parsetree
open Typedtree

type state = {
  mutable reftype : reftype;
  mutable docs : docbind list;
  mutable refs : reqref list;
}

let read_reftype ~loc state payload =
  match payload with
  | (* Must be an identifier. *)
    PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_construct ({ txt=Longident.Lident ident; loc=ident_loc }, None) }, _)}] ->
    let reftype = match ident with
      | "Impl" -> Impl
      | "Test" -> Test
      | _ -> 
        raise (Location.Error (
            Location.error ~loc ("@reftype accepts an identifier (Impl or Test)")
          ))
    in
    state.reftype <- reftype
  | _ ->
    raise (Location.Error (
        Location.error ~loc ("@reftype accepts an identifier (Impl or Test)")
      ))

let read_docid ~loc expr =
  match expr with
  | Pexp_apply (
      { pexp_desc=Pexp_ident { txt=Longident.Lident "rfc"; loc=ident_loc } },
      [(label, { pexp_desc=Pexp_constant (Const_int number)})]
    ) ->
    RFC number
  | Pexp_apply (
      { pexp_desc=Pexp_ident { txt=Longident.Lident "uri"; loc=ident_loc } },
      [(label, { pexp_desc=Pexp_constant (Const_string (literal, None))})]
    ) ->
    Uri literal
  | _ -> 
    raise (Location.Error (
        Location.error ~loc ("@reqdoc accepts (rfc <integer>) or (uri <string>)")
      ))

let read_reqdoc ~loc state payload =
  match payload with
  | (* We expect a let binding. *)
    PStr [{ pstr_desc = Pstr_value (rec_flag, [{
        pvb_pat = { ppat_desc = Ppat_var { txt=var_txt; loc=var_loc } };
        pvb_expr = { pexp_desc };
      }])}] ->
    let docid = read_docid ~loc pexp_desc in
    state.docs <- (var_txt, docid) :: state.docs;
  | _ ->
    raise (Location.Error (
        Location.error ~loc ("@reqdoc accepts (rfc <integer>) or (uri <string>)")
      ))

let read_docref ~loc state expr =
  match expr with
  | Pexp_ident { txt=Longident.Lident ident; loc=ident_loc } ->
    (* [@req ident "reqid"] requires a previous attribute
       [@reqdoc let ident = docid] and is equivalent to
       [@req docid "reqid"],
       but hopefully ident is easier to remember than RFC numbers *)
    if List.mem_assoc ident state.docs then
      Bound ident
    else
      raise (Location.Error (
          Location.error ~loc:ident_loc (ident ^ " was not defined by [@reqdoc let name = ...]")
        ))
  | _ ->
    try
      Unbound (read_docid ~loc expr)
    with
    | Location.Error _ ->
      raise (Location.Error (
          Location.error ~loc ("@req accepts a name bound by @reqdoc, or (rfc <integer>), or (uri <string>)")
        ))

let rec docref_equal state ref1 ref2 =
  match ref1, ref2 with
  | Unbound id1, Unbound id2 -> (id1 = id2)
  | Bound name, _ ->
    let id1 = List.assoc name state.docs in
    docref_equal state (Unbound id1) ref2
  | Unbound id1, Bound name ->
    docref_equal state ref2 ref1

let reqref_equal state ref1 ref2 =
  (docref_equal state ref1.docref ref2.docref) &&
  (ref1.reqid = ref2.reqid) &&
  (ref1.loc = ref2.loc) &&
  (ref1.reftype = ref2.reftype)

let read_reqref ~loc reftype state payload =
  match payload with
  | PStr [{ pstr_desc = Pstr_eval ({
      pexp_desc = Pexp_apply (
          { pexp_desc=func },
          [(label, { pexp_desc=Pexp_constant (Const_string (literal, None))})]
        )
    }, _)}] ->
    let docref = read_docref ~loc state func in
    let ref = { docref; reqid=literal; loc; reftype; } in
    if not (List.exists (reqref_equal state ref) state.refs) then
      state.refs <- ref :: state.refs

  | _ ->
    raise (Location.Error (
        Location.error ~loc ("@req accepts a string or @reqdoc application")
      ))

let read_attribute state attribute =
  let open Parsetree in
  let { txt; loc }, payload = attribute in
  match txt with
  | "reftype" -> read_reftype ~loc state payload
  | "reqdoc" -> read_reqdoc ~loc state payload
  | "req" -> read_reqref ~loc state.reftype state payload
  | _ -> ()


let loc_str {Location.loc_start={Lexing.pos_fname=filename; Lexing.pos_lnum=linenum}} =
  Printf.sprintf "%s:%d" filename linenum

let error_str {Location.loc=loc; Location.msg=msg} =
  (loc_str loc) ^ ": " ^ msg

let read_structure ~rfcs str =
  let docs = List.map (fun (name, num) -> (name, RFC num)) rfcs in
  let state = { reftype=Unknown; docs; refs=[] } in
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
    ReqtraceTypes.docs = state.docs;
    ReqtraceTypes.refs = state.refs;
  }


let read_cmt ~rfcs filename =
  let open Cmi_format in
  let open Cmt_format in
  try
    let cmt_info = read_cmt filename in
    match cmt_info.cmt_annots with
    | Implementation impl -> begin
      match cmt_info.cmt_interface_digest with
      | Some digest ->
        `Ok (read_structure ~rfcs impl)
      | None -> `Error "corrupted interface"
    end
    | _ -> `Error "not an interface"
  with
  | Location.Error error -> `Error (error_str error)
  | Cmi_format.Error (Not_an_interface _) -> `Error "not an interface"
  | Cmi_format.Error (Wrong_version_interface _) -> `Error "wrong version interface"
  | Cmi_format.Error (Corrupted_interface _) -> `Error "corrupted interface"
  | Cmt_format.Error (Not_a_typedtree _) -> `Error "not a typedtree"

