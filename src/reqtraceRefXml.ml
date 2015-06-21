(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
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

open ReqtraceTypes.Refs

let ns = "https://github.com/infidel/reqtrace"

let attr name value = ((ns, name), value)

type node = ('a Xmlm.frag as 'a) Xmlm.frag

let make_tag tag (attrs,nodes) : node =
  `El (((ns,tag),attrs),nodes)

let of_docid docid =
  match docid with
    | RFC number -> make_tag "rfc" ([], [`Data (string_of_int number)])
    | Uri uri -> make_tag "uri" ([], [`Data uri])

let strip_prefix str prefix =
  let n = String.length prefix in
  if String.length str > n && prefix = String.sub str 0 n then
    String.sub str n (String.length str - n)
  else
    str

let of_loc ?strip {Location.loc_start={Lexing.pos_fname=filename; Lexing.pos_lnum=linenum}} =
  let stripped = match strip with
    | None -> filename
    | Some prefix -> strip_prefix filename prefix
  in
  let attrs = [
    attr "filename" stripped;
    attr "linenum" (string_of_int linenum)] in
  let nodes = [] in
  make_tag "loc" (attrs, nodes)

let of_docref =
  function
  | Bound name ->
    make_tag "docref" ([attr "name" name], [])
  | Unbound docid ->
    make_tag "docref" ([], [of_docid docid])

let of_reqid reqid =
  make_tag "reqid" ([], [`Data reqid])

let of_reqref ?strip {docref; reqid; loc; reftype} =
  let attrs = match reftype with
    | Unknown -> []
    | Impl -> [attr "type" "impl"]
    | Test -> [attr "type" "test"]
  in
  let nodes = [
    of_docref docref;
    of_reqid reqid;
    of_loc ?strip loc;
  ] in
  make_tag "reqref" (attrs, nodes)

let of_docbind (name, docid) =
  let attrs = [attr "name" name] in
  let nodes = [of_docid docid] in
  make_tag "specdoc" (attrs, nodes)

let of_impl_unit ?strip {docs; refs} =
  let attrs = [(Xmlm.ns_xmlns, "xmlns"), ns] in
  let nodes = List.map (of_reqref ?strip) refs in
  let nodes = (List.map of_docbind docs) @ nodes in
  make_tag "unit" (attrs, nodes)

let output_impl_unit ?strip xmlout impl =
  Xmlm.output_doc_tree (fun node -> node) xmlout (None, of_impl_unit ?strip impl)


let require_attr = ReqtraceDocXml.require_attr
let optional_attr = ReqtraceDocXml.optional_attr
let fail_xml = ReqtraceDocXml.fail_xml
let read_text = ReqtraceDocXml.read_text

let empty path = { docs = []; refs = []; }

let rec read_impl_unit path xml =
  let xmlns = ns in
  let fail_xml msg = fail_xml msg xml path in
  let read_text text = read_text text xml path in
  (* Returns a string *)
  let rec read_no_children () =
    match Xmlm.input xml with
    | `El_start _ -> fail_xml "expected no children"
    | `Data data -> read_no_children ()
    | `Dtd _ -> read_no_children ()
    | `El_end -> ()
  in
  let rec read_docid docref =
    match Xmlm.input xml with
    | `El_start ((ns,"rfc"),attrs) when ns = xmlns ->
      let text = read_text "" in
      begin match docref with
        | None -> read_docid (Some (RFC (int_of_string text)))
        | Some _ -> fail_xml "<docref> must contain exactly one of <rfc> or <uri>"
      end
    | `El_start ((ns,"uri"),attrs) when ns = xmlns ->
      let text = read_text "" in
      begin match docref with
        | None -> read_docid (Some (Uri text))
        | Some _ -> fail_xml "<docref> must contain exactly one of <rfc> or <uri>"
      end
    | `El_start _ -> fail_xml "expected <rfc> or <uri> in <docref>"
    | `Data _ | `Dtd _ -> read_docid docref
    | `El_end -> match docref with
      | None -> fail_xml "<docref>/<specdoc> must contain exactly one of <rfc> or <uri>"
      | Some value -> value
  in
  (* Returns a reqref record *)
  let rec read_reqref reqref =
    match Xmlm.input xml with
    | `El_start ((ns,"docref"),attrs) when ns = xmlns ->
      if reqref.docref <> Unbound (Uri "") then
        fail_xml "<reqref> must have exactly one <docref>";
      let docref = match optional_attr attrs "name" with
        | None -> Unbound (read_docid None)
        | Some name -> read_no_children (); Bound name
      in
      read_reqref { reqref with docref }
    | `El_start ((ns,"reqid"),attrs) when ns = xmlns ->
      if reqref.reqid <> "" then
        fail_xml "<reqref> must have exactly one <reqid>";
      let text = read_text "" in
      read_reqref { reqref with reqid = text }
    | `El_start ((ns,"loc"),attrs) when ns = xmlns ->
      if reqref.loc <> Location.none then
        fail_xml "<reqref> must have exactly one <loc>";
      let filename = require_attr attrs "filename" fail_xml in
      let linenum = require_attr attrs "linenum" fail_xml in
      read_no_children ();
      let loc_start = Lexing.({ pos_fname=filename; pos_lnum=int_of_string linenum; pos_bol=0; pos_cnum=0 }) in
      let loc = Location.({ loc_start; loc_end=loc_start; loc_ghost=true; }) in
      read_reqref { reqref with loc = loc }
    | `El_start _ -> fail_xml "expected only <docref>, <reqid> and <loc> in <reqref>"
    | `Data _ | `Dtd _ -> read_reqref reqref
    | `El_end -> reqref
  in
  (* Returns an impl_unit *)
  let rec read_children impl = 
    match Xmlm.input xml with
    | `El_start ((ns,"specdoc"),attrs) when ns = xmlns ->
      let name = require_attr attrs "name" fail_xml in
      let docid = read_docid None in
      read_children { impl with docs = (name, docid) :: impl.docs }
    | `El_start ((ns,"reqref"),attrs) when ns = xmlns ->
      let reftype = match optional_attr attrs "type" with
        | None -> Unknown
        | Some value when value = "impl" -> Impl
        | Some value when value = "test" -> Test
        | Some _ -> fail_xml "Invalid value for <reqref type='_'> attribute"
      in
      let reqref = read_reqref { docref=Unbound (Uri ""); reqid=""; loc=Location.none; reftype } in
      if reqref.docref = Unbound (Uri "") then
        fail_xml "<reqref> must have exactly one <docref>";
      if reqref.reqid = "" then
        fail_xml "<reqref> must have exactly one <reqid>";
      if reqref.loc = Location.none then
        fail_xml "<reqref> must have exactly one <loc>";
      read_children { impl with refs = reqref :: impl.refs }
    | `El_start _ -> fail_xml "expected <specdoc> or <reqref> in <unit>"
    | `Data _ | `Dtd _ -> read_children impl
    | `El_end -> impl
  in
  (* Returns an impl_unit *)
  let read_root = function
    | ((ns,"unit"),attrs) when ns = xmlns ->
      read_children { docs = []; refs = []; }
    | _ -> fail_xml "expected root node <unit>"
  in
  match Xmlm.input xml with
  | `El_start tag -> read_root tag
  | `El_end -> empty path
  | `Data _ | `Dtd _ -> read_impl_unit path xml

let read path =
  let ic = open_in path in
  let input = Xmlm.make_input (`Channel ic) in
  let impl = read_impl_unit path input in
  let () = close_in ic in
  `Ok impl

