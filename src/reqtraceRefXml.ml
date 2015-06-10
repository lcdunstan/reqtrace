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

open Ezxmlm
open ReqtraceTypes

let ns = "https://github.com/infidel/reqtrace"

let attr name value = ((ns, name), value)

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
  to_output xmlout (None, of_impl_unit ?strip impl)

