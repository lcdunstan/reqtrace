(*
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

open Ezxmlm
open ReqtraceTypes.RFC

let ns = ""
let attr name value = ((ns, name), value)

let opt_attr name opt_value attrs =
  match opt_value with
  | None -> attrs
  | Some value -> attr name value :: attrs

let of_line (line:linesub) =
  (* TODO: start/end/num *)
  make_tag "span" ([attr "class" "line"], [`Data line.text])

let body_of_doc doc ref_hash src_base =
  let of_ref target =
    make_tag "div" ([attr "class" "ref"], [
        `Data "See ";
        make_tag "a" ([attr "href" ("#" ^ target)], [`Data target]);
      ])
  in

  let open ReqtraceTypes.Refs in
  let of_reqref { loc={Location.loc_start={Lexing.pos_fname=path; Lexing.pos_lnum=linenum}}; reftype; } =
    let reftype_str = match reftype with
      | Impl -> "impl"
      | Test -> "test"
      | Unknown -> match Stringext.find_from path "test" with
        | None -> "impl"
        | Some i -> "test"
    in
    let text = Printf.sprintf "%s:%d" path linenum in
    make_tag "div" (
      [attr "class" ("coderef " ^ reftype_str);],
      [
        `Data (reftype_str ^ ": ");
        if src_base = "" then
          `Data text
        else
          make_tag "a" (
            [attr "href" (Printf.sprintf "%s%s#L%d" src_base path linenum)],
            [`Data text]
          );
      ])
  in

  let of_notes_child = function
    | Note { text; todo; } -> make_tag "div" ([attr "class" "note"], [`Data text])
    | Ref target -> of_ref target
    | CodeRef ref -> make_tag "div" ([attr "class" "coderef"], [`Data ref]) (*TODO*)
  in

  let of_clause (clause:clause) =
    let label, code_refs = match clause.id with
      | None -> [], []
      | Some id -> [make_tag "span" ([attr "class" "label"], [`Data id])], Hashtbl.find_all ref_hash id
    in
    let text = String.concat " " (List.map (fun (line:linesub) -> line.text) clause.lines) in
    let notes = (List.map of_notes_child clause.notes) @ (List.map of_reqref code_refs) in
    let notes_div = if notes = [] then [] else [make_tag "div" ([attr "class" "notes"], notes)] in
    make_tag "div" (
      opt_attr "id" clause.id
        [attr "class" "clause"],
      label @ notes_div @ [`Data text])
  in

  let of_toc_paragraph paragraph =
    let text = String.concat "\n" (List.map (fun (line:linesub) -> line.text) paragraph.lines) in
    make_tag "pre" ([attr "class" "toc"], [`Data text])
  in

  let of_paragraph paragraph =
    let lines = List.map of_line paragraph.lines in
    let clauses = List.map of_clause paragraph.clauses in
    (* TODO: <notes> in <paragraph> *)
    make_tag "div" ([attr "class" "paragraph"], lines @ clauses)
  in

  let of_section section =
    let heading = make_tag "h2" (
        opt_attr "id" section.id [],
        [`Data section.name]
      ) in
    let anchor = make_tag "a" ([
        attr "name" (match section.id with None -> section.name | Some id -> id)
      ], []) in
    let paras = if section.name = "Table of Contents" then
        List.map of_toc_paragraph section.paras
      else
        List.map of_paragraph section.paras
    in
    (* TODO: <notes> in <section> *)
    make_tag "div" ([attr "class" "section"], heading :: anchor :: paras)
  in

  let clause_index doc =
    make_tag "div" (
      [attr "id" "index_of_clauses"],
      [])
  in

  let p_links = make_tag "p" ([], [
      `Data "Jump to:";
      make_tag "a" ([attr "href" "#index_of_clauses"], [`Data "Index of Clauses"]);
    ]) in
  let h1 = make_tag "h1" ([], [`Data doc.title]) in
  let sections = List.map of_section doc.sections in
  let index = clause_index doc in
  make_tag "body" ([], p_links :: h1 :: sections @ [index])

let index_of_refs refs rfc =
  let open ReqtraceTypes.Refs in
  let ref_hash = Hashtbl.create 1000 in
  List.iter (fun impl ->
      let doc_hash = Hashtbl.create (List.length impl.docs) in
      List.iter (fun (name, docid) -> Hashtbl.add doc_hash name docid) impl.docs;
      let add_ref docid ref =
        match docid with
          | RFC n when n = rfc.number -> Hashtbl.add ref_hash ref.reqid ref
          | RFC _ -> ()
          | Uri _ -> ()
      in
      List.iter (fun ref ->
          match ref.docref with
          | Bound name ->
            let docid = Hashtbl.find doc_hash name in
            add_ref docid ref
          | Unbound docid ->
            add_ref docid ref
        ) impl.refs
    ) refs;
  ref_hash

let of_rfc ~css ~js ~refs ~src_base rfc =
  let ref_hash = index_of_refs refs rfc in
  let title = Printf.sprintf "RFC %d: %s" rfc.number rfc.title in
  let head =
    make_tag "head" ([], [
        make_tag "meta" ([attr "charset" "utf=8"], []);
        make_tag "title" ([], [`Data title]);
        make_tag "link" ([
            attr "rel" "stylesheet";
            attr "type" "text/css";
            attr "href" css;
          ], []);
        make_tag "script" ([attr "src" js], [`Data "\n"]);
      ])
  in
  let body = body_of_doc rfc ref_hash src_base in
  make_tag "html" ([], [head; body])

