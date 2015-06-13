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

let of_ref target =
  make_tag "div" ([attr "class" "ref"], [
      `Data "See ";
      make_tag "a" ([attr "href" ("#" ^ target)], [`Data target]);
    ])

let of_notes_child = function
  | Note { text; todo; } -> make_tag "div" ([attr "class" "note"], [`Data text])
  | Ref target -> of_ref target
  | CodeRef ref -> make_tag "div" ([attr "class" "coderef"], [`Data ref]) (*TODO*)

let of_clause (clause:clause) =
  let label = match clause.id with
    | None -> []
    | Some id -> [make_tag "span" ([attr "class" "label"], [`Data id])]
  in
  let notes = List.map of_notes_child clause.notes in
  let notes = if notes = [] then [] else
      [make_tag "div" ([attr "class" "notes"], notes)]
  in
  let text = String.concat " " (List.map (fun (line:linesub) -> line.text) clause.lines) in
  make_tag "div" (
    opt_attr "id" clause.id
      [attr "class" "clause"],
    label @ notes @ [`Data text])

let of_toc_paragraph paragraph =
  let text = String.concat "\n" (List.map (fun (line:linesub) -> line.text) paragraph.lines) in
  make_tag "pre" ([attr "class" "toc"], [`Data text])

let of_paragraph paragraph =
  let lines = List.map of_line paragraph.lines in
  let clauses = List.map of_clause paragraph.clauses in
  (* TODO: <notes> in <paragraph> *)
  make_tag "div" ([attr "class" "paragraph"], lines @ clauses)

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

let clause_index doc =
  make_tag "div" (
    [attr "id" "index_of_clauses"],
    [])

let body_of_doc doc =
  let p_links = make_tag "p" ([], [
      `Data "Jump to:";
      make_tag "a" ([attr "href" "#index_of_clauses"], [`Data "Index of Clauses"]);
    ]) in
  let h1 = make_tag "h1" ([], [`Data doc.title]) in
  let sections = List.map of_section doc.sections in
  let index = clause_index doc in
  make_tag "body" ([], p_links :: h1 :: sections @ [index])

let of_rfc ~normal_uri ~uri_of_path ~css ~js ~refs rfc =
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
  let body = body_of_doc rfc in
  make_tag "html" ([], [head; body])

