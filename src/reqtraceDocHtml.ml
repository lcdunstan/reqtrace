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

let strings_of_importance = function
  | Must -> "must", "MUST, SHALL"
  | Should -> "should", "SHOULD, RECOMMENDED"
  | May -> "may", "MAY, OPTIONAL"
  | Not -> "other", "OTHER"

let ns = ""
let attr name value = ((ns, name), value)

let opt_attr name opt_value attrs =
  match opt_value with
  | None -> attrs
  | Some value -> attr name value :: attrs

let of_line (line:linesub) =
  (* TODO: start/end/num *)
  make_tag "span" ([attr "class" "line"], [`Data line.text])

let fold_clauses f a rfc =
  List.fold_left (fun acc s ->
      List.fold_left (fun acc p ->
          List.fold_left f acc p.clauses)
        acc s.paras)
    a rfc.sections

let body_of_doc doc ref_hash src_base =
  let of_ref target =
    make_tag "div" ([attr "class" "ref"], [
        `Data "See ";
        make_tag "a" ([attr "href" ("#" ^ target)], [`Data target]);
      ])
  in

  let string_of_reftype reftype path =
    let open ReqtraceTypes.Refs in
    match reftype with
      | Impl -> "impl"
      | Test -> "test"
      | Unknown -> match Stringext.find_from path "test" with
        | None -> "impl"
        | Some i -> "test"
  in

  let open ReqtraceTypes.Refs in
  let of_reqref { loc={Location.loc_start={Lexing.pos_fname=path; Lexing.pos_lnum=linenum}}; reftype; } =
    let reftype_str = string_of_reftype reftype path in
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
    let paras = if section.name = "Table of Contents" then
        List.map of_toc_paragraph section.paras
      else
        List.map of_paragraph section.paras
    in
    (* TODO: <notes> in <section> *)
    make_tag "div" ([attr "class" "section"], heading :: paras)
  in

  let clause_table doc importance =
    let imp_id, imp_heading = strings_of_importance importance in
    let headers = ["Clause"; "Notes"; "Impl"; "Test"; "TODO"] in
    let row_of_clause clause id =
      let code_refs = Hashtbl.find_all ref_hash id in
      let has_notes, has_todo = List.fold_left (
          fun (has_notes, has_todo) child ->
            match child with
            | Note note -> (true, has_todo || note.todo)
            | Ref ref -> (true, has_todo)
            | CodeRef _ -> (has_notes, has_todo)
        ) (false, false) clause.notes
      in
      let has_impl, has_test =
        List.fold_left (
          fun (has_impl, has_test) { loc={Location.loc_start={Lexing.pos_fname=path; Lexing.pos_lnum=linenum}}; reftype; } ->
            let reftype_str = string_of_reftype reftype path in
            (has_impl || reftype_str = "impl", has_test || reftype_str = "test")
        ) (false, false) code_refs
      in
      make_tag "tr" ([], [
          make_tag "td" ([], [
              make_tag "a" ([attr "href" ("#" ^ id)], [`Data id])
            ]);
          make_tag "td" ([], [`Data (if has_notes then "Yes" else "")]);
          make_tag "td" ([], [`Data (if has_impl then "Yes" else "")]);
          make_tag "td" ([], [`Data (if has_test then "Yes" else "")]);
          make_tag "td" ([], [`Data (if has_todo then "Yes" else "")]);
        ])
    in
    let rows_rev = fold_clauses (fun l clause ->
        if clause.importance = importance then
          match clause.id with
          | None -> l
          | Some id -> (row_of_clause clause id) :: l
        else
          l
      ) [] doc
    in
    make_tag "div" (
      [
        attr "class" "index";
        attr "id" ("clauses_" ^ imp_id)
      ],
      [
        make_tag "h2" ([], [`Data imp_heading]);
        make_tag "table" (
          [attr "class" "index"],
          [
            make_tag "thead" ([], List.map (fun h -> make_tag "th" ([], [`Data h])) headers);
            make_tag "tbody" ([], List.rev rows_rev);
          ])
      ])
  in

  let clause_index doc =
    make_tag "div" (
      [attr "id" "index_of_clauses"],
      [
        make_tag "h1" ([], [`Data "Index of Clauses"]);
        clause_table doc Must;
        clause_table doc Should;
        clause_table doc May;
      ])
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

