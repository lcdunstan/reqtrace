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

open ReqtraceTypes.RFC

let xmlns = ""

module StringMap = Map.Make(String)

let empty path = {
  number = 0;
  title = "";
  sections = [];
}

let fail_xml msg xml path =
  let line, col = Xmlm.pos xml in
  failwith (Printf.sprintf "%s:%d: %s (col %d)" path line msg col)

let rec require_attr attrs name fail_xml =
  match attrs with
  | ((ns,attr_name),value) :: tl ->
    if ns = xmlns && attr_name = name then
      value
    else
      require_attr tl name fail_xml
  | [] -> fail_xml ("Missing required attribute " ^ name)

let rec optional_attr attrs name =
  match attrs with
  | ((ns,attr_name),value) :: tl ->
    if ns = xmlns && attr_name = name then
      Some value
    else
      optional_attr tl name
  | [] -> None

(* Returns a string *)
let rec read_text text xml path : string =
  match Xmlm.input xml with
  | `El_start _ -> fail_xml "expected text only" xml path
  | `Data data -> read_text (text ^ data) xml path
  | `Dtd _ -> read_text text xml path
  | `El_end -> text

let rec of_xml path xml =
  let fail_xml msg = fail_xml msg xml path in
  let read_text text = read_text text xml path in
  (* Returns a list of notes, in reverse order *)
  let rec read_notes notes =
    match Xmlm.input xml with
    | `El_start ((ns,"note"),attrs) when ns = xmlns ->
      let todo = match optional_attr attrs "type" with
        | None -> false
        | Some value when value = "todo" -> true
        | Some _ -> false
      in
      let text = read_text "" in
      let note = { text; todo; } in
      read_notes (Note note :: notes)
    | `El_start ((ns,"ref"),attrs) when ns = xmlns ->
      let text = read_text "" in
      let target = match optional_attr attrs "target" with
        | None -> text
        | Some value -> value
      in
      read_notes (Ref target :: notes)
    (* TODO: coderef *)
    | `El_start _ -> fail_xml "expected <note> or <ref> in <notes>"
    | `Data _ | `Dtd _ -> read_notes notes
    | `El_end -> notes
  in
  let importance_of_attr = function
    | Some "must" -> Must
    | Some "should" -> Should
    | Some "may" -> May
    | Some _ -> Not
    | None -> Not
  in
  (* Returns a clause record, with line substrings and notes in reverse order *)
  let rec read_clause (clause:clause) =
    match Xmlm.input xml with
    | `El_start ((ns,"linesub"),attrs) when ns = xmlns ->
      let start_offset = require_attr attrs "start" fail_xml in
      let end_offset = require_attr attrs "end" fail_xml in
      let text = read_text "" in
      let line = { start_offset = int_of_string start_offset; end_offset = int_of_string end_offset; text; } in
      read_clause { clause with lines = line :: clause.lines }
    | `El_start ((ns,"notes"),attrs) when ns = xmlns ->
      read_clause { clause with notes = read_notes clause.notes }
    | `El_start _ -> fail_xml "expected <linesub> and <notes> in <clause>"
    | `Data _ | `Dtd _ -> read_clause clause
    | `El_end -> clause
  in
  (* Returns a paragraph record, with lines and clauses in reverse order *)
  let rec read_paragraph paragraph =
    match Xmlm.input xml with
    | `El_start ((ns,"line"),attrs) when ns = xmlns ->
      let start_offset = require_attr attrs "start" fail_xml in
      let end_offset = require_attr attrs "end" fail_xml in
      let text = read_text "" in
      let line = { start_offset = int_of_string start_offset; end_offset = int_of_string end_offset; text; } in
      read_paragraph { paragraph with lines = line :: paragraph.lines }
    | `El_start ((ns,"clause"),attrs) when ns = xmlns ->
      let id = optional_attr attrs "id" in
      let importance = importance_of_attr (optional_attr attrs "importance") in
      let clause_rev = read_clause { id; lines=[]; notes=[]; importance; } in
      let clause = { clause_rev with lines = List.rev clause_rev.lines; notes = List.rev clause_rev.notes; } in
      read_paragraph { paragraph with clauses = clause :: paragraph.clauses }
    | `El_start _ -> fail_xml "expected <line> or <clause> in <paragraph>"
    | `Data _ | `Dtd _ -> read_paragraph paragraph
    | `El_end -> paragraph
  in
  (* Returns a section record, with paragraphs in reverse order *)
  let rec read_section section =
    match Xmlm.input xml with
    | `El_start ((ns,"paragraph"),attrs) when ns = xmlns ->
      let id = optional_attr attrs "id" in
      let para_rev = read_paragraph { id; lines=[]; clauses=[]; } in
      let para = { para_rev with lines = List.rev para_rev.lines; clauses = List.rev para_rev.clauses } in
      read_section { section with paras = para :: section.paras }
    | `El_start _ -> fail_xml "expected <paragraph> in <section>"
    | `Data _ | `Dtd _ -> read_section section
    | `El_end -> section
  in
  let rec read_value value =
    match Xmlm.input xml with
    | `El_start _ -> fail_xml "expected text in <value>"
    | `Data _ | `Dtd _ -> (*TODO*) read_value value
    | `El_end -> value
  in
  let rec read_header rfc =
    match Xmlm.input xml with
    | `El_start ((ns,"value"),_) when ns = xmlns ->
      read_header (read_value rfc)
    | `El_start _ -> fail_xml "expected <value> in <header>"
    | `Data _ | `Dtd _ -> read_header rfc
    | `El_end -> rfc
  in
  (* Returns a list of sections, in reverse *)
  let rec read_sections sections =
    match Xmlm.input xml with
    | `El_start ((ns,"section"),attrs) when ns = xmlns ->
      let name = require_attr attrs "name" fail_xml in
      let id = optional_attr attrs "id" in
      let section_rev = read_section { name; id; paras=[]; } in
      let section = { section_rev with paras = List.rev section_rev.paras } in
      read_sections (section :: sections)
    | `El_start _ -> fail_xml "expected <section> in <sections>"
    | `Data _ | `Dtd _ -> read_sections sections
    | `El_end -> sections
  in
  (* Returns an rfc record *)
  let rec read_rfc rfc =
    match Xmlm.input xml with
    | `El_start ((ns,"header"),_) when ns = xmlns ->
      read_rfc (read_header rfc)
    | `El_start ((ns,"sections"),_) when ns = xmlns ->
      { rfc with sections = List.rev (read_sections []) }
    | `El_start _ -> fail_xml "expected <header> and <sections> in element in <rfc>" 
    | `Data _ | `Dtd _ -> read_rfc rfc
    | `El_end -> rfc
  in
  (* Returns an rfc record *)
  let read_root = function
    | ((ns,"rfc"),attrs) when ns = xmlns ->
      let number = require_attr attrs "number" fail_xml in
      let title = require_attr attrs "title" fail_xml in
      read_rfc { number = int_of_string number; title; sections = [] }
    | _ -> fail_xml "expected root node <rfc num='nnn' title='...'>"
  in
  match Xmlm.input xml with
  | `El_start tag -> read_root tag
  | `El_end -> empty path
  | `Data _ | `Dtd _ -> of_xml path xml

let read path =
  let ic = open_in path in
  let input = Xmlm.make_input (`Channel ic) in
  let rfc = of_xml path input in
  let () = close_in ic in
  rfc

