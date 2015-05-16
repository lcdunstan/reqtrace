open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

type reqref = {
  reqid : string;
  loc : Location.t;
}

type state = {
  mutable refs : reqref list;
}

let state = { refs=[] }

let filter_attribute attribute =
  match attribute with
  | ({ txt = "req"; loc = attr_loc}, payload) ->
    begin match payload with
      | (* Should have a single structure item, which is evaluation of a constant string. *)
        PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
        (* Store it *)
        state.refs <- { reqid=sym; loc=attr_loc } :: state.refs;
        (* Delete the attribute *)
        false
      | _ ->
        raise (Location.Error (
            Location.error ~loc:attr_loc "[@req] accepts a string, e.g. [@req \"6372.s9_p1_c2\"]"))
    end
  | _ -> true

let reqtrace_mapper =
  (* Our reqtrace_mapper only overrides the handling of attribute lists in the default mapper. *)
  { default_mapper with
    attributes = fun mapper attributes ->
      default_mapper.attributes mapper (List.filter filter_attribute attributes)
  }

let to_xml reqref =
  let filename, linenum, charnum = Location.get_pos_info (reqref.loc.Location.loc_start) in
  let ns = "" in
  let reqid = ((ns, "reqid"), reqref.reqid) in
  let path = ((ns, "path"), filename) in
  let line = ((ns, "line"), (string_of_int linenum)) in
  Ezxmlm.make_tag "reqref" ([reqid; path; line], [])

let save_req filename =
  let ch = open_out filename in
  let dtd = None in
  let nodes = List.map to_xml state.refs in
  let root = Ezxmlm.make_tag "refs" ([], nodes) in
  Ezxmlm.to_channel ch dtd [root];
  close_out ch

let getenv s = try Sys.getenv s with Not_found -> ""

let run_main mapper =
  try
    let a = Sys.argv in
    let n = Array.length a in
    let source = a.(n - 2) in
    let target = a.(n - 1) in
    if n > 2 then
      begin
        apply ~source ~target mapper;
        if n > 3 then
          save_req a.(n - 3)
      end
    else begin
      Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
                     Sys.executable_name;
      exit 2
    end
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2

let () =
  run_main reqtrace_mapper

