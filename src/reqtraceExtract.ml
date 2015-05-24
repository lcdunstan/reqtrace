(*
 * Copyright (c) 2015 Luke Dunstan <LukeDunstan81@gmail.com>
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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

module Error = struct
  let source_missing path =
    `Error (false, "source "^path^" does not exist")

  let dir_to_file dir file =
    `Error (false, "can't process directory "^dir^" into file "^file)

  (*
  let unknown_file_type path =
    `Error (false, "don't know how to handle file "^path)
    `Error (false, "source "^in_file^" is not a cmt")

  let not_an_interface path =
    `Error (false, path^" is not an interface")

  let wrong_version_interface path =
    `Error (false, path^" has the wrong format version")

  let not_an_implementation path =
    `Error (false, path^" is not an implementation")

  let wrong_version_implementation path =
    `Error (false, path^" has the wrong format version")

  let corrupted_interface path =
    `Error (false, path^" is corrupted")

  let not_a_typedtree path =
    `Error (false, path^" is not a typed tree")
  *)

  let read_cmt_failed path msg =
    `Error (false, path ^ ": failed to load cmt: " ^ msg)
end

let combine_errors errs = `Error
  begin List.fold_left (fun (show_help,str) -> function
  | `Error (err_help,err_str) -> (err_help || show_help, str ^ "\n" ^ err_str)
  ) (false,"") errs
  end

module Dir = struct
  module Error = struct
    let nondirectory_segment path =
      `Error (false, "path "^path^" is not a directory")
  end

  let rec make_exist ~perm path =
    try Unix.access path []; None
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
      let dir = Filename.dirname path in
      begin match make_exist ~perm dir with
      | None ->
        Unix.(mkdir path perm);
        None
      | Some err -> Some err
      end
    | Unix.Unix_error (Unix.ENOTDIR, _, _) ->
      Some (Error.nondirectory_segment path)

  let make_dirs_exist ~perm =
    List.fold_left (fun err_opt path ->
      match err_opt with None -> make_exist ~perm path | Some err -> Some err
    ) None

  let name path = match Filename.dirname path with "." -> "" | p -> p
end

let (/) = Filename.concat

let cmt_path path output = ReqtraceUtil.(rel_of_path (depth output) path)

let xml_filename_of_cmt cmt =
  Filename.(chop_suffix (basename cmt) ".cmt")^".req"

let dir_of_cmt cmt = Filename.(chop_suffix (basename cmt) ".cmt")
let xml_index_of_cmt cmt = (dir_of_cmt cmt) / "index.req"

let only_cmt file path =
  Filename.check_suffix file ".cmt"

let all_cmts dir =
  ReqtraceUtil.foldp_paths (fun lst rel_cmt -> rel_cmt::lst) only_cmt [] dir

let extract cmt out_dir rel_xml =
  let xml = out_dir / rel_xml in
  let dirs = [Dir.name xml] in
  (* here, we rely on umask to set the perms correctly *)
  match Dir.make_dirs_exist ~perm:0o777 dirs with
  | Some err -> err
  | None ->
    match ReqtraceCmt.read_cmt cmt with
    | `Error msg -> Error.read_cmt_failed cmt msg
    | `Ok unit ->
      let oc = open_out xml in
      let xout = Xmlm.make_output (`Channel oc) in
      ReqtraceXml.output_impl_unit xout unit;
      close_out oc;
      `Ok unit

let extract_file in_file out_dir xml_file =
  ReqtraceUtil.map_ret (fun _ -> ())
    (extract in_file out_dir xml_file)

let run_dir in_dir out_dir =
  let cmts = all_cmts in_dir in
  let cmt_count = List.length cmts in
  Printf.printf
    "%4d cmt under %s\n" cmt_count in_dir;
  match List.fold_left (fun (units,errs) rel_cmt ->
      let rel_dir = Dir.name rel_cmt in
      let xml_file = xml_index_of_cmt rel_cmt in
      match extract (in_dir / rel_cmt) out_dir (rel_dir / xml_file)
      with
      | `Ok unit -> (unit::units, errs)
      | `Error err -> (units, (`Error err)::errs)
    ) ([],[]) cmts
  with
  | _, ((_::_) as errs) -> combine_errors errs
  | units, [] -> `Ok (`Dir out_dir)

let run output path =
  match path, output with
  | `Missing path, _ -> Error.source_missing path
  | `File in_file, _ when not (Filename.check_suffix in_file ".cmt") ->
    `Error (false, "source "^in_file^" is not a cmt")
  | `File in_file, None ->
    let xml_file = xml_filename_of_cmt in_file in
    let out_dir = Dir.name in_file in
    ReqtraceUtil.map_ret
      (fun () -> `File xml_file)
      (extract_file in_file out_dir xml_file)
  | `File in_file, Some (`Missing out_file | `File out_file) ->
    (* simple doc gen *)
    let out_dir = Dir.name out_file in
    let rel_file = Filename.basename out_file in
    ReqtraceUtil.map_ret
      (fun () -> `File out_file)
      (extract_file in_file out_dir rel_file)
  | `File in_file, Some (`Dir out_dir) ->
    ReqtraceUtil.map_ret (fun _ -> `Dir out_dir)
      (extract in_file out_dir (xml_index_of_cmt in_file))
  | `Dir in_dir, None -> run_dir in_dir in_dir
  | `Dir in_dir, Some (`Missing out_dir | `Dir out_dir) ->
    run_dir in_dir out_dir
  | `Dir in_dir, Some (`File out_file) -> Error.dir_to_file in_dir out_file

