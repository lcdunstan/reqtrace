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

let map_ret f = function
  | `Ok v -> `Ok (f v)
  | `Error (help,msg) as err -> err

let rec read_files acc dh =
  match
    try Some (Unix.readdir dh)
    with End_of_file -> None
  with Some file -> read_files (file::acc) dh | None -> acc

let rec all_files base acc dh =
  let files = read_files [] dh in
  List.fold_left (fun acc -> function
  | "." | ".." -> acc
  | dirent ->
    let file = Filename.concat base dirent in
    try
      let dh = Unix.opendir file in
      let acc = all_files file acc dh in
      Unix.closedir dh;
      acc
    with
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> file::acc
    | Unix.Unix_error (Unix.ENOENT,  _, _) -> (* dangling symlink or race *)
      acc
  ) acc files

let in_dir path f =
  let cwd = Unix.getcwd () in
  Unix.chdir path;
  try let r = f () in Unix.chdir cwd; r
  with e -> Unix.chdir cwd; raise e

let foldp_paths f p acc dir =
  let dh = Unix.opendir dir in
  let files = in_dir dir (fun () -> all_files "" [] dh) in
  let () = Unix.closedir dh in
  List.fold_left (fun acc file ->
    if p file dir then f acc file else acc
  ) acc files

let rec ascent_of_depth tl = function
  | 0 -> tl
  | n -> ascent_of_depth ("../" ^ tl) (n - 1)

let depth path =
  max 0 (List.length (Stringext.split path ~on:'/') - 1)

let rel_of_path depth path =
  if path <> "" && path.[0] = '/'
  then path
  else (ascent_of_depth "" depth) ^ path

let is_link path =
  let open Unix in
  try
    (lstat path).st_kind = S_LNK
  with
  | Unix.Unix_error _ -> false

let copy in_file out_file =
  if is_link out_file then
    `Error (false, out_file ^ " is a symbolic link")
  else
    let page_size = 4096 in
    let ic = open_in_bin  in_file in
    let oc = open_out_bin out_file in
    let buf = Bytes.create page_size in
    let rec copy_more () =
      match input ic buf 0 page_size with
      | 0 -> ()
      | len -> output oc buf 0 len; copy_more ()
    in
    copy_more ();
    close_in ic;
    close_out oc;
    `Ok out_file

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

