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

open Cmdliner

let version = "0.0.1"

let global_option_section = "COMMON OPTIONS"

let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "AUTHORS";
  `P "Luke Dunstan <LukeDunstan81@gmail.com>";
  `S "BUGS";
  `P "Browse and report new issues at"; `Noblank;
  `P "<https://github.com/infidel/reqtrace/issues>.";
]

(*
module Common = struct
  type t = {
    force : bool;
    index : bool;
  }

  let create_t force index = { force; index; }
  let create ~force ~index = create_t force index

  let force_arg = Arg.(value (
    let docv = "FORCE" in
    let doc = "force the execution of the command" in
    flag & info ~docs:global_option_section ~docv ~doc ["f"]
  ))

  let index_arg = Arg.(value (
    let docv = "INDEX" in
    let doc =
      "whether to update indexes and the relative path of the index file"
    in
    flag & info ~docs:global_option_section ~docv ~doc ["index"]
  ))

  let term = Term.(pure create_t $ force_arg $ index_arg)
end
*)

type path = [
  | `File of string
  | `Dir of string
  | `Missing of string
]

let rec to_path path = Unix.(
  try match (stat path).st_kind with
  | S_DIR -> `Ok (`Dir path)
  | S_REG -> `Ok (`File path)
  | S_LNK | S_CHR | S_BLK | S_FIFO | S_SOCK ->
    `Error (false, "unsupported file type")
  with
  | Unix_error (ENOENT,_,_) -> `Ok (`Missing path)
  | Unix_error (e,_,_) -> `Error (false, path^": "^(error_message e))
)

let map f = Term.(app (pure f))
let ret_map f t = Term.ret (map f t)

let path ~doc arg =
  Arg.(ret_map to_path (required (
    let docv = "PATH" in
    arg (some string) None & info ~docv ~doc []
  )))

let path_opt ~doc names =
  Arg.(ret_map (function
  | None -> `Ok None
  | Some x -> ReqtraceUtil.map_ret (fun x -> Some x) (to_path x)
  ) (value (
    let docv = "PATH" in
    Arg.opt (some string) None & info ~docv ~doc names
  )))

let output = path_opt ~doc:"the output path" ["o"]

let strip =
  Arg.(
    value &
    opt (some string) None &
    info ~docv:"PREFIX" ~doc:"the path prefix to strip from code references" ["s"; "strip"]
  )

let rfc =
  Arg.(
    value &
    opt_all (pair ~sep:'=' string int) [] &
    info ~docv:"name=number" ~doc:"defines a friendly name for the RFC with the specified number" ["r"; "rfc"]
  )

let extract_cmd =
  let doc = "extract references to requirements from cmt files into XML" in
  let man = [

  ] @ help_sections
  in
  let path' = path
    ~doc:"the module, interface, or directory to extract"
    (Arg.pos 0)
  in
  Term.(ret (pure (ReqtraceUtil.map_ret (fun _ -> ())) $
             (pure ReqtraceExtract.run
              $ strip $ rfc
              $ output $ path')),
        info "extract" ~doc ~sdocs:global_option_section ~man)

let default_cmd =
  let exec_name = Filename.basename Sys.argv.(0) in
  let doc = "analyse requirement traceability of OCaml code" in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b, "^exec_name^") analyses requirements traceability of OCaml code.");
  ] @ help_sections
  in
  let no_cmd_err _ = `Error (true, "No command specified.") in
  Term.(ret (pure no_cmd_err $ pure () (*Common.term*)),
        info exec_name ~version ~sdocs:global_option_section
          ~doc ~man)

let () =
  match Term.eval_choice default_cmd [
      extract_cmd;
    ] with
  | `Ok () | `Version | `Help -> exit 0
  | `Error _ -> exit 1
