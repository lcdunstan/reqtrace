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

type elemid = string

module RFC = struct
  type linesub = {
    start_offset: int;
    end_offset: int;
    text: string;
  }

  type note = {
    text: string;
    todo: bool;
  }

  type notes_child =
    | Note of note
    | Ref of string
    | CodeRef of string

  type clause = {
    id: elemid option;
    lines: linesub list;
    notes: notes_child list;
  }

  type paragraph = {
    id: elemid option;
    lines: linesub list;
    clauses: clause list;
  }

  type section = {
    name: string;
    id: elemid option;
    paras: paragraph list;
  }

  type rfc = {
    number: int;
    title: string;
    sections: section list;
  }
end

module Refs = struct
  type docid = RFC of int | Uri of string

  type docbind = string * docid

  type docref = Bound of string | Unbound of docid

  type reftype = Impl | Test | Unknown

  type reqref = {
    docref : docref;
    reqid : string;
    loc : Location.t;
    reftype : reftype;
  }

  type impl_unit = {
    docs : docbind list;
    refs : reqref list;
  }
end
