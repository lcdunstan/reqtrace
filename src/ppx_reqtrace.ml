open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let filter_attribute attribute =
  match attribute with
  | ({ txt = "req"; loc = attr_loc}, payload) ->
    begin match payload with
      | (* Should have a single structure item, which is evaluation of a constant string. *)
        PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
        (* Delete the attribute *)
        false
      | _ ->
        raise (Location.Error (
            Location.error ~loc:attr_loc "[@req] accepts a string, e.g. [@req \"6372.s9_p1_c2\"]"))
    end
  | _ -> true

let reqtrace_mapper argv =
  (* Our reqtrace_mapper only overrides the handling of attribute lists in the default mapper. *)
  { default_mapper with
    attributes = fun mapper attributes ->
      default_mapper.attributes mapper (List.filter filter_attribute attributes)
  }

let () = register "reqtrace" reqtrace_mapper
