
Through the combination of indexing requiments specification
documents (especially IETF RFCs) and the use of attributes in OCaml
source code referring to specific requirements, this tool can produce
an annotated requirements specification that includes references to
the code that implements and/or tests each requirement.
This also allows unimplemented or untested requirements to be identified
automatically.

Only a subset of attribute syntax can be used, because of:
https://github.com/ocaml/camlp4/issues/55

The extraction of attributes could be done two ways, each with advantages:

- During compilation as a ppx transformation.
    - Execution of camlp4 (e.g. cstruct.syntax) happens automatically.
    - Allows compiler warnings/errors to be emitted.
    - May help to avoid conflicts with other PPX extensions (or not).
    - Could the dependency on ppx_reqtrace be made optional?
    - Fits better with OASIS?
    - PPX is intended for transformations, but we need to generate separate files containing the references.
    - How to tell ppx_reqtrace where to read the spec(s) from (for validating references)?
    - How to tell ppx_reqtrace where to write the extracted references to?
- As an independent tool, similar to ocamldoc.
    - Maybe we don't want bad attributes to be compiler errors/warnings?
    - Avoids a little extra time in the edit-compile-debug cycle (doesn't need to be run often).
    - Don't need the extension installed to build something.
    - Easy to pass more input/output files apart from the OCaml source files.
    - Could run a single command to process all source files, instead of running per source file (efficency).

Either way, the syntax and the end result would be the same.

