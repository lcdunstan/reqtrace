#!/bin/bash
set -eu
ppx_reqtrace=$1

# NOTE: the default is for dumpast to discard location data, which allows AST comparison
# NOTE: tail is skipping the first line, which is just the filename
ocamlfind ppx_tools/dumpast src_test/example_req.ml | tail -n +2 > _build/example_req.ml.ast
ocamlfind ppx_tools/dumpast src_test/example_noreq.ml | tail -n +2 > _build/example_noreq.ml.ast
if diff -u _build/example_req.ml.ast _build/example_noreq.ml.ast >/dev/null ; then
    echo "Should be different"
    exit 1
fi

ocamlfind ppx_tools/dumpast -ppx $ppx_reqtrace src_test/example_req.ml | tail -n +2 > _build/example_req.ml.ppx.ast
diff -u _build/example_noreq.ml.ast _build/example_req.ml.ppx.ast

#ocamlfind ppx_tools/dumpast -ppx $ppx_reqtrace src_test/example_cstruct | tail -n +2 > _build/example_req.ml.ppx.ast
