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

ocamlfind ppx_tools/dumpast -ppx "$ppx_reqtrace _build/example_req.ml.req" src_test/example_req.ml | tail -n +2 > _build/example_req.ml.ppx.ast
diff -u _build/example_noreq.ml.ast _build/example_req.ml.ppx.ast

# For now we just make sure camlp4 doesn't fail, and ignore the output
cstruct_dir=`ocamlfind query cstruct`
ocamlfind ppx_tools/dumpast -pp "camlp4o -I $cstruct_dir cstruct-syntax.cma" -ppx $ppx_reqtrace src_test/example_cstruct.ml | tail -n +2 > _build/example_cstruct.ml.ppx.ast

(cd ./python && ./test_parseietf.py)

[ ! -d _build/src_test ] && mkdir _build/src_test
cp -f src_test/example_req.ml _build/src_test/
ocamlc -bin-annot -c _build/src_test/example_req.ml
./reqtrace.native extract --strip=_build/ _build/src_test/example_req.cmt
./python/rfc_notes.py src_test/example_spec.xml --ref=_build/src_test/example_req.req --html=_build/src_test/example_spec.html
grep coderef _build/src_test/example_spec.html
