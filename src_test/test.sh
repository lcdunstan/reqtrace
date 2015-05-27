#!/bin/bash
set -eu
ppx_reqtrace=$1
indir=src_test
outdir=_build/src_test

[ ! -d ${outdir} ] && mkdir ${outdir}

# NOTE: the default is for dumpast to discard location data, which allows AST comparison
# NOTE: tail is skipping the first line, which is just the filename
ocamlfind ppx_tools/dumpast ${indir}/example_req.ml | tail -n +2 > ${outdir}/example_req.ml.ast
ocamlfind ppx_tools/dumpast ${indir}/example_noreq.ml | tail -n +2 > ${outdir}/example_noreq.ml.ast
if diff -u ${outdir}/example_req.ml.ast ${outdir}/example_noreq.ml.ast >/dev/null ; then
    echo "Should be different"
    exit 1
fi

ocamlfind ppx_tools/dumpast -ppx "$ppx_reqtrace ${outdir}/example_req.ml.req" ${indir}/example_req.ml | tail -n +2 > ${outdir}/example_req.ml.ppx.ast
diff -u ${outdir}/example_noreq.ml.ast ${outdir}/example_req.ml.ppx.ast

# For now we just make sure camlp4 doesn't fail, and ignore the output
cstruct_dir=`ocamlfind query cstruct`
ocamlfind ppx_tools/dumpast -pp "camlp4o -I $cstruct_dir cstruct-syntax.cma" -ppx $ppx_reqtrace ${indir}/example_cstruct.ml | tail -n +2 > ${outdir}/example_cstruct.ml.ppx.ast

(cd ./python && ./test_parseietf.py)

cp -f ${indir}/example_req.ml ${outdir}/
ocamlc -bin-annot -c ${outdir}/example_req.ml
./reqtrace.native extract --strip=_build/ ${outdir}/example_req.cmt
./python/rfc_notes.py ${indir}/example_spec.xml --ref=${outdir}/example_req.req --base=https://github.com/infidel/reqtrace/blob/master/ --html=${outdir}/example_spec.html
grep coderef ${outdir}/example_spec.html
