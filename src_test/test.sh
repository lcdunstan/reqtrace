#!/bin/bash
set -eu
reqtrace=$1
indir=src_test
outdir=_build/src_test

[ ! -d ${outdir} ] && mkdir ${outdir}

(cd ./python && ./test_parseietf.py)

cp ${indir}/example_bad.ml ${outdir}/
ocamlc -bin-annot -c ${outdir}/example_bad.ml
if ${reqtrace} extract --strip=_build/ ${outdir}/example_bad.cmt ; then
    echo "Should have failed"
    exit 1
fi

cp -f ${indir}/example_req.ml ${outdir}/
ocamlc -bin-annot -c ${outdir}/example_req.ml
${reqtrace} extract --strip=_build/ ${outdir}/example_req.cmt
${reqtrace} html --share=python ${indir}/example_spec.xml --ref=${outdir}/example_req.req --base=https://github.com/infidel/reqtrace/blob/master/ -o ${outdir}/example_spec.html
grep coderef ${outdir}/example_spec.html

# Try an example that requires camlp4
cp -f ${indir}/example_cstruct.ml ${outdir}/
cstruct_dir=`ocamlfind query cstruct`
ocamlfind ocamlc -package cstruct -syntax camlp4o -package cstruct.syntax -bin-annot -c ${outdir}/example_cstruct.ml
${reqtrace} extract --strip=_build/ --rfc mdns=6762 --rfc edns0=2671 ${outdir}/example_cstruct.cmt
${reqtrace} html --share=python ${indir}/example_spec.xml --ref=${outdir}/example_cstruct.req --base=https://github.com/infidel/reqtrace/blob/master/ -o ${outdir}/example_cstruct.html
grep coderef ${outdir}/example_cstruct.html

