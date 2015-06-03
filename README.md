
Through the combination of indexing requiments specification
documents (especially IETF RFCs) and the use of attributes in OCaml
source code referring to specific requirements, this tool can produce
an annotated requirements specification that includes references to
the code that implements and/or tests each requirement.
This also allows unimplemented or untested requirements to be identified
automatically.

# Basics

## References to Requirements

References from OCaml source code to requirements specifications
is done using attribute syntax: http://caml.inria.fr/pub/docs/manual-ocaml/extn.html#attr-id

The most common case is to add an attribute after an expression:

```ocaml
let _ =
  hello (something 1) [@req (rfc 9999) "s18"]
```

The above attribute is intended to express that the adjacent code
implements the requirement defined in section 18 of fictional RFC 9999.
Currently the hyperlink generated for such references links
to the line of code, and is intended for human readers, so
the precise placement of the attribute is not important.

## References to Specification Documents

To make it easier to remember the meaning of RFC numbers,
and to enable less verbose references,
it is possible to express the same reference as follows:

```ocaml
[@@@specdoc let foo = rfc 9999]

let _ =
  hello (something 1) [@req foo "s18"]
```

The name `foo` can be anything that has meaning to you.

Documents other than IETF RFCs can be referenced using URIs:

```ocaml
[@@@specdoc let reqif = uri "http://www.omg.org/spec/ReqIF/1.1/"]
```

## Types of References

When verifying traceability the goal is typically to ensure
that all requirements have been implemented, and also that
all requirements have been tested.

To express the difference between implementation code and
test code, use the following near the top of the source file:

```ocaml
[@@@reftype Impl]
```

or:

```ocaml
[@@@reftype Test]
```

## Specification XML Format

The format of annotated specification documents is yet
to be documented. An example can be found at:

https://github.com/infidel/ocaml-mdns/blob/master/doc/rfc6762_notes.xml

## reqtrace tool invocation

To generate the necessary metadata required by the `reqtrace` tool,
pass the `-bin-annot` option to the `ocamlc` compiler command.
This will generate a `.cmt` file for each OCaml source file.

After compiling the code, invoke the `reqtrace extract`
command and pass the path to the root directory containing
the `.cmt` files:

```
reqtrace extract --strip=_build/ _build
```

This will generate a set of `.req` XML files in the same directory
structure. The combine the specification XML with the references
to generate an annotated specification in HTML format:

```
rfc_notes.py example_spec.xml --ref=_build --base=https://github.com/foo/bar/blob/master/ --html=example_spec.html
```

# Support for camlp4

If your OCaml code requires preprocessing using camlp4 then
only a subset of attribute syntax can be used, because of:
https://github.com/ocaml/camlp4/issues/55

The recommended usage is:

```ocaml
let _ =
  hello (something 1) [@req foo "s18"]
```

One reason for this is that camlp4 does not support
block attributes (`[@@attr...]) or floating attributes (`[@@@attr...]`).
While it is possible to use expression attributes for
`@specdoc` and `@reftype`, this is not so convenient.

Since no `@specdoc` attribute defines the name `foo`, it is necessary
to use the `reqtrace` command line option `--rfc foo=9999` instead.

Unfortunately an attribute such as `[@req (rfc 1234) "s3_p2"]`
is corrupted by camlp4, causing it to be interpreted as `[@req rfc 1234 "s3_p2"]`,
so named specifications are required.

# Conversion of RFCs to XML

The annotated RFC 6762 specification mentioned above was initially generated
by running:

```
parseietf.py rfc6762.txt --xml rfc6762_notes.xml
```

However, this script only been tested on very few IETF RFCs,
so it is unlikely to work on a significant percentage of RFCs
(because they were not really intended to be machine-readable).

