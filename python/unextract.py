#!/usr/bin/env python3

import os
import os.path
import re
import xml.etree.ElementTree as etree

from rfc_notes import ParseException
NS = ''


REFTYPES = { 'impl' : 'Impl', 'test' : 'Test' }

class Reference:
    def __init__(self, linenum, id):
        self.linenum = linenum
        self.id = id


class SourceFile:
    def __init__(self, path, reftype):
        self.reftype = reftype
        self.refs = []

    def insert_refs(self, lines, docref, offset):
        for ref in self.refs:
            i = ref.linenum - 1 + offset
            attr = ' [@ref {0} "{1}"]'.format(docref, ref.id)
            line = lines[i].rstrip('\n')
            if line.find(attr) == -1:
                lines[i] = line + attr + '\n'


def get_target(elem, parent_map):
    assert elem.tag == NS + 'coderef'
    parent = parent_map[elem]
    assert parent.tag == NS + 'notes'
    grandparent = parent_map[parent]
    assert grandparent.tag in (NS + 'clause', NS + 'paragraph', NS + 'section')
    return grandparent.get('id')


def add_header(lines, header):
    offset = 0
    i = 0
    # Skip comments, blank lines and existing attributes at the beginning of the file
    while i < len(lines):
        line = lines[i].lstrip()
        if line.startswith('(*'):
            start_comment = i
            # Find the end of the comment
            while i < len(lines) and lines[i].find('*)') == -1:
                # TODO: support nested comments
                assert lines[i].find('(*') == -1 or i == start_comment
                i += 1
        elif line:
            if line.find('[@') == -1:
                break
            elif line in header:
                header.remove(line)
        i += 1
    if header:
        for line in header:
            lines.insert(i, line)
            offset += 1
            i += 1
        lines.insert(i, '\n')
        offset += 1
    return offset


def insert_attributes(xml, target_dir, let_name, camlp4):
    if not let_name:
        let_name = 'rfc'
    parent_map = {child:parent for parent in xml.iter() for child in parent}
    root = xml.getroot()
    if root.tag != NS + 'rfc':
        raise ParseException('Specification XML file should have <rfc> as root')
    rfc_num = root.attrib['number']

    files = {}
    #import pdb; pdb.set_trace()
    for elem in root.iter():
        if elem.tag == NS + 'coderef':
            path = elem.get('path')
            reftype = REFTYPES[elem.get('type')]
            if path not in files:
                file = SourceFile(path, reftype)
                files[path] = file
            else:
                file = files[path]
                assert file.reftype == reftype
            file.refs.append(Reference(int(elem.get('line')), get_target(elem, parent_map)))

    for rel_path, file in files.items():
        print(rel_path)
        path = os.path.join(target_dir, rel_path)
        with open(path, 'r') as f:
            lines = f.readlines()
        if camlp4:
            offset = 0
            # camlp4 mangles [@ref (rfc 9999) "xxx"] into [@ref rfc 999 "xxx"]
            #docref = '(rfc {0})'.format(rfc_num)
            docref = let_name
        else:
            header = [
                    '[@@@reftype {0}]\n'.format(file.reftype),
                    '[@@@specdoc let {0} = rfc {1}"]\n'.format(let_name, rfc_num),
                    ]
            offset = add_header(lines, header)
            docref = let_name
        file.insert_refs(lines, docref, offset)
        with open(path, 'w') as f:
            f.writelines(lines)


def main():
    import argparse
    parser = argparse.ArgumentParser(description='Read manually edited code references from an annotated IETF RFC in XML and add them to OCaml code as attributes')
    parser.add_argument('input', metavar='rfcNNNN_notes.xml', nargs=1, type=str,
            help='The path to the input XML document (.xml)')
    parser.add_argument('--let', dest='let', type=str,
            help='The name to bind with [@@@specdoc let name = rfc <number>]')
    parser.add_argument('--target', dest='target', type=str,
            help='The root directory containing the source code to be modified')
    parser.add_argument('--camlp4', action='store_true',
            help='Set this option to generate camlp4-compatible code')
    args = parser.parse_args()

    doc = etree.parse(args.input[0])

    insert_attributes(doc, args.target, args.let, args.camlp4)

if __name__ == '__main__':
    main()
