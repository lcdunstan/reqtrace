#!/usr/bin/env python3

import os
import os.path
import re
import xml.etree.ElementTree as etree


NS = "{https://github.com/infidel/reqtrace}"

IMPORTANCES = [ 'must', 'should', 'may' ]
IMPORTANCE_HEADINGS = {
        'must': 'MUST, SHALL',
        'should': 'SHOULD, RECOMMENDED',
        'may': 'MAY, OPTIONAL',
        }


class ParseException(Exception):
    pass


def line_as_element(line):
    elem = etree.Element('span')
    elem.set('id', line.get('id'))
    elem.set('class', 'line')
    elem.text = line.text
    elem.tail = '\n'
    return elem


def note_as_element(note):
    elem = etree.Element('div')
    elem.set('class', 'note')
    elem.text = note.text
    elem.tail = '\n'
    return elem


def coderef_as_element(coderef):
    elem = etree.Element('div')
    elem.set('class', 'coderef')
    coderef_type = coderef.get('type', 'code')
    path = coderef.get('path', '')
    line = coderef.get('line')
    base = 'https://github.com/infidel/ocaml-dns/blob/master/'  # TODO
    if base:
        elem.text = coderef_type + ': '
        url = base + path
        a_text = path
        if line:
            url += '#l' + line
            a_text += ':' + line
        a = etree.Element('a', href=url)
        a.text = a_text
        elem.append(a)
    else:
        elem.text = coderef_type + ': ' + repo
    elem.tail = '\n'
    return elem


def ref_as_element(ref):
    #ref_type = coderef.get('type', '')
    target = ref.get('target', '???')
    elem = etree.Element('div')
    elem.set('class', 'ref')
    elem.text = 'See '
    url = '#{0}'.format(target)
    a = etree.Element('a', href=url)
    a.text = target
    elem.append(a)
    elem.tail = '\n'
    return elem


def notes_as_element(note, target_id, refs):
    elem = etree.Element('div')
    if target_id:
        elem.set('onmouseover', "addClass(document.getElementById('{0}'), 'hover')".format(target_id))
        elem.set('onmouseout', "removeClass(document.getElementById('{0}'), 'hover')".format(target_id))
    elem.set('class', 'notes')
    elem.text = '\n'
    for child in note:
        if child.tag == 'note':
            elem.append(note_as_element(child))
        elif child.tag == 'coderef':
            elem.append(coderef_as_element(child))
        elif child.tag == 'ref':
            elem.append(ref_as_element(child))
    if target_id in refs.references:
        for ref in refs.references[target_id]:
            elem.append(coderef_as_element(ref.as_xml()))
        del refs.references[target_id]
    elem.tail = '\n\n'
    return elem


def clause_as_element(clause, refs):
    elem = etree.Element('div')
    id = clause.get('id')
    if id:
        elem.set('id', id)
        anchor = etree.Element('a', name=id)
        elem.append(anchor)
    css_class = 'clause'
    importance = clause.get('importance')
    if importance:
        css_class += ' ' + importance
    elem.set('class', css_class)

    # For editing the XML it is nicer to have <notes> after
    # the text, but for the HTML it looks better before the text.
    for notes in clause.findall('notes'):
        elem.append(notes_as_element(notes, id, refs))
    if id in refs.references:
        #import pdb; pdb.set_trace()
        elem.insert(0, notes_as_element(etree.Element('notes'), id, refs))

    label = etree.Element('span')
    label.set('class', 'label')
    label.text = id
    elem.append(label)
    label.tail = ' '.join(sub.text for sub in clause.findall('linesub'))
    elem.tail = '\n'
    return elem


def paragraph_as_element(paragraph, section, refs):
    elem = etree.Element('div')
    elem.set('class', 'paragraph')
    elem.text = '\n'
    id = paragraph.get('id')
    if id:
        elem.set('id', id)
    if section.get('name') == 'Table of Contents':
        pre = etree.Element('pre')
        pre.text = '\n'.join(line.text for line in paragraph.findall('line'))
        elem.append(pre)
    else:
        for child in paragraph:
            if child.tag == 'clause':
                clause = clause_as_element(child, refs)
                elem.append(clause)
            elif child.tag == 'line':
                elem.append(line_as_element(child))
            elif child.tag == 'notes':
                div = notes_as_element(child, id, refs)
                if first_notes is None:
                    first_notes = child
                elem.insert(0, div)
        if id in refs.references:
            elem.insert(0, notes_as_element(etree.Element('notes'), id, refs))
    elem.tail = '\n\n'
    return elem


def section_as_elements(section, refs):
    h = etree.Element('h2')
    elements = [h]
    name = section.get('name')
    num = section.get('num')
    id = section.get('id')
    if id:
        h.set('id', id)
        anchor = etree.Element('a', name=id)
        elements.append(anchor)
    if num and name:
        h.text = num + ' ' + name
    elif name:
        h.text = name
    h.tail = '\n\n'
    for child in section:
        if child.tag == 'paragraph':
            elements.append(paragraph_as_element(child, section, refs))
        elif child.tag == 'notes':
            elements.insert(0, notes_as_element(child, id, refs))
    return elements


def table_of_clauses(clauses):
    table = etree.Element('table')
    table.set('class', 'index')
    thead = etree.SubElement(table, 'thead')
    thead.text = '\n'
    head_tr = etree.SubElement(thead, 'tr')
    for heading in ['Clause', 'Notes', 'Impl', 'Test', 'TODO']:
        th = etree.SubElement(head_tr, 'th')
        th.text = heading

    tbody = etree.SubElement(table, 'tbody')
    tbody.text = '\n'
    for clause in clauses:
        tr = etree.SubElement(tbody, 'tr')
        td_id = etree.SubElement(tr, 'td')
        id = clause.get('id')
        if id:
            a = etree.SubElement(td_id, 'a', href='#' + id)
            a.text = id
        else:
            td_id.text = id
        td_notes = etree.SubElement(tr, 'td')
        if clause.find('notes'):
            td_notes.text = 'Yes'
        td_impl = etree.SubElement(tr, 'td')
        impl = clause.findall(".//coderef[@type='impl']")
        if impl:
            td_impl.text = 'Yes'
        td_test = etree.SubElement(tr, 'td')
        test = clause.findall(".//coderef[@type='test']")
        if test:
            td_test.text = 'Yes'
        td_todo = etree.SubElement(tr, 'td')
        todo = clause.findall(".//note[@type='todo']")
        if todo:
            td_todo.text = 'Yes'
        tr.tail = '\n'
    table.tail = '\n\n'
    return table


def index_clauses(root):
    h1 = etree.Element('h1')
    h1.text = 'Index of Clauses'
    h1.tail = '\n\n'
    anchor = etree.Element('a', name='index_of_clauses')
    elements = [h1, anchor]
    for importance in IMPORTANCES:
        clauses = root.findall(".//clause[@importance='{0}']".format(importance))
        if clauses:
            h2 = etree.Element('h2')
            h2.text = IMPORTANCE_HEADINGS[importance]
            h2.tail = '\n\n'
            elements.append(h2)
            elements.append(table_of_clauses(clauses))
    return elements


def root_as_html(xml, refs):
    root = etree.Element('html',
            xmlns='http://www.w3.org/1999/xhtml')

    head = etree.Element('head')
    head.text = '\n'
    title = 'RFC {0}: {1}'.format(xml.attrib['number'], xml.attrib['title'])
    title_elem = etree.Element('title')
    title_elem.text = title
    title_elem.tail = '\n'
    style = etree.Element('link', rel='stylesheet', href='rfc_notes.css', type='text/css')
    style.tail = '\n'
    head.append(style)
    script = etree.Element('script', src='rfc_notes.js')
    script.text = ' '
    script.tail = '\n'
    head.append(script)
    head.append(title_elem)
    head.tail = '\n'
    root.append(head)

    body = etree.Element('body')
    body.text = '\n'
    
    p_links = etree.SubElement(body, 'p')
    p_links.text = 'Jump to: '
    etree.SubElement(p_links, 'a', href='#index_of_clauses').text = 'Index of Clauses'

    etree.SubElement(body, 'h1').text = title

    sections = xml.find('sections')
    for section in sections.findall('section'):
        body.extend(section_as_elements(section, refs))
    body.extend(index_clauses(xml))
    body.tail = '\n'
    root.append(body)

    return b'<!DOCTYPE html>\n' + etree.tostring(root)


class Reference:
    def __init__(self, id, filename, linenum):
        self.id = id
        self.filename = filename
        self.linenum = linenum

    def as_xml(self):
        type = 'impl'
        if self.filename.find('test') != -1:
            type = 'test'
        return etree.Element('coderef', type=type, path=self.filename, line=str(self.linenum))


class References:
    def __init__(self):
        self.references = {}

    def load(self, path):
        doc = etree.parse(path)
        root = doc.getroot()
        for reqref in root.findall(NS + 'reqref'):
            reqid = reqref.find(NS + 'reqid').text
            loc = reqref.find(NS + 'loc')
            ref = Reference(reqid, loc.get('filename'), int(loc.get('linenum')))
            l = self.references.setdefault(reqid, [])
            l.append(ref)


def main():
    import argparse
    parser = argparse.ArgumentParser(description='Convert an IETF RFC from annotated XML to XHTML')
    parser.add_argument('input', metavar='rfcNNNN_notes.xml', nargs=1, type=str,
            help='The path to the input XML document (.xml)')
    parser.add_argument('--html', dest='output_html', nargs=1, type=str,
            help='The path to an HTML output file')
    parser.add_argument('--ref', dest='ref', nargs='+', type=str,
            help='The path to one or more input XML files containing requirement references extracted from OCaml code')
    args = parser.parse_args()

    doc = etree.parse(args.input[0])

    refs = References()
    if args.ref:
        for ref_path in args.ref:
            if os.path.isdir(ref_path):
                for dirpath, dirnames, filenames in os.walk(ref_path):
                    for filename in filenames:
                        if filename.endswith('.req'):
                            refs.load(os.path.join(dirpath, filename))
            else:
                refs.load(ref_path)

    if args.output_html:
        html = root_as_html(doc.getroot(), refs)
        with open(args.output_html[0], 'wb') as f:
            f.write(html)

if __name__ == '__main__':
    main()
