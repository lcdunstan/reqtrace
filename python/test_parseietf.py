#!/usr/bin/env python3

import unittest
import parseietf


class TestParse(unittest.TestCase):
    def old_test_rfc2671(self):
        doc = parseietf.parse_path('rfc2671.txt')
        self.assertEqual(2671, doc.rfc_number)
        self.assertEqual('Standards Track', doc.category)
        self.assertEqual('Extension Mechanisms for DNS (EDNS0)', doc.title)
        self.assertEqual(395, len(doc.lines))
        self.assertEqual(7, doc.lines[-1].page)

        section = doc.sections[0]
        self.assertEqual('Status of this Memo', section.heading)
        self.assertEqual('', section.num)
        self.assertEqual('Status of this Memo', section.name)
        self.assertEqual(1, len(section.paragraphs))
        self.assertEqual('This document specifies an Internet standards track protocol for the Internet community, and requests discussion and suggestions for improvements.  Please refer to the current edition of the "Internet Official Protocol Standards" (STD 1) for the standardization state and status of this protocol.  Distribution of this memo is unlimited.',
                section.paragraphs[0].text)

        section = doc.sections[3]
        self.assertEqual('1 - Rationale and Scope', section.heading)
        self.assertEqual('1', section.num)
        self.assertEqual('Rationale and Scope', section.name)

    def test_rfc6762(self):
        doc = parseietf.parse_path('rfc6762.txt')
        self.assertEqual(6762, doc.rfc_number)
        self.assertEqual('Standards Track', doc.category)
        self.assertEqual('Multicast DNS', doc.title)
        self.assertEqual(3923, len(doc.lines))
        self.assertEqual(70, doc.lines[-1].page)

        section = doc.sections[0]
        self.assertEqual('Abstract', section.heading)
        self.assertEqual(3, len(section.paragraphs))
        self.assertEqual('As networked devices become smaller, more portable, and more ubiquitous, the ability to operate with less configured infrastructure is increasingly important.  In particular, the ability to look up DNS resource record data types (including, but not limited to, host names) in the absence of a conventional managed DNS server is useful.',
                section.paragraphs[0].text)

        section = doc.sections[4]
        self.assertEqual('1.  Introduction', section.heading)
        self.assertEqual('1', section.num)
        self.assertEqual('Introduction', section.name)
        self.assertEqual(2, len(section.paragraphs))

        section = doc.sections[6]
        self.assertEqual('3', section.num)
        self.assertEqual('Multicast DNS Names', section.name)
        paragraph = section.paragraphs[3]
        self.assertEqual(4, paragraph.num)

        clause = paragraph.clauses[0]
        self.assertEqual(1, clause.num)
        self.assertEqual('Any DNS query for a name ending with ".local." MUST be sent to the mDNS IPv4 link-local multicast address 224.0.0.251 (or its IPv6 equivalent FF02::FB).', clause.text)

        clause = paragraph.clauses[1]
        self.assertEqual(2, clause.num)
        self.assertEqual('The design rationale for using a fixed multicast address instead of selecting from a range of multicast addresses using a hash function is discussed in Appendix B.', clause.text)

        section = doc.sections[14]
        self.assertEqual('6', section.num)
        paragraph = section.paragraphs[13]
        self.assertEqual('Other benefits of sending responses via multicast are discussed in Appendix D.', paragraph.clauses[1].text)
        self.assertEqual('A Multicast DNS querier MUST only accept unicast responses if they answer a recently sent query (e.g., sent within the last two seconds) that explicitly requested unicast responses.', paragraph.clauses[2].text)


def main():
    unittest.main()

if __name__ == '__main__':
    main()
