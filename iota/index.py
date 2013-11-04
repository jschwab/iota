import logging
import os
import os.path
from pybtex.database.input import bibtex
import xapian
import iota
import json
import sys

class IndexError(iota.IotaError):
    """Base class for exceptions in iota index module."""
    pass

INDEX_SEXP = """(
    :count {count}
)
"""
class Paper:
    """A directory which represents a paper"""

    def __init__(self, root, files):

        # main directory path
        self.path = root

        # it must contain exactly one BibTex (.bib) file
        bibfiles = [f for f in files if os.path.splitext(f)[1] == ".bib"]
        if len(bibfiles) == 1:
            self.bibfile = bibfiles[0]
        elif len(bibfiles) == 0:
            raise IndexError(self.path, "no BibTeX file")
        else:
            raise IndexError(self.path, "more than one BibTeX file")

        # that BibTeX file must contain exactly one bibliography entry
        parser = bibtex.Parser()
        bibdata = parser.parse_file(os.path.join(self.path, self.bibfile))
        if len(bibdata.entries) == 1:
            self.bibkey = [x for x in bibdata.entries][0]
            self.bibdata = bibdata.entries[self.bibkey]
        else:
            raise IndexError(self.bibfile, "more than one BibTeX entry in file")

        # usually, it will contain one PDF (.pdf) file with the same
        # basename as the BibTeX file

        pdffile = os.path.extsep.join((os.path.splitext(bibfiles[0])[0],"pdf"))
        if pdffile in files:
            self.pdffile = pdffile
        else:
            self.pdffile = None

        # keep a list of other files around
        files.remove(self.bibfile)
        if self.pdffile is not None:
            files.remove(self.pdffile)
        self.otherfiles = files

    def data(self):
        data = {}

        # paths and file data
        data['path'] = self.path
        data['pdffile'] = self.pdffile
        data['bibfile'] = self.bibfile
        data['otherfiles'] = self.otherfiles

        # information about the paper itself

        # the BibTeX entry must have a title
        try:
            title = self.bibdata.fields['title'].strip('{}')
        except KeyError:
            raise IndexError('BibTeX file does not contain a title')
        else:
            data['title'] = title

        # the BibTeX entry must have authors
        try:
            authors = self.bibdata.persons['author']
        except KeyError:
            raise IndexError('BibTeX file does not contain authors')
        else:
            data['authors'] = [unicode(x) for x in authors]

        # the abstract
        try:
            abstract = self.bibdata.fields['abstract']
        except KeyError:
            abstract = None
        data['abstract'] = abstract

        # the year
        try:
            year = self.bibdata.fields['year']
        except KeyError:
            year = None
        data['year'] = year

        # the keywords
        try:
            keywords = self.bibdata.fields['keywords']
        except KeyError:
            keywords = None
        data['keywords'] = keywords

        return data


def index_paper(database, paper):
    """Add the information from a paper to the Xapian index"""

    # Set up a TermGenerator that we'll use in indexing.
    termgenerator = xapian.TermGenerator()
    termgenerator.set_stemmer(xapian.Stem("en"))

    # We make a document and tell the term generator to use this.
    doc = xapian.Document()
    termgenerator.set_document(doc)

    data = paper.data()
    doc.set_data(json.dumps(data, encoding='utf8'))

    # these data must exist
    termgenerator.index_text(data['title'], 1, iota.TERMPREFIX_TITLE)

    for author in data['authors']:
        termgenerator.index_text(author, 1, iota.TERMPREFIX_AUTHOR)

    # these data may not exist
    if data['abstract'] is not None:
        termgenerator.index_text(data['abstract'], 1, iota.TERMPREFIX_ABSTRACT)

    if data['year'] is not None:
        termgenerator.index_text(data['year'], 1, iota.TERMPREFIX_YEAR)

    if data['keywords'] is not None:
        termgenerator.index_text(data['keywords'], 1, iota.TERMPREFIX_KEYWORD)

    # Index title & abstract fields for general search
    termgenerator.index_text(data['title'])
    if data['abstract'] is not None:
        termgenerator.increase_termpos()
        termgenerator.index_text(data['abstract'])

    # We use the bibkey to ensure each object ends up in the database
    # only once no matter how many times we run the indexer.
    idterm = u"Q" + paper.bibkey
    doc.add_boolean_term(idterm)
    database.replace_document(idterm, doc)

def index(database, args):
    """Index a paperdir"""

    # give a brief status update
    logging.info("Indexing paperdir: {}".format(args.paperdir))

    # get Xapian ready
    # TODO

    paper_count = 0

    for root, dirs, files in os.walk(args.paperdir):

        # it contains no sub-directories
        if len(dirs) != 0:
            logging.debug("Skipping {}: has subdirectories".format(root))
            continue

        # convert directory into paper object
        try:
            paper = Paper(root, files)
        except IndexError as e:
            logging.debug(e)
            continue

        # add paper object to Xapian database
        try:
            index_paper(database, paper)
        except IndexError as e:
            logging.debug(e)
            continue

        paper_count += 1
        logging.info("Added {}".format(root))

    return [INDEX_SEXP.format(count=paper_count)]
