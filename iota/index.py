import json
import logging
import os
import xapian

from pybtex.database.input import bibtex

import iota
import utils

class IndexError(iota.IotaError):
    """Base class for exceptions in iota index module."""

    def __init__(self, path, msg):
        self.path = path
        self.msg = msg


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
        data['bibkey'] = self.bibkey

        # the BibTeX entry must have a title
        try:
            title = self.bibdata.fields['title'].strip('{}')
        except KeyError:
            raise IndexError(self.path, 'BibTeX file does not contain a title')
        else:
            # don't allow the empty string
            if not title:
                raise IndexError(self.path, 'BibTeX file does not contain a title')
            data['title'] = title

        # the BibTeX entry must have authors
        try:
            authors = self.bibdata.persons['author']
        except KeyError:
            raise IndexError(self.path, 'BibTeX file does not contain authors')
        else:
            data['authors'] = [utils.sanitize_string(unicode(x),
                                                     exceptions=".,- ")
                               for x in authors]

        # the first author is special
        data['1au'] = utils.sanitize_string(data['authors'][0],
                                            exceptions=".,- ")

        # the journal
        try:
            journal = self.bibdata.fields['journal']
        except KeyError:
            journal = None
        data['journal'] = utils.canonical_journal(journal)

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

        # the month
        try:
            month = self.bibdata.fields['month']
        except KeyError:
            month = ''
        data['month'] = month

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

    for i, author in enumerate(data['authors']):
        termgenerator.index_text(author, 1, iota.TERMPREFIX_AUTHOR)
        # the first author is special
        if i == 0:
            doc.add_value(iota.SLOT_1AU, author)

    # these data may not exist
    if data['abstract'] is not None:
        termgenerator.index_text(data['abstract'], 1, iota.TERMPREFIX_ABSTRACT)

    if data['year'] is not None:
        termgenerator.index_text(data['year'], 1, iota.TERMPREFIX_YEAR)
        doc.add_value(iota.SLOT_YEAR, data['year'])

    if data['keywords'] is not None:
        termgenerator.index_text(data['keywords'], 1, iota.TERMPREFIX_KEYWORD)

    # Index title, authors & abstract fields for general search
    termgenerator.index_text(data['title'])
    for author in data['authors']:
        termgenerator.increase_termpos()
        termgenerator.index_text(author)
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

    return [utils.to_sexp({"count": paper_count})]
