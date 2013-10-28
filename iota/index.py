import os
import os.path
from pybtex.database.input import bibtex
import xapian
import iota

class Error(Exception):
    """Base class for exceptions in this module."""
    pass

class PaperError(Error):
    pass

class PaperIndexError(Error):
    pass


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
            raise PaperError(self.path, "no BibTeX file")
        else:
            raise PaperError(self.path, "more than one BibTeX file")

        # that BibTeX file must contain exactly one bibliography entry
        parser = bibtex.Parser()
        bibdata = parser.parse_file(os.path.join(self.path, self.bibfile))
        if len(bibdata.entries) == 1:
            self.bibkey = [x for x in bibdata.entries][0]
            self.bibdata = bibdata.entries[self.bibkey]
        else:
            raise PaperError(self.bibfile, "more than one BibTeX entry in file")

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


def index_paper(database, paper):
    """Add the information from a paper to the Xapian index"""

    # Set up a TermGenerator that we'll use in indexing.
    termgenerator = xapian.TermGenerator()
    termgenerator.set_stemmer(xapian.Stem("en"))

    # We make a document and tell the term generator to use this.
    doc = xapian.Document()
    termgenerator.set_document(doc)

    # the BibTeX entry must have a title
    try:
        title = paper.bibdata.fields['title']
    except KeyError:
        raise PaperIndexError('BibTeX file does not contain a title')
    else:
        termgenerator.index_text(title, 1, iota.TERMPREFIX_TITLE)

    # the BibTeX entry must have authors
    try:
        authors = paper.bibdata.persons['author']
    except KeyError:
        raise PaperIndexError('BibTeX file does not contain authors')
    else:
        for author in authors:
            termgenerator.index_text(unicode(author), 1, iota.TERMPREFIX_AUTHOR)

    # it is ok if the following information is unset

    # the abstract
    try:
        abstract = paper.bibdata.fields['abstract']
    except KeyError:
        abstract = None
    else:
        # use a custom prefix "XA" for abstract
        termgenerator.index_text(abstract, 1, iota.TERMPREFIX_ABSTRACT)

    # the year
    try:
        year = paper.bibdata.fields['year']
    except KeyError:
        pass
    else:
        termgenerator.index_text(year, 1, iota.TERMPREFIX_YEAR)

    # the keywords
    try:
        keywords = paper.bibdata.fields['keywords']
    except KeyError:
        pass
    else:
        termgenerator.index_text(keywords, 1, iota.TERMPREFIX_KEYWORD)

    # Index title & abstract fields for general search
    termgenerator.index_text(title)
    if abstract is not None:
        termgenerator.increase_termpos()
        termgenerator.index_text(abstract)

    # We use the bibkey to ensure each object ends up in the database
    # only once no matter how many times we run the indexer.
    idterm = u"Q" + paper.bibkey
    doc.add_boolean_term(idterm)
    database.replace_document(idterm, doc)


def index(database, args):
    """Index a paperdir"""

    # give a brief status update
    print("indexing paperdir: {}".format(args.paperdir))

    # get Xapian ready
    # TODO

    for root, dirs, files in os.walk(args.paperdir):

        # it contains no sub-directories
        if len(dirs) != 0:
            print("skipping {}: no subdirectories".format(root))
            continue

        # convert directory into paper object
        try:
            paper = Paper(root, files)
        except PaperError as e:
            print(e)
            continue

        # add paper object to Xapian database
        try:
            index_paper(database, paper)
        except PaperIndexError as e:
            print(e)
            continue

        print("Added {}".format(root))
