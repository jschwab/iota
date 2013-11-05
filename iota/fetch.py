import StringIO
import webbrowser
import logging
import os
import re
import sys
import urllib2
from pybtex.database.input import bibtex


ID_BIBCODE = re.compile("^(\d{4})(.{14})([A-Z])$")
ID_arXiv = re.compile("^\d{4}.\d{4}$")

# TODO: convert all of this to urlparse

ADS_BIBTEX_URL = "http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode={}&data_type=BIBTEXPLUS&db_key=AST&nocookieset=1"

ADS_PAPER_URL = "http://adsabs.harvard.edu/cgi-bin/nph-data_query?bibcode={}&link_type=ARTICLE&db_key=AST&high="
ARXIV_PAPER_URL = "http://arxiv.org/pdf/{}.pdf"

EXTERNAL_PAPER_URL = "http://adsabs.harvard.edu/cgi-bin/nph-data_query?bibcode={}&link_type=EJOURNAL&db_key=AST&db_key=PHY&high="

FETCH_SEXP = """(
    :path {path}
    :bibfile {bibfile}
    :pdffile {pdffile}
)
"""


def arXiv2ADS(arxiv_id):
    request = urllib2.urlopen("http://adsabs.harvard.edu/cgi-bin/bib_query?arXiv:{}".format(arxiv_id))
    pagetext = request.read()
    return re.search(b"\d{4}.{14}[A-Z]",pagetext).group(0).decode("utf-8")

class ADSPaper:

    def __init__(self, id):

        self.id = id
        logging.info("Preparing to fetch %s", id)

        if ID_BIBCODE.match(id):
            self.bibcode = id
            self._paper_url = ADS_PAPER_URL
            logging.info("%s is a valid ADS id", id)
        elif ID_arXiv.match(id):
            self.bibcode = arXiv2ADS(id)
            self._paper_url = ARXIV_PAPER_URL
            logging.info("%s is a valid arXiv id", id)
        else:
            pass
            # raise ADSError("Not a valid bibcode")

        # prepare to retrieve BibTeX information
        self._bibtex = None
        self._bibtex_url = ADS_BIBTEX_URL

        # parse the bibcode
        m = ID_BIBCODE.match(self.bibcode)
        self.publication_year = m.group(1)
        self.unique_id = m.group(2).replace(".","_").replace("&","+")
        self.last_initial = m.group(3)

        self.get_bibtex()

    def get_bibtex(self):
        """Retrive the BibTeX entry for this paper"""
        logging.debug("Fetching BibTex file")
        self._bibtex = urllib2.urlopen(self._bibtex_url.format(self.bibcode)).read()

        parser = bibtex.Parser()
        bd = parser.parse_stream(StringIO.StringIO(self._bibtex.decode('utf-8')))
        bibkey = [x for x in bd.entries][0]
        self.bibdata = bd.entries[bibkey]

    def save_bibtex(self, filename):
        """Retrive and save the BibTeX entry for this paper"""
        if self._bibtex is None:
            self.get_bibtex()
        with open(filename, "wb") as f:
            f.write(self._bibtex)
        return

    def pdf_url(self):
        return self._paper_url.format(self.id)

    def save_pdf(self, filename):
        """Retrieve and save a PDF for this paper"""
        request = urllib2.urlopen(self.pdf_url())
        with open(filename, "wb") as f:
            f.write(request.read())
        return

    def title(self):
        """Retreive title of paper"""
        return self.bibdata.fields['title']

    def name(self):
        """Determine the name by which the file will be referred to"""
        authors = self.bibdata.persons['author']
        first_author = authors[0].last()[0].strip('{}')
        year = self.bibdata.fields['year']
        if len(authors) > 1:
            return "{}_et_al_{}".format(first_author, year)
        else:
            return "{}_{}".format(first_author, year)


def fetch(database, args):
    """Fetch a paper from ADS.

    TODO: Also update the index when this happens."""

    paper = ADSPaper(args.id)

    logging.info("Retreived paper %s", paper.name())

    # the folder hierarchy is based on year and last initial of first author

    # make year directory; ok if already exists
    yeardir = os.path.join(args.paperdir, paper.publication_year)
    try:
        os.mkdir(yeardir)
    except OSError:
        pass

    # make first initial directory; ok if already exists
    initialdir = os.path.join(yeardir, paper.last_initial)
    try:
        os.mkdir(initialdir)
    except OSError:
        pass

    # make paper directory; not ok if it already exists
    uniquedir = os.path.join(initialdir, paper.unique_id)
    try:
        os.mkdir(uniquedir)
    except OSError:
        logging.warning("This paper is already present in your library")
        return
    else:
        logging.debug("Created directory %s", uniquedir)

    # save the BibTeX file
    bibfilename = "{}.bib".format(paper.name())
    paper.save_bibtex(os.path.join(uniquedir, bibfilename))
    logging.info("Saved BibTeX file %s", bibfilename)

    # download a PDF
    pdffilename = "{}.pdf".format(paper.name())
    try:
        paper.save_pdf(os.path.join(uniquedir, pdffilename))
    except urllib2.HTTPError:
        pdffilename = 'nil'
        webbrowser.open(EXTERNAL_PAPER_URL.format(paper.bibcode))
    else:
        logging.info("Saved PDF file %s", pdffile)

    # TODO: check that you actually got a PDF!

    sexp = FETCH_SEXP.format(path = uniquedir,
                             bibfile = bibfilename,
                             pdffile = pdffilename)

    return [sexp]
