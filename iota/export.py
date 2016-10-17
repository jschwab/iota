import logging
import os
import os.path
from pybtex.database.input import bibtex
import xapian
import iota
import json
import datetime
import calendar
import operator
import string

from utils import with_previous

class ExportError(iota.IotaError):
    """Base class for exceptions in iota export module."""
    pass

EXPORT_SEXP = """(
    :count {count}
)
"""

M2N = dict([(n,i) for i,n in enumerate(calendar.month_name)])

class Paper:

    def __init__(self, data):

        self.data = json.loads(data)

        # extract first author (only A-Za-z)
        self.author = filter(unicode.isalpha, self.data['authors'][0].split(",")[0])

        # extract date
        self.year = int(self.data['year'])
        self.month = M2N[self.data['month']]

        self.disambiguation = None

    def bibentry(self):
        lines = []
        with open(os.path.join(self.data['path'], self.data['bibfile']), 'r') as f:
            pastat = False
            for line in f:
                if line.startswith('@'):
                    pastat = True
                    line = line.replace(str(self.data['bibkey']), self.citekey())
                if pastat:
                    lines.append(line)
        return ''.join(lines)

    def citekey(self):
        return "{}{}{}".format(self.author,str(self.year)[2:],self.disambiguation)

    def __repr__(self):
        return repr((self.author, self.year, self.month))

def export(database, args):
    """Export a paperdir as BibTeX"""

    # give a brief status update
    logging.info("Exporting paperdir: {}".format(args.paperdir))

    # get Xapian ready
    enquire = xapian.Enquire(database)
    enquire.set_query(xapian.Query.MatchAll)

    paper_count = database.get_doccount()
    matches = enquire.get_mset(0, paper_count)

    # loop over papers
    papers = sorted([Paper(match.document.get_data()) for match in matches],
                    key=operator.attrgetter('year', 'author', 'month'))

    # add in disambiguation
    ndupes = 0
    for previous_paper, paper in with_previous(papers):

        paper.disambiguation = ''

        if previous_paper is not None:

            if paper.author == previous_paper.author and paper.year == previous_paper.year:
                previous_paper.disambiguation = string.lowercase[ndupes]
                ndupes += 1
                paper.disambiguation = string.lowercase[ndupes]
            else:
                ndupes = 0

    if False:
        for paper in papers:
            print(paper.bibentry())
    else:
        with open('Papers.bib', 'w') as f:
            for paper in papers:

                # add bib entry
                f.write(paper.bibentry())

                # add symlink
                if paper.data['pdffile'] is not None:
                    p1 = os.path.join(paper.data['path'], paper.data['pdffile'])
                    p2 = "./pdfs/{}.pdf".format(paper.citekey())
                    os.symlink(p1, p2)

    return [EXPORT_SEXP.format(count=paper_count)]
