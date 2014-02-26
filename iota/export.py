import logging
import os
import os.path
from pybtex.database.input import bibtex
import xapian
import iota
import json
import os
import datetime
from pybtex.database.input import bibtex
import calendar
import operator
from itertools import tee, islice, chain, izip
import string

def with_previous(some_iterable):
    "Iterate with access to previous value"
    prevs, items = tee(some_iterable, 2)
    prevs = chain([None], prevs)
    return izip(prevs, items)

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
                    key=operator.attrgetter('year', 'month', 'author'))

    # add in disambiguation
    for previous_paper, paper in with_previous(papers):

        paper.disambiguation = ''

        if previous_paper is not None:

            if paper.author == previous_paper.author and paper.year == previous_paper.year:
                previous_paper.disambiguation = string.lowercase[ndupes]
                ndupes += 1
                paper.disambiguation = string.lowercase[ndupes]
            else:
                ndupes = 0

    # construct final biblography
    for paper in papers:
        print(paper.bibentry())

    return [EXPORT_SEXP.format(count=paper_count)]
