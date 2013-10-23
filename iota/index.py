import os
import os.path
from pybtex.database.input import bibtex


def index_one(db, pdffile, bibdata):
    """Add the information from one paper to the Xapian index"""
    print("{} found at {}".format(bibdata.fields['title'], pdffile))


def index(db, args):
    """Index a paperdir"""

    print("index paperdir: {}".format(args.paperdir))

    for root, dirs, files in os.walk(args.paperdir):

        # it contains no sub-directories
        if len(dirs) != 0:
#            print("no subdirectories")
            continue

        # it must contain exactly one BibTex (.bib) file
        bibfiles = [f for f in files if os.path.splitext(f)[1] == ".bib"]
        if len(bibfiles) != 1:
#            print("no BibTeX file")
            continue

        # must contain one PDF (.pdf) file with the same basename as the BibTeX file
        pdffile = os.path.extsep.join((os.path.splitext(bibfiles[0])[0],"pdf"))
        if pdffile not in files:
 #           print("no PDF file")
            continue

        # that BibTeX file must contain exactly one bibliography entry
        parser = bibtex.Parser()
        bibdata = parser.parse_file(os.path.join(root, bibfiles[0]))
        if len(bibdata.entries) != 1:
#            print("more than one entry")
            continue

        bibkey = [x for x in bibdata.entries][0]
        index_one(db, os.path.join(root,pdffile), bibdata.entries[bibkey])
