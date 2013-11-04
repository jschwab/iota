import iota
import json
import logging
import xapian
import sys

PAPER_SEXP = """(
    :docid {docid}
    :title {title}
    :path {path}
)
"""

def match_as_sexp(match):
    data = json.loads(match.document.get_data())
    sexp = PAPER_SEXP.format(docid=match.docid,
                             title='"{}"'.format(data['title']),
                             authors=data['authors'],
                             path=data['path'])
    return sexp

def result(mset):
    sexps = [match_as_sexp(match) for match in mset]
    sexps.append('(:count {})\n'.format(len(mset)))
    return sexps

def find(database, args):

    # Set up a QueryParser with a stemmer and suitable prefixes
    queryparser = xapian.QueryParser()
    queryparser.set_stemmer(xapian.Stem("en"))
    queryparser.set_stemming_strategy(queryparser.STEM_SOME)

    # Add in the termprefixes that are in use
    for tp in iota.TERMPREFIXES:
        queryparser.add_prefix(*tp)

    # And parse the query
    query = queryparser.parse_query(args.query)

    # Use an Enquire object on the database to run the query
    enquire = xapian.Enquire(database)
    enquire.set_query(query)

    # And print out something about each match
    logging.info("execute query: {}".format(args.query))
    mset = enquire.get_mset(0, 100)

    return result(mset)
