import iota
import xapian

PAPER_SEXP = """(
    :docid {0}
)"""


def format_sexp(match):
    return PAPER_SEXP.format(match.docid)

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
    print("execute query: {}".format(args.query))
    for match in enquire.get_mset(0, 10):
        print(match.document)
        print(format_sexp(match))
