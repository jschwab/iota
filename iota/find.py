import iota
import json
import logging
import xapian
import sys
import sexpdata

from utils import to_sexp


def match_as_sexp(match):
    data = json.loads(match.document.get_data())
    s = {'header': True}
    s['docid'] = int(match.docid)
    for k in ('title', '1au', 'journal'):
        s[k] = data[k]
    s['year'] = int(data['year'])
    sexp = to_sexp(s)
    return sexp


def find(database, query, sortfield=None, reverse=False, maxnum=100):
    """sort_direction is False for ascending order, True for descending order.
    """

    # Set up a QueryParser with a stemmer and suitable prefixes
    queryparser = xapian.QueryParser()
    queryparser.set_stemmer(xapian.Stem("en"))
    queryparser.set_stemming_strategy(queryparser.STEM_SOME)

    # Add in the termprefixes that are in use
    for tp in iota.TERMPREFIXES:
        queryparser.add_prefix(*tp)

    # And parse the query
    xquery = queryparser.parse_query(query)

    # Use an Enquire object on the database to run the query
    enquire = xapian.Enquire(database)
    enquire.set_query(xquery)

    # Determine how we're going to sort the results.
    if sortfield is not None:
        try:
            slot = iota.SLOTS[sortfield]
        except KeyError:
            pass
        else:
            enquire.set_sort_by_value_then_relevance(slot, not reverse)

    # And print out something about each match
    logging.info("execute query: {}".format(query))
    mset = enquire.get_mset(0, maxnum)

    # make sexps
    sexps = [match_as_sexp(match) for match in mset]
    sexps.append('(:found {})\n'.format(len(mset)))

    return sexps
