import os
import tempfile
import xapian

from collections import namedtuple
from nose.tools import assert_raises

import index
import find

TEST_PAPERDIR = '/home/jschwab/Software/iota/tests/Papers'

def test_Find():
    "Check that the Index has the right number of results"

    # set up db
    dbpath = tempfile.mkdtemp()
    db = xapian.WritableDatabase(dbpath, xapian.DB_CREATE_OR_OPEN)

    # fake arguments
    Args = namedtuple('Args', ['paperdir'])
    args = Args(TEST_PAPERDIR)

    # do the indexing
    index.index(db, args)

    # do a query
    Args = namedtuple('Args', ['query'])
    args = Args("relativity")

    results = find.find(db, args)
    print(results)


if __name__ == '__main__':
    test_Find()
