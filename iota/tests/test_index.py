import os
import tempfile
import xapian

from collections import namedtuple
from nose.tools import assert_raises

import index

TEST_PAPERDIR = '/home/jschwab/Software/iota/tests/Papers'


def test_ManyEntries():
    "BibTeX files with multiple entries are not allowed"

    root = os.path.join(TEST_PAPERDIR, 'ManyEntries')
    files = os.listdir(root)

    with assert_raises(index.IndexError) as cm:
        index.Paper(root, files)

    e = cm.exception
    assert e.msg == "more than one BibTeX entry in file"


def test_Empty():
    "Directories with no BibTeX files are not allowed"

    root = os.path.join(TEST_PAPERDIR, 'Empty')
    files = os.listdir(root)

    with assert_raises(index.IndexError) as cm:
        index.Paper(root, files)

    e = cm.exception
    assert e.msg == "no BibTeX file"


def test_ManyBibTeX():
    "Directories with multiple BibTeX files are not allowed"

    root = os.path.join(TEST_PAPERDIR, 'ManyBibTeX')
    files = os.listdir(root)

    with assert_raises(index.IndexError) as cm:
        index.Paper(root, files)

    e = cm.exception
    assert e.msg == "more than one BibTeX file"


def test_NoTitle():
    "BibTeX files without titles are not allowed"

    root = os.path.join(TEST_PAPERDIR, 'NoTitle')
    files = os.listdir(root)

    paper = index.Paper(root, files)
    with assert_raises(index.IndexError) as cm:
        paper.data()

    e = cm.exception
    assert e.msg == "BibTeX file does not contain a title"


def test_NoAuthor():
    "BibTeX files without authors are not allowed"

    root = os.path.join(TEST_PAPERDIR, 'NoAuthor')
    files = os.listdir(root)

    paper = index.Paper(root, files)
    with assert_raises(index.IndexError) as cm:
        paper.data()

    e = cm.exception
    assert e.msg == "BibTeX file does not contain authors"


def test_Index():
    "Check that the Index has the right number of results"

    # set up db
    dbpath = tempfile.mkdtemp()
    db = xapian.WritableDatabase(dbpath, xapian.DB_CREATE_OR_OPEN)

    # fake arguments
    Args = namedtuple('Args', ['paperdir'])
    args = Args(TEST_PAPERDIR)

    result = index.index(db, args)
    assert result['count'] == 7

if __name__ == '__main__':
    pass
