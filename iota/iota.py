#!/usr/bin/env python2

import argparse
import logging
import os.path
import xapian

VERSION = "(prerelease)"

# define a set of term prefixes for Xapian
# begin with a subset of the ones from
# http://xapian.org/docs/omega/termprefixes

TERMPREFIX_AUTHOR = 'A'
TERMPREFIX_DATE = 'D'
TERMPREFIX_KEYWORD = 'K'
TERMPREFIX_MONTH = 'M'
TERMPREFIX_PATHNAME = 'P'
TERMPREFIX_UNIQUEID = 'Q'
TERMPREFIX_TITLE = 'S'
TERMPREFIX_YEAR = 'Y'

# and extend via X*
TERMPREFIX_ABSTRACT = 'XA'
TERMPREFIX_JOURNAL = 'XJ'

TERMPREFIXES = (
    ('author', TERMPREFIX_AUTHOR),
    ('keyword', TERMPREFIX_KEYWORD),
    ('year', TERMPREFIX_YEAR),
    ('abstract', TERMPREFIX_ABSTRACT),
)

SLOT_YEAR = 0
SLOT_1AU = 1

SLOTS = {
    'year': SLOT_YEAR,
    '1au': SLOT_1AU,
}


class IotaError(Exception):
    """Base class for exceptions in Iota."""
    pass

if __name__ == '__main__':

    from export import export
    from fetch import fetch
    from find import find
    from index import index
    from server import server

    # TODO: read in the default config

    dbfilename = os.path.expanduser("~/.iota/xapian")
    rcfilename = os.path.expanduser("~/.iotarc")
    paperdir = os.path.expanduser("~/Papers")
    logfilename = os.path.expanduser("~/.iota/log/iota.log")

    # parse the options
    parser = argparse.ArgumentParser(description='Tools to work with paperdirs')
    parser.add_argument('-p', '--paperdir', metavar='<paperdir>',
                        default=paperdir,
                        help='top of the paperdir')
    parser.add_argument('-x', '--xapiandb', metavar='<xapiandb>',
                        default=dbfilename,
                        help='location of xapian database')
    parser.add_argument('-l', '--logfile', metavar='<logfile>',
                        default=logfilename,
                        help='location of log file')
    parser.add_argument('-d', '--debug', action='store_true',
                        help='generate extra debug information')

    subparsers = parser.add_subparsers(title='subcommands',
                                       help='valid subcommands')

    # create the parser for the "index" command
    parser_index = subparsers.add_parser('index',help='index the articles in a paperdir')
    parser_index.set_defaults(func=index)

    # create the parser for the "find" command
    parser_find = subparsers.add_parser('find', help='find articles in a paperdir')
    parser_find.add_argument('query', help='search query')
    parser_find.set_defaults(func=find)

    # create the parser for the "server" command
    parser_find = subparsers.add_parser('server', help='backend for iota4e')
    parser_find.set_defaults(func=server)

    # create the parser for the "fetch" command
    parser_fetch = subparsers.add_parser('fetch', help='fetch paper from ADS')
    parser_fetch.add_argument('id', metavar='id',
                              help='ADS/arXiv id for the paper')
    parser_fetch.set_defaults(func=fetch)

    # create the parser for the "export" command
    parser_find = subparsers.add_parser('export', help='export paperdir to BibTeX')
    parser_find.set_defaults(func=export)

    args = parser.parse_args()

    # fire up the logger
    logging.basicConfig(filename=logfilename, level=logging.DEBUG)

    logging.debug('Started iota')

    # open up a Xapian connection
    db = xapian.WritableDatabase(args.xapiandb, xapian.DB_CREATE_OR_OPEN)

    args.func(db, args)

    logging.debug('Finished iota')
