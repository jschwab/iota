#!/usr/bin/env python2

import argparse
import xapian

VERSION="(prerelease)"

# define a set of term prefixes for Xapian
# begin with a subset of the ones from
# http://xapian.org/docs/omega/termprefixes

TERMPREFIX_AUTHOR='A'
TERMPREFIX_DATE='D'
TERMPREFIX_KEYWORD='K'
TERMPREFIX_MONTH='M'
TERMPREFIX_PATHNAME='P'
TERMPREFIX_UNIQUEID='Q'
TERMPREFIX_TITLE='S'
TERMPREFIX_YEAR='Y'

# and extend via X*
TERMPREFIX_ABSTRACT='XA'
TERMPREFIX_JOURNAL='XJ'

TERMPREFIXES = (
    ('author',TERMPREFIX_AUTHOR),
    ('keyword', TERMPREFIX_KEYWORD),
    ('year', TERMPREFIX_YEAR),
    ('abstract', TERMPREFIX_ABSTRACT),
)

if __name__ == '__main__':

    from find import find
    from index import index
    from server import server

    # read in the default config
    # TODO
    dbfilename = "/tmp/iota.db" # default will be ~/.iota/xapian

    # parse the options
    parser = argparse.ArgumentParser(description='Tools to work with a paperdir')
    subparsers = parser.add_subparsers(title='subcommands',
                                       help='valid subcommands')

    # create the parser for the "index" command
    parser_index = subparsers.add_parser('index', help='index the articles in a paperdir')
    parser_index.add_argument('-p', '--paperdir', metavar='<paperdir>',
                              help='top of the paperdir')
    parser_index.set_defaults(func=index)

    # create the parser for the "find" command
    parser_find = subparsers.add_parser('find', help='find articles in a paperdir')
    parser_find.add_argument('query', help='search query')
    parser_find.set_defaults(func=find)

    # create the parser for the "server" command
    parser_find = subparsers.add_parser('server', help='backend for iota4e')
    parser_find.set_defaults(func=server)

    args = parser.parse_args()

    # open up a Xapian connection
    db = xapian.WritableDatabase(dbfilename, xapian.DB_CREATE_OR_OPEN)

    args.func(db, args)
