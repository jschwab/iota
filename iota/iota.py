#!/usr/bin/env python2

import argparse
from index import index
from find import find

if __name__ == '__main__':

    # read in the default config
    # TODO

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

    args = parser.parse_args()

    # open up a Xapian connection
    db = None
    args.func(db, args)
