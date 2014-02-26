from itertools import tee, chain, izip
import string
import sexpdata


def with_previous(some_iterable):
    "Iterate with access to previous value"
    prevs, items = tee(some_iterable, 2)
    prevs = chain([None], prevs)
    return izip(prevs, items)


def sanitize_string(s, exceptions=[]):
    "Remove all non-alphabetic characters"
    return ''.join([c for c in s
                    if c in string.letters
                    or c in exceptions])


def parse_command(command):
    "This is awful and should be replaced with something better"
    d = {}
    for s in command.split(' '):
        k, v = s.strip().split(':')
        d[k] = eval(v)
    return d


def to_sexp(d):
    """Make a sexp string out of a python data structure"""
    return sexpdata.dumps(d).encode('ascii')
