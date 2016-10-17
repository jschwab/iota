from fetch import fetch, FetchError
import collections
import time
import os
import sys

def main(ids):

    Args = collections.namedtuple('Args', ['id', 'paperdir'])
    paperdir = os.path.expanduser("~/Papers")

    with open('good.txt', 'w') as g, open('bad.txt', 'w') as b:

        for id in ids:
            args = Args(id, paperdir)
            try:
                fetch(None, args)
            except FetchError as e:
                f = b
                print(e.msg)
                print("failed", id)
            else:
                f = g
                print("succeded", id)

            f.write(id)
            f.write('\n')

            time.sleep(30)

    return

if __name__ == '__main__':

    with open(sys.argv[1], 'r') as f:
        ids = [line.strip() for line in f.readlines()]

    main(ids)
