import cmd
import sys
import iota
from fetch import fetch
from find import find
from index import index

class IotaServer(cmd.Cmd):

    def _cookieify(self, sexp):
        cookie = '{}{:x}{}'.format(self._cookie_pre,
                                   len(sexp),
                                   self._cookie_post)
        return ''.join((cookie,sexp))


    def print_sexp(self,sexps):
        sys.stdout.write(self._cookieify('(:erase t)\n'))
        for sexp in sexps:
            sys.stdout.write(self._cookieify(sexp))


    def __init__(self, database, args):

        cmd.Cmd.__init__(self)

        self.prompt = ";; iota> "
        self.intro = ";; Welcome to iota {}".format(iota.VERSION)

        # cookies used in communication
        self._cookie_pre = '\xfe'
        self._cookie_post = '\xff'

        self.database = database
        self.args = args

    ## Command definitions ##
    def do_exit(self, args):
        """Exit iota

        iota> exit
        """
        return -1

    def do_EOF(self, args):
        return self.do_exit(args)

    def do_fetch(self, id):
        """Fetch a paper from ADS given the id

        iota> fetch id
        """
        self.args.id = id
        sexps = fetch(self.database, self.args)
        self.print_sexp(sexps)

    def do_find(self, query):
        """Find papers from ADS given the query

        iota> find query
        """
        self.args.query = query
        sexps = find(self.database, self.args)
        self.print_sexp(sexps)


    def do_index(self, args):
        """Index papers in iota paperdir

        iota> index
        """
        sexps = index(self.database, self.args)
        self.print_sexp(sexps)

def server(database, args):

    # get a console object and enter the main loop
    console = IotaServer(database, args)
    console.cmdloop()
