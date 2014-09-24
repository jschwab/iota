import logging
import cmd
import sys
import iota
from fetch import fetch, FetchError
from find import find
from index import index
from view import view
from utils import parse_command


class IotaServer(cmd.Cmd):

    def _cookieify(self, sexp):
        cookie = '{}{:x}{}'.format(self._cookie_pre,
                                   len(sexp),
                                   self._cookie_post)
        return ''.join((cookie,sexp))

    def print_sexp(self, sexps, erase=True):
        sys.stdout.write("\n")
        if erase: sys.stdout.write(self._cookieify('(:erase t)\n'))
        for sexp in sexps:
            sys.stdout.write(self._cookieify(sexp))
        sys.stdout.flush()

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

    def do_quit(self, args):
        return self.do_exit(args)

    def do_EOF(self, args):
        return self.do_exit(args)

    def do_ping(self, args):
        """Say pong to a ping

        iota> ping
        """
        sexps = ('(:pong "iota" :props (:version "{}"))\n'.format(iota.VERSION),)
        self.print_sexp(sexps, erase=False)

    def do_fetch(self, id):
        """Fetch a paper from ADS given the id

        iota> fetch id
        """
        self.args.id = id
        try:
            sexps = fetch(self.database, self.args)
        except FetchError as e:
            logging.error(e.msg)
        else:
            self.print_sexp(sexps)

    def do_find(self, command):
        """Find papers from the paperdir

        iota> find query:"test" sortfield:year reverse:True maxnum:50
        """
        c = parse_command(command)
        try:
            sexps = find(self.database, **c)
        except TypeError as e:
            raise e
        else:
            self.print_sexp(sexps)


    def do_view(self, command):
        """View a paper from the paperdir given a docid

        iota> view docid:1
        """
        c = parse_command(command)
        try:
            sexps = view(self.database, **c)
        except TypeError:
            pass
        else:
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
