import cmd
import iota

class IotaServer(cmd.Cmd):

    def __init__(self, args):
        cmd.Cmd.__init__(self)
        self.prompt = ";; iota> "
        self.intro = ";; Welcome to iota {}".format(iota.VERSION)
        self.args = args

def server(database, args):

    # get a console object and enter the main loop
    console = IotaServer(args)
    console.cmdloop()
