"""Contains functions for starting up the shell in VE-Launcher."""
from subprocess import Popen, PIPE
from os import execl

from velBase import *
from velCoveredConfig import *

def Start(shellScript = None):
    if windows:
        if shellScript:
            Popen([shellScript])
        else:
            os.system('start "%s" cmd' %LAUNCHER_SHELL_NAME)
    elif unix:
        if shellScript:
            print "VE-Suite script started."
            execl(shellScript, "")
        else:
            print "VE-Suite subshell started."
            print "Type exit to return to your previous" + \
                  " shell once you're done."
            execl(UNIX_SHELL, "")
    else:
        print "SHELL ERROR! This OS isn't supported."
    return
