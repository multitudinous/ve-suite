"""Handles arguments sent to velauncher.

OBSOLETE."""
from velCoveredConfig import *

def Interpret(state, arguments):
    """Grab 1st argument. Assign it as either a VES or script file."""
    if not arguments:
        return
    arg = arguments[0]
    if arg[-4:] == '.ves':
        state.VesArgument(arg)
        print "VES Argument: %s" %arg ##TESTER
    else:
        state.ScriptArgument(arg)
        print "Script Argument: %s" %arg ##TESTER
    return
