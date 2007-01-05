"""Handles immediate startups from VE-Launcher's command line."""
import wx
import os
from velBase import *
from velLaunchCode import *
from velModes import *
from velCoveredConfig import *
from velServerKillWindow import *
from velJconfDict import *
from velClusterDict import *
from velConfigFunctions import *
import velShell
import velArguments

class CommandLine:
    """Launches VE Suite using arguments from the command line."""
    def __init__(self, opts, arguments):
        ##Set up default vars.
        self.state = CoveredConfig()
        LoadConfig(DEFAULT_CONFIG, self.state)
        self.state.CommandLineMode()
        if ("--dev", "") in opts:
            devMode = True
            self.state.DevMode()
        else:
            devMode = False
        ##Set vars from the command line.
        for opt, arg in opts:
            if opt in ('-c', "--conductor"):
                self.state.Edit("Conductor", True)
            elif opt in ('-n', "--nameserver"):
                self.state.Edit("NameServer", True)
            elif opt in ('-x', "--xplorer="):
                self.state.Edit("Xplorer", True)
                if arg in XPLORER_TYPE_LIST:
                    self.state.Edit("XplorerType", XPLORER_TYPE_LIST.index(arg))
                else:
                    self.state.Edit("XplorerType", 0)
            elif opt in ('-k', "--desktop"):
                self.state.Edit("DesktopMode", True)
            elif opt in ('-j', "--jconf="):
                self.state.Edit("JconfDict", {"Default": arg})
            elif opt in ('-t', "--taomachine="):
                self.state.Edit("TaoMachine", arg)
            elif opt in ('-p', "--port="):
                self.state.Edit("TaoPort", arg)
            elif opt in ('-w', "--dir="):
                self.state.Edit("Directory", arg)
            elif opt in ('-e', "--dep="):
                self.state.Edit("DependenciesDir", arg)
            elif opt in ('-m', "--master="):
                self.state.Edit("ClusterMaster", arg)
            elif opt in ('-s', "--shell"):
                self.state.Edit("Shell", True)
                self.state.Cover("Conductor", False)
                self.state.Cover("Xplorer", False)
                self.state.Cover("NameServer", False)
            elif opt in ('-b', "--builder="):
                self.state.Edit("Shell", True)
                self.state.Edit("BuilderDir", arg)
                self.state.Cover("Conductor", False)
                self.state.Cover("Xplorer", False)
                self.state.Cover("NameServer", False)
            elif opt in ('-f', "--file"):
                self.state.InterpretArgument(arg)
            elif opt in ('-v', "--ves="):
                self.state.VesArgument(arg)
            ## NOTE:
            ## ('-q', '--quick'): Quick Mode
            ## Tells VE-Launcher to immediately start up with
            ## previous settings. Doesn't change anything by
            ## itself.

            ##NOTE: --setup will be used to set up working directories &
            ##dependencies folders without going into the GUI.
            ##Not implemented yet.
            ##elif opt in ('-s', "--setup"):
            ##    print "ERROR: Setup isn't implemented yet." + \
            ##          " Wait until next version."

        ##Launch
        launchInstance = Launch(self.state.GetLaunchSurface())
        ##Show NameServer kill window if NameServer was started.
        if self.state.GetSurface("NameServer"):
            app = wx.PySimpleApp()
            window = ServerKillWindow(pids = launchInstance.GetNameserverPids())
            app.MainLoop()
        ##Launch the shell here, if needed.
        if self.state.GetSurface("Shell") == True:
            velShell.Start(self.state.GetSurface("ShellScript"))
##            if windows:
##                os.system("""start "%s" cmd""" % BUILDER_SHELL_NAME)
##            elif unix:
##                print "VE-Suite subshell started."
##                print "Type exit to return to your previous" + \
##                      " shell once you're done."
##                os.execl(UNIX_SHELL, "")
##            else:
##                print "SHELL ERROR! This OS isn't supported."
