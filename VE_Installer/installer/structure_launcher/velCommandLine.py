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

class CommandLaunch:
    """Launches VE Suite using arguments from the command line."""
    def __init__(self, opts, args):
##        config = wx.Config.Get()
        ##Set up default vars.
        self.state = CoveredConfig()
        LoadConfig(CONFIG_DEFAULT, self.state)
        self.state.CommandLaunch()
        if ("--dev", "") in opts:
            devMode = True
            self.state.DevMode()
        else:
            devMode = False
##        strReads = ["Directory",
##                    "DependenciesDir",
##                    "JconfSelection",
##                    "TaoMachine",
##                    "BuilderDir"]
##        intReads = ["XplorerType"]
##        if config.GetEntryType("TaoPort") == 3: ##3: Int entry type
##            intReads.append("TaoPort")
##        else:
##            strReads.append("TaoPort")
##        for var in strReads:
##            if config.Exists(var):
##                self.state.Edit(var, config.Read(var))
##        for var in intReads:
##            if config.Exists(var):
##                self.state.Edit(var, config.ReadInt(var))
##        self.state.Edit("JconfDict", JconfDict())
##        self.state.Edit("ClusterDict", ClusterDict(preset = {}))

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
            elif opt in ('-j', "--jconf="):
                self.state.Edit("JconfDict", {"Default": arg})
            elif opt in ('-t', "--taomachine="):
                self.state.Edit("TaoMachine", arg)
            elif opt in ('-p', "--port="):
                self.state.Edit("TaoPort", arg)
            elif opt in ('-v', "--ves="):
                self.state.VesArgument(arg)
            ##NOTE: --setup will be used to set up working directories &
            ##dependencies folders without going into the GUI.
            ##Not implemented yet.
            ##elif opt in ('-s', "--setup"):
            ##    print "ERROR: Setup isn't implemented yet." + \
            ##          " Wait until next version."
            elif opt in ('-w', "--dir="):
                self.state.Edit("Directory", arg)
            elif opt in ('-e', "--dep="):
                self.state.Edit("DependenciesDir", arg)
            elif opt in ('-m', "--master="):
                self.state.Edit("ClusterMaster", arg)

        ##Fill in any args left out from the default config settings.
##        if self.jconf == None:
##            if config.HasGroup(JCONF_CONFIG):
##                selection = config.Read("JconfSelection", "None")
##                config.SetPath(JCONF_CONFIG)
##                self.jconf= config.Read(selection, "None")
##                config.SetPath('..')
##            else:
##                self.jconf = DEFAULT_JCONF
##        ##Set Xplorer Type
##        if self.xplorerType == None:
##            data = config.ReadInt("XplorerType", 0)
##            if data in range(len(RADIO_XPLORER_LIST)):
##                self.xplorerType = data
##            else:
##                self.xplorerType = 0
##        del config
##        ##Set VES file
##        vesFile = None
##        if len(args) > 0:
##            vesFile = args[0]
        ##Launch
        launchInstance = Launch(self.state.GetLaunchSurface())
        ##Destroy the Launch progress window.
        ##progress.OnClose("this message does not matter")
        ##Show NameServer kill window if NameServer was started.
        if self.state.GetSurface("NameServer"):
            app = wx.PySimpleApp()
            window = ServerKillWindow(pids = launchInstance.GetNameserverPids())
            app.MainLoop()
##        Launch(None, self.workDir,
##               self.nameServer, self.conductor, self.xplorer, self.xplorerType,
##               self.jconf,
##               self.taoMachine, self.taoPort,
##               self.desktop,
##               self.depDir, master = self.clusterMaster,
##               shell = self.shell, builderDir = self.builderDir,
##               vesFile = vesFile)
        ##Bring up the the Name Server Kill window.
        ##ERROR: ServerKillWindow() doesn't work outside of app.MainLoop()
        ##if self.nameServer and not self.shell:
        ##    win = ServerKillWindow()
        ##
        ##Launch the shell here, if needed.
        if self.state.GetSurface("Shell") == True:
            if windows:
                os.system("""start "%s" cmd""" % BUILDER_SHELL_NAME)
            elif unix:
                print "VE-Suite subshell started."
                print "Type exit to return to your previous" + \
                      " shell once you're done."
                os.execl(UNIX_SHELL, "")
            else:
                print "SHELL ERROR! This OS isn't supported."
