"""Handles immediate startups from VE-Launcher's command line."""
import wx
import os
from velBase import *
from velLaunchCode import *

class CommandLaunch:
    """Launches VE Suite using arguments from the command line."""
    def __init__(self, opts, args):
        config = wx.Config.Get()
        ##Set up vars.
        self.conductor = False
        self.nameServer = False
        self.xplorer = False
        self.xplorerType = None
        self.desktop = False
        self.taoMachine = config.Read("TaoMachine", DEFAULT_TAO_MACHINE)
        self.taoPort = config.Read("TaoPort", DEFAULT_TAO_PORT)
        self.mode = None
        self.depDir = config.Read("DependenciesDir", "None")
        self.workDir = config.Read("Directory", DIRECTORY_DEFAULT)
        self.jconf = None
        self.cluster = None
        self.clusterMaster = None
        self.builderDir = None
        self.shell = False
        self.builderDir = None

        ##Set vars from the command line.
        for opt, arg in opts:
            if opt in ('-c', "--conductor"):
                self.conductor = True
            elif opt in ('-n', "--nameserver"):
                self.nameServer = True
            elif opt in ('-x', "--xplorer="):
                self.xplorer = True
                if arg in XPLORER_TYPE_LIST:
                    self.xplorerType = XPLORER_TYPE_LIST.index(arg)
                else:
                    self.xplorerType = 0
            elif opt in ('-k', "--desktop"):
                self.desktop = True
            elif opt in ('-j', "--jconf="):
                self.jconf = arg
            elif opt in ('-t', "--taomachine="):
                self.taoMachine = arg
            elif opt in ('-p', "--port="):
                self.taoPort = arg
            ##NOTE: --setup will be used to set up working directories &
            ##dependencies folders without going into the GUI.
            ##Not implemented yet.
            ##elif opt in ('-s', "--setup"):
            ##    print "ERROR: Setup isn't implemented yet." + \
            ##          " Wait until next version."
            elif opt in ('-w', "--dir="):
                self.workDir = arg
            elif opt in ('-e', "--dep="):
                self.depDir = arg
            elif opt in ('-m', "--master="):
                self.clusterMaster = arg
            elif opt in ('-s', "--shell"):
                self.shell = True
            elif opt in ('-b', "--builder="):
                self.shell = True
                self.builderDir = arg

        ##Fill in any args left out from the default config settings.
        if self.jconf == None:
            if config.HasGroup(JCONF_CONFIG):
                selection = config.Read("JconfSelection", "None")
                config.SetPath(JCONF_CONFIG)
                self.jconf= config.Read(selection, "None")
                config.SetPath('..')
            else:
                self.jconf = DEFAULT_JCONF
        ##Set Xplorer Type
        if self.xplorerType == None:
            data = config.ReadInt("XplorerType", 0)
            if data in range(len(RADIO_XPLORER_LIST)):
                self.xplorerType = data
            else:
                self.xplorerType = 0
        del config
        ##Set VES file
        vesFile = None
        if len(args) > 0:
            vesFile = args[0]
        ##Launch
        Launch(None, self.workDir,
               self.nameServer, self.conductor, self.xplorer, self.xplorerType,
               self.jconf,
               self.taoMachine, self.taoPort,
               self.desktop,
               self.depDir, master = self.clusterMaster,
               shell = self.shell, builderDir = self.builderDir,
               vesFile = vesFile)
        ##Bring up the the Name Server Kill window.
        ##ERROR: ServerKillWindow() doesn't work outside of app.MainLoop()
        ##if self.nameServer and not self.shell:
        ##    win = ServerKillWindow()
        ##
        ##Launch the shell here, if needed.
        if self.shell:
            if windows:
                os.system("""start "%s" cmd""" % BUILDER_SHELL_NAME)
            elif unix:
                print "VE-Suite subshell started."
                print "Type exit to return to your previous" + \
                      " shell once you're done."
                os.execl(UNIX_SHELL, "")
            else:
                print "SHELL ERROR! This OS isn't supported."
