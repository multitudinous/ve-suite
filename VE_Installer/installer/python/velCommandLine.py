# /*************** <auto-copyright.pl BEGIN do not edit this line> *************
# *
# * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
# *
# * Original Development Team:
# *   - ISU's Thermal Systems Virtual Engineering Group,
# *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
# *   - Reaction Engineering International, www.reaction-eng.com
# *
# * This library is free software; you can redistribute it and/or
# * modify it under the terms of the GNU Library General Public
# * License as published by the Free Software Foundation; either
# * version 2 of the License, or (at your option) any later version.
# *
# * This library is distributed in the hope that it will be useful,
# * but WITHOUT ANY WARRANTY; without even the implied warranty of
# * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# * Library General Public License for more details.
# *
# * You should have received a copy of the GNU Library General Public
# * License along with this library; if not, write to the
# * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# * Boston, MA 02111-1307, USA.
# *
# * -----------------------------------------------------------------
# * Date modified: $Date$
# * Version:       $Rev$
# * Author:        $Author$
# * Id:            $Id$
# * -----------------------------------------------------------------
# *
# *************** <auto-copyright.pl END do not edit this line> **************
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
from velSaveLoadConfig import *
import velShell

class CommandLine:
    """Launches VE Suite using arguments from the command line.

    Returns True if auto-launched,
    False if no launch, run GUI instead."""
    def __init__(self, opts, arguments, previousState = None):
        if previousState:
            self.state = previousState
        else:
            ##Set up default vars.
            self.state = CoveredConfig()
            LoadConfig(DEFAULT_CONFIG, self.state, loadLastConfig = True)
        self.state.ChangeMode(MODE_LIST[self.state.GetSurface("Mode")])
        commandLineActivators = ('-c', "--conductor", '-n', "--nameserver",
                                 '-x', "--xplorer", '-s', "--shell")
        setCommandLineMode = False
        devMode = False
        self.autoLaunch = False
        ##Set Configuration & Dev mode.
        for opt, arg in opts:
            ##print opt, arg ##TESTER
            if opt in ('-g', "--config="):
                try:
                    LoadConfig(arg, self.state)
                    self.state.ChangeMode(MODE_LIST[self.state.GetSurface("Mode")])
                except NonexistantConfigError, error:
                    print "ERROR: Config '%s' doesn't exist. Aborting launch." % error.value
                    sys.exit(2)
            elif opt in ('-d', "--dev"):
                devMode = True
                self.state.DevMode()
            elif opt in commandLineActivators:
                setCommandLineMode = True
                self.autoLaunch = True
            else:
                self.autoLaunch = True
        ##Set Files, if necessary.
        if arguments:
            self.state.InterpretArgument(arguments[0])
            if self.state.GetSurface("AutoRunVes"):
                self.autoLaunch = True
        ##Return to GUI if not autoLaunching.
        if not self.autoLaunch:
            return
        if setCommandLineMode:
            self.state.CommandLineMode()

        ##Set vars from the command line.
        for opt, arg in opts:
            if opt in ('-c', "--conductor"):
                self.state.Cover("Conductor", True, layer = COMMAND_LINE_LAYER)
            elif opt in ('-n', "--nameserver"):
                self.state.Cover("NameServer", True, layer = COMMAND_LINE_LAYER)
            elif opt in ('-x', "--xplorer"):
                self.state.Cover("Xplorer", True, layer = COMMAND_LINE_LAYER)
            elif opt in ('-l', "--cluster="):
                self.state.Cover("XplorerType", self.xplorerType(arg), layer = COMMAND_LINE_LAYER)
            elif opt in ('-s', "--shell"):
                self.state.Cover("Shell", True, layer = COMMAND_LINE_LAYER)
##            elif opt in ('-k', "--desktop"):
##                self.state.Cover("DesktopMode", True, layer = COMMAND_LINE_LAYER)
            elif opt in ('-j', "--jconf="):
                self.state.Cover("JconfDict", {"Default": arg}, layer = COMMAND_LINE_LAYER)
            elif opt in ('-t', "--taomachine="):
                self.state.Cover("TaoMachine", arg, layer = COMMAND_LINE_LAYER)
            elif opt in ('-p', "--port="):
                self.state.Cover("TaoPort", arg, layer = COMMAND_LINE_LAYER)
            elif opt in ('-w', "--dir="):
                self.state.Cover("Directory", arg, layer = COMMAND_LINE_LAYER)
##            elif opt in ('-m', "--master="):
##                self.state.Cover("ClusterMaster", arg, layer = COMMAND_LINE_LAYER)
            elif opt in ('-b', "--debug"):
                self.state.Cover("Debug", True, layer = COMMAND_LINE_LAYER)
            elif opt in ('-v', "--version"):
                version()
                sys.exit()
        ##Grab the first argument passed as the file to load.
        if len(arguments) > 0:
            self.state.InterpretArgument(arguments[0])
##        print "---------" ##TESTER
##        testDict = self.state.GetLaunchSurface() ##TESTER
##        for var in testDict: ##TESTER
##            print var, testDict[var] ##TESTER
            ## NOTE:
            ## These are now invalid:
            ## -b, -e, -x=, -v
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
            globalPath = launchInstance.GetPathEnv()
            velShell.Start(self.state.GetSurface("ShellScript"), globalPath)


    def AutoLaunched(self):
        return self.autoLaunch
    
    def xplorerType(self, inputArg):
        """Converts true/false input into a cluster/non-cluster XplorerType."""
        clusterMode = self.boolReader(inputArg)
        if clusterMode:
            return "OSG-VEPC"
        else:
            return "OSG-VEP"


    def boolReader(self, inputArg, default = False):
        """Converts user input into a True/False input."""
        yesInputs = ["true", "t", "yes", "y", "on"]
        noInputs = ["false", "f", "no", "n", "off"]
        ##Convert to lower-case for comparison
        inputArg = inputArg.lower()
        if inputArg in yesInputs:
            return True
        elif inputArg in noInputs:
            return False
        else:
            return default
