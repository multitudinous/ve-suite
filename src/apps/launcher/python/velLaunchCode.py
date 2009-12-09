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
import os, sys
import wx
from sys import stdin
from time import sleep ##Used for delays in launch
from platform import architecture ##Used to test if it's 32/64-bit
from socket import gethostname ##Used to get hostname
from velBase import *
from velJconfDict import *
from velClusterDict import *
from velLauncherDebugWin import *
from velModes import DEFAULT_JCONF
from velDepsArray import *
import string
import subprocess
import platform
import signal
#used to work with the windows registrey
#import _winreg

from os import name ##Used for getting system values
pj = os.path.join

# Setup prefix for mac specific calls
# TODO: append the mac specific paths to the path env var instead of the solution
# below
darwinPlatform = False
if string.find(sys.platform, 'darwin' ) != -1:
    darwinPlatform = True
    exeXplorerPrefix = pj( VELAUNCHER_DIR,'ves_xplorer.app', 'Contents', 'MacOS' )
    exeConductorPrefix = pj( VELAUNCHER_DIR,'ves_conductor.app', 'Contents', 'MacOS' )

class Launch:
    """Prepares the environment and launches the chosen programs.

    Order of steps:
        Change directory to chosen working directory.
        EnvSetup [sets environmental variables]
        Windows or Unix [launch based on os type]
        OnClose [quits the launcher]

    Functions:
        __init__(launcherWindow, workingDir, runName, runConductor, runXplorer,
                 typeXplorer, jconf, taoMachine, taoPort, desktopMode,
                 dependenciesDir, cluster, master)
        Windows(self, runName, runConductor, runXplorer, typeXplorer, jconf,
                desktopMode)
        Unix(runName, runConductor, runXplorer, typeXplorer, jconf,
             desktopMode, cluster, clusterMaster)
        EnvSetup(self, dependenciesDir, workingDir, taoMachine, taoPort,
                 clusterMaster)
        EnvFill(var, default)"""
    def __init__(self, settings, runOnce = True):
        """Sets environmental vars and calls OS-specific launch code."""
        ##Set self's variables
        self.settings = settings
        self.pathEnv = ""
        self.VeDepsDir = ""
        self.VeLauncherDir = ""
        self.nameserverPids = []
        self.conductorPid = []
        self.runOnce = runOnce
        ##Debug settings.
        self.debugOutput = []
        if self.settings["Debug"] and self.runOnce:
            self.inputSource = subprocess.PIPE
            machineName = platform.node()
            if posix:
                ves_userName = os.popen("whoami").readline().split()[0]
                vesDebugFileName = "debug." + ves_userName + "." + machineName + ".txt"
                #debugFileName = os.path.join('/','var', 'tmp', vesDebugFileName)
                debugFileName = os.path.join(self.settings["Directory"],vesDebugFileName)
            else:
                vesDebugFileName = "debug." + machineName + ".txt"
                debugFileName = os.path.join(self.settings["Directory"],vesDebugFileName)
            self.debugFile = open(debugFileName, 'w') 
            self.outputDestination = self.debugFile
            print "Output written to %s" %self.debugFile.name
        else:
            self.inputSource = None
            self.outputDestination = None

        ##Set Windows suffix.
        ##our installs do not support debug and release so there is not need for this
        self.windowsSuffix = ".exe"
        ##Set self.cluster to True if there's cluster functionality.
        ##If so, begin building self.clusterScript
        ##Used in EnvSetup and Windows/Unix.
        if settings["Cluster"]:
            self.clusterScript = ""
            ##Set up beginning of clusterScript for env setting.
            if unix:
                self.WriteClusterScriptPrefix()
        else:
            self.cluster = False
        ##Set the environmental variables
        self.EnvSetup()
        if self.settings["Debug"] and self.runOnce:
            frame = LauncherDebugWin(None, self.debugOutput)
        ##Change the directory.
        os.chdir(self.settings["Directory"])
        if self.settings["Shell"]: ##Shell is activated after destroy VE-Launcher.
            return
        ##Checks the OS and routes the launcher to the proper subfunction
        ##NOTE: Code out separate Setups, code in the combined Setup
        if windows:
            self.Windows()
        elif unix:
            self.Unix()
        else:
            print "ERROR: VE-Suite-Launcher doesn't support this OS."


    def GetNameserverPids(self):
        return self.nameserverPids


    def GetConductorPid(self):
        return self.conductorPid


    def GetPathEnv(self):
        return self.pathEnv


    def GetVeDepsDir(self):
        return self.VeDepsDir


    def GetVeLauncherDir(self):
        return self.VeLauncherDir


    def Windows(self):
        """Launches the chosen programs under an Unix OS.
        Keyword arguments:
        runName, runConductor, runXplorer -- Run NameServer/Conductor/Xplorer?
        typeXplorer -- Which Xplorer program to run.
        jconf -- Which .jconf file to use for Xplorer's settings.Naming_Service -ORBEndpoint htiop
        desktopMode -- Run in Desktop mode."""
        ##Name Server section
        if self.settings["NameServer"]:
            sleep(1)
            print "Starting Name Server."
            #Checking existence of executable file first before calling it
            exe = self.NameServiceCall()
            pids = []
            try:
                pids.append(subprocess.Popen(exe, 
                                             stdin = self.inputSource, 
                                             stdout = self.outputDestination, 
                                             stderr = self.outputDestination).pid)
            except OSError:
                print "Naming Service call error, \"%s\" not found on your environment." % exe
                sys.exit(2)
                
            #Checking existence of executable file first before calling it
            exe = "ves_ce" + self.windowsSuffix
            sleep(2)
            try:
                pids.append(subprocess.Popen(self.ServerCall(),
                                             stdin = self.inputSource,
                                             stdout = self.outputDestination,
                                             stderr = self.outputDestination).pid)
            except OSError:
                print "CE call error, \"%s\" not found on your environment." % exe
                sys.exit(2)
                
            sleep(2)
            self.nameserverPids = pids
                
        ##Cluster Xplorer section
        if self.settings["Cluster"]:
            print "Starting Xplorer on the cluster."
            ##Finish building cluster script
            self.ReadClusterTemplate()
            ##Write cluster script
            sourceFile = file(CLUSTER_FILE_PATH, 'w')
            sourceFile.write(self.clusterTemplate)
            sourceFile.close()
            ##Master call
            print "***MASTER CALL: %s***" %(self.settings["ClusterMaster"]) ##TESTER
            self.ExecuteClusterScript(self.settings["ClusterMaster"])
            sleep(self.settings["MasterWait"])
            ##Slave calls
            for comp in self.settings["ClusterSlaves"]:
                print "***CLUSTER CALL: %s***" %(comp) ##TESTER
                self.ExecuteClusterScript(comp)
                sleep(self.settings["SlaveWait"])
            ##Delete the cluster file afterwards when VE_Suite is terminated.
            ##To see the deletion code, please look at velServerKillWindow.py and
            ##velServerAutoKill____.py

        ##Xplorer section
        elif self.settings["Xplorer"]:
            print "Starting Xplorer."
            ##Append argument if desktop mode selected
            exe = "ves_xplorer" + self.windowsSuffix
            try:
                subprocess.Popen(self.XplorerCall(), 
                                 stdin = self.inputSource, 
                                 stdout = self.outputDestination, stderr = self.outputDestination)
            except OSError:
                print "Xplorer call error, \"%s\" not found on your environment." % exe
                sys.exit(2)
                
        ##Conductor section
        if self.settings["Conductor"]:
            print "Starting Conductor."
            exe = "ves_conductor" + self.windowsSuffix
            conduct_Pid = []
            ##Append argument if desktop mode selected
            if self.settings["VESFile"]:
                sleep(2)
            try:
                conduct_Pid.append(subprocess.Popen(self.ConductorCall(), 
                                                    stdin = self.inputSource, 
                                                    stdout = self.outputDestination, 
                                                    stderr = self.outputDestination).pid)
            except OSError:
                print "Conductor call error, \"%s\" not found on your environment." % exe
                sys.exit(2)

            sleep(2)
            self.conductorPid = conduct_Pid
            print "Finished sending launch commands."
                                
        return


    def Unix(self):
        """Launches the chosen programs under an Unix OS.

        NOTE: In Cluster mode, makes program calls through /usr/bin/X11.
        Make sure this links to your /usr/X11R#/bin directory.
        
        Keyword arguments:
        runName, runConductor, runXplorer -- Run NameServer/Conductor/Xplorer?
        typeXplorer -- Which Xplorer program to run.
        jconf -- Which .jconf file to use for Xplorer's settings.
        desktopMode -- Run in Desktop mode.
        cluster -- List of slaves in the cluster.
        clusterMaster -- The master of the cluster."""
        ##Kill any screen savers.
        #try:
        #    subprocess.Popen(["xset", "-display", ":0.0", "-dpms",
        #                      "s", "reset", "s", "off"])
        #except OSError:
        #    print "OS can not find \"xset\" command on your environment."
        #    sys.exit(2)
        
        ##Name Server section
        if self.settings["NameServer"]:
            sleep(2)
            print "Starting Name Server."
            exe = self.NameServiceCall()
            pids = []
            try:
                pids.append(subprocess.Popen(exe,
                                             stdout = self.outputDestination, stderr = subprocess.STDOUT).pid)
            except OSError:
                print "Naming Service call error, \"%s\" not found on your environment." % exe
                sys.exit(2)
                
            #Checking existence of executable file first before calling it
            exe = "ves_ce"
            sleep(3)
            try:
                pids.append(subprocess.Popen(self.ServerCall(), 
                                             stdout = self.outputDestination, stderr = subprocess.STDOUT).pid)
            except OSError:
                print "CE call error, \"%s\" not found on your environment." % exe
                sys.exit(2)
                
            self.nameserverPids = pids
            
        ##Cluster mode
        if self.settings["Cluster"]:
            print "Starting Xplorer on the cluster."
            ##Finish building cluster script
            self.WriteClusterScriptPost()
            clusterFilePath = CLUSTER_FILE_PATH
            ##Write cluster script
            sourceFile = file(clusterFilePath, 'w')
            sourceFile.write(self.clusterScript)
            sourceFile.close()
            os.system(("chmod 755 %s") % clusterFilePath)
            ##On the debug mode, it will display the cluster script on a CRT
            if self.settings["Debug"]:
                print "***Cluster Script*****************************************************"
                print "***Script File Location: %s" % clusterFilePath
                os.system(("cat %s") % clusterFilePath)
                print "**********************************************************************"
            ##Master call
            print "***MASTER CALL: %s***" %self.settings["ClusterMaster"] ##TESTER
            self.ExecuteClusterScriptUnix(self.settings["ClusterMaster"],
                                          clusterFilePath, "master")
            sleep(self.settings["MasterWait"])
            ##Slave calls
            for comp in self.settings["ClusterSlaves"]:
                print "***CLUSTER CALL: %s***" %(comp) ##TESTER
                self.ExecuteClusterScriptUnix(comp, clusterFilePath, "slave")
                sleep(self.settings["SlaveWait"])
            ##Delete the cluster file afterwards.
            ##print clusterFilePath
            ##if not self.settings["Debug"]:
            ##    os.remove(clusterFilePath)
        ##Xplorer section
        elif self.settings["Xplorer"]:
            print "Starting Xplorer."
            #Checking existence of project_tao_osg_vep file first before calling it
            exe = "ves_xplorer"
                
            try:
                subprocess.Popen(self.XplorerCall(), 
                                 stdout = self.outputDestination, stderr = subprocess.STDOUT)
            except OSError:
                print "Xplorer call error, \"%s\" not found on your environment." % exe
                sys.exit(2)
        
        ##Conductor section
        if self.settings["Conductor"]:
            print "Starting Conductor."
            #Checking existence of WinClient file first before calling it
            exe = "ves_conductor"

            conduct_Pid = []
            try:
                conduct_Pid.append(subprocess.Popen(self.ConductorCall(), 
                                                    stdout = self.outputDestination, stderr = subprocess.STDOUT).pid)
            except OSError:
                print "Conductor call error, \"%s\" not found on your environment." % exe
                sys.exit(2)
                
            sleep(3)
            self.conductorPid = conduct_Pid
            print "Finished sending launch commands."

        return
        
    def TaoPair(self):
        """Returns TAO_MACHINE:TAO_PORT."""
        taoMachine = os.getenv("TAO_MACHINE", "None")
        taoPort = os.getenv("TAO_PORT", "None")
        return "%s:%s" %(taoMachine, taoPort)


    def ServiceArg(self):
        """Returns the 'NameService=...' statement."""
        s = "NameService=corbaloc:iiop:%s/NameService" %(self.TaoPair())
        return s


    def NameServiceCall(self):
        """Returns a generic Naming_Service array."""
        exe = "Naming_Service"
        if windows:
            exe += ".exe"
        else:
            try:
                pid = subprocess.Popen(exe, stdin = self.inputSource,
                    stdout = self.outputDestination, 
                    stderr = self.outputDestination).pid
                os.kill( pid, signal.SIGKILL )
            except OSError:
                print "Naming Service call error, \"%s\" not found on your environment." % exe
                # this is for use on suse linux where tao is available
                # from a third party repo
                exe = "tao_cosnaming"
                try:
                    pid = subprocess.Popen(exe, stdin = self.inputSource,
                        stdout = self.outputDestination, 
                        stderr = self.outputDestination).pid
                    os.kill( pid, signal.SIGKILL )
                except OSError:
                    print "Naming Service call error, \"%s\" not found on your environment." % exe

        c = [exe, "-ORBEndPoint", "iiop://%s" %self.TaoPair()]
        return c


    def ServerCall(self):
        """Returns a generic Server call."""
        if unix:
            exe = "ves_ce"
        elif windows:
            exe = "ves_ce" + self.windowsSuffix
        else:
            exe = "Error"
        c = [exe, "-ORBInitRef", self.ServiceArg()]
        if windows:
            c[len(c):] = ["-ORBDottedDecimalAddresses", "1"]
        return c


    def ConductorCall(self):
        """Returns a generic Conductor call."""
        exe = "ves_conductor"
            
        if windows:
            exe += self.windowsSuffix
        ##Append ves arguments if needed.
        if self.settings["VESFile"]:
            ves = ["-VESFile", self.settings["VESFile"]]
        else:
            ves = []
        ##Append argument if desktop mode selected.
        if self.settings["DesktopMode"]:
            desktop = ["-VESDesktop"]
        else:
            desktop = []
        ##Construct the call.
        s = [exe, "-ORBInitRef", self.ServiceArg()]
        s[len(s):] = desktop
        s[len(s):] = ves
        if windows:
            s[len(s):] = ["-ORBDottedDecimalAddresses", "1"]
        return s


    def XplorerCall(self, computerType = "None"):
        """Returns a generic Xplorer call."""
        ##Append argument if desktop mode selected
        if self.settings["DesktopMode"]:
            w, h = wx.DisplaySize()
            desktop = ["-VESDesktop", str(w), str(h)]
        else:
            desktop = []
        ##Set Xplorer's type
        exe = "ves_xplorer"

        ##Tack on the Windows suffix.
        if windows:
            exe += self.windowsSuffix
        ##Construct the call
        #support for vrj 3.0
        if computerType.lower() == "master":
            s = [exe, "--vrjmaster", "--jconf", "%s" %self.settings["JconfPath"], "-ORBInitRef", self.ServiceArg() ]
        elif computerType.lower() == "slave":
            s = [exe, "--vrjslave", "-ORBInitRef", self.ServiceArg() ]
        else:
            # running in desktop mode or have an error in cluster mode
            s = [exe, "-ORBInitRef", self.ServiceArg(), "--jconf", "%s" %self.settings["JconfPath"]]
        ##Comment this out for vrj 3.0 support
        #s = [exe, "-ORBInitRef", self.ServiceArg(), "%s" %self.settings["JconfPath"]]
        if self.settings["XplorerType"] == "OSG-VEPC": ##OSG VEPC selection
            s += ["-VESCluster"]
            s += [ self.settings["ClusterMaster"].split('.')[0] ]
        
        if self.settings["RTT"]:
            s += ["-VESRTT"]

        s[len(s):] = desktop
        ##Special arguments added for master, slave, and solo computer Xplorer calls.
        if computerType.lower() == "master":
            s += []
        elif computerType.lower() == "slave":
            s += []
        else:
            s += []
        print s
        return s


    def ReadClusterTemplate(self):
        """Prepares the cluster template (for Windows)."""
        clusterFilePath = os.path.join('C:\\WINDOWS', 'Temp', "cluster.bat")
        drive = "%s:" %self.settings["Directory"].split(':')[0]
        user = os.getenv('USERNAME')
        self.clusterCall = ["psexec", "<SLAVE GOES HERE>",
                            "-i", "-e", "-c", CLUSTER_FILE_PATH, "\\k"]
        ##Insert username directory if specified
        if self.settings["User"] != "":
            self.clusterCall[2:2] = ["-u", self.settings["User"]]
        ##Begin cluster template
        self.clusterTemplate = ""
        if self.settings["Debug"]:
            self.clusterTemplate += "@ECHO ON\n"
        else:
            self.clusterTemplate += "@ECHO OFF\n"
        if os.path.exists(TEMPLATE_PATH):
            COMMENT_NOTE = '##'
            VAR_SEP = '%'
            f = file(TEMPLATE_PATH)
            for line in f:
                line = line.lstrip()
                ##Toss comments & blank lines.
                if len(line) == 0 or line[:2] == COMMENT_NOTE:
                    continue
                ##Add the rest to the cluster.bat.
                self.clusterTemplate += line
            f.close()
        else:
            pass
        self.clusterTemplate += "\n"
        self.clusterTemplate += "%s\n" %drive
        self.clusterTemplate += "cd %s\n" %self.settings["Directory"]
        self.clusterTemplate += "\n"
        self.clusterTemplate += self.clusterScript
        self.clusterTemplate += "\n"
        self.clusterTemplate += "%s\n" %string.join(self.XplorerCall())
        if self.settings["Debug"]:
            self.clusterTemplate += "pause\n"
        return


    def WriteClusterScriptPrefix(self):
        """Writes the cluster script section before the environment setting."""
        if unix:
            ssh_cmd = os.popen("which ssh").readline().split()[0]
            self.clusterScript = "#!/bin/csh\n"
            self.clusterScript += "%s -C $1 << EOF\n" % ssh_cmd
            self.clusterScript += "/bin/csh\n"
            ##Turn off comp's screen saver
            #self.clusterScript += "xset -display :0.0" + \
            #                      " -dpms s reset s off\n"
            if self.settings["EnableVSync"]:
                self.clusterScript += 'setenv __GL_SYNC_TO_VBLANK "1"\n'
            self.WriteToClusterScript("PYTHONPATH")
            self.WriteToClusterScript("DISPLAY")
        else:
            self.clusterScript = "ERROR: Unsupported OS type."
        return
            

    def WriteClusterScriptPost(self):
        """Writes the cluster script section after the environment setting."""
        if unix:
            ves_userName = os.popen("whoami").readline().split()[0]
            ves_machineName = os.popen("hostname").readline().split()[0]
            vesOutFile = "ves." + ves_userName +"."+ves_machineName+".out"
            
            slaveCommand = "%s" %(string.join(self.XplorerCall("slave")))
            if self.settings["Debug"]: 
                slaveCommand = slaveCommand + "> & $1.%s.log" %vesOutFile
            masterCommand = "%s" %(string.join(self.XplorerCall("master")))
            self.clusterScript += 'cd "%s"\n' %self.settings["Directory"]
            if self.settings["Debug"]: 
                self.clusterScript += 'limit coredumpsize 1000000000000\n'
            self.clusterScript += 'if ( $2 == "slave" ) then\n'
            self.clusterScript += "    %s\n" %(slaveCommand)
            self.clusterScript += "else\n"
            ##NOTE: StdOutput written to local /var/tmp file to avoid
            ##network traffic issues. Change " &" to " 2>&1 &" to reroute
            ##StdErrors to local /var/tmp file as well.
            self.clusterScript += "    %s " % (masterCommand) + '| grep -v -e "Stable buffer is empty." > '
            if self.settings["Debug"]: 
                self.clusterScript += str(os.path.join('/','var', 'tmp', vesOutFile))
            #else :
            #    self.clusterScript += os.path.join('/','dev', 'null')
            self.clusterScript += " &\n"
            self.clusterScript += "endif\n"
            self.clusterScript += "EOF\n"
##        elif windows:
##            commandList = self.XplorerCall()
##            command = ""
##            for word in commandList:
##                command += "%s " %str(word)
##            command += "\n"
##            workingDir = os.getenv("VE_WORKING_DIR","None")
##            workingDrive = "%s:" %workingDir.split(':')[0]
##            self.clusterScript += "%s\n" %workingDrive
##            self.clusterScript += 'cd "%s"\n' %workingDir
##            self.clusterScript += "%s\n" %(command)
        else:
            self.clusterScript += "ERROR: OS not supported."
        #print self.clusterScript


    def ExecuteClusterScriptUnix(self, nodeName,
                                 clusterFilePath, nodeStatus = "slave"):
        """Executes the ClusterScript for nodeName on Unix."""
        if unix:
            if gethostname().split('.')[0] == nodeName.split('.')[0]:
                s = self.XplorerCall( nodeStatus )
                print "ExecuteClusterScriptUnix"
                print s
                try:
                    subprocess.Popen( s )
                except OSError:
                    exe = "ves_xplorer"
                    print "Xplorer Call Error, \"%s\" not found on your environment."
                    sys.exit(2)
                return
            else:
                os.system(("%s %s %s &") % (clusterFilePath, nodeName, nodeStatus))
                return
        else:
            print "Error! Windows linking into Unix cluster function."

        
    def ExecuteClusterScript(self, nodeName):
        """Executes the ClusterScript for nodeName on Windows."""
        if windows:
            print "Executing %s!" %nodeName
            ##Do a regular call if the initial machine's the node.
            if gethostname() == nodeName.split('.')[0]:
                try:
                    subprocess.Popen(self.XplorerCall())
                except OSError:
                    exe = "ves_xplorer"
                    print "Xplorer Call Error, \"%s\" not found on your environment."
                    sys.exit(2)
                return
            ##Else call the script on the other computer in psexec.
            self.clusterCall[1] = "\\\\%s" %nodeName
            ##Sets the psexec window to stay up after it quits (Debug mode).
            if self.settings["Debug"]:
                try:
                    subprocess.Popen(["cmd", "/k"] + self.clusterCall)
                except OSError:
                    print "System function call error, fail to run \"cmd\" command."
                    sys.exit(2)
            else:
                try:
                    subprocess.Popen(self.clusterCall)
                except OSError:
                    ##Send error & 'psexec' to error call.
                    self.ErrorCall(sys.exc_info(), self.clusterCall[0])
        else:
            print "Error! Unix linking into Windows cluster function."


    def ErrorCall(self, errorInfo, step):
        """Brings up the system error encountered during [step]."""
        ##Arrays of names/errors for comparison below.
        fileNotFoundErrors = ["[Errno 2] The system cannot find the file specified"]
        namingServiceNames = [ self.NameServiceCall() ]
        if step == "psexec":
            stepName = "PsExec for cluster"
        elif step in namingServiceNames:
            stepName = "the Name Server"
        else:
            stepName = step
        ##Set extraNotes here.
        ##extraNotes gives additional information on potential error solutions.
        if str(errorInfo[1]) in fileNotFoundErrors:
            if step == "psexec":
                ##psexec not found
                extraNotes = "Check if psexec is installed\n" + \
                             "and on PATH."
            elif step in namingServiceNames:
                ##namingService not found
                if windows:
                    depsName = "VE-Suite's dependencies"
                else:
                    depsName = "ACE/TAO"
                extraNotes = "Check if %s is in the /bin\n" % (step) + \
                             "directory of %s." % (depsName)
            else:
                ##conductor or xplorer not found.
                extraNotes = "Check if %s is in\n" + \
                             "VE-Suite's /bin directory."
        else:
            extraNotes = ""
        ##Tack line returns on extraNotes if it contains anything.
        if extraNotes:
            extraNotes = "\n\n" + extraNotes
        ##Print the error warning in console.
        print "Error encountered. Pausing launch."
        print str(errorInfo[1])
        ##The actual warning box.
        dlg = wx.MessageDialog(None,
                               "An error came up while" +
                               " launching %s:\n\n" % (stepName) +
                               str(errorInfo[1]) + extraNotes +
                               "\n\nDo you want to continue launching?",
                               "VE-Suite Launch Error",
                               wx.YES_NO | wx.YES_DEFAULT)
        if dlg.ShowModal() == wx.ID_NO:
            dlg.Destroy()
            print "Quitting launch."
            raise QuitLaunchError
            ##IMPROVE: Kill all other started progs?
        else:
            dlg.Destroy()
            print "Resuming launch."
        return


    def EnvSetup(self):
        """Sets up the environmental variables to launch VE-Suite's programs.

        Only takes care of basic variables. Coders with custom builds can set
        advanced variables by creating a batch/shell file to set the extra
        variables, then execute the launcher in --dev mode as its last command.
        The environmental settings will carry over.

        Variables overwritten by this class:
        CFDHOSTTYPE (removes parantheses from CFDHOSTTYPE)
        TAO_MACHINE
        TAO_PORT
        VPR_DEBUG_ENABLE
        VPR_DEBUG_NFY_LEVEL
        OSG_NOTIFY_LEVEL

        Variables overwritten (when not in dev mode):
        ##NOTE: Change to set if unset, period.
        ##VE_SUITE_HOME
        ##VE_INSTALL_DIR
        VE_DEPS_DIR
        VE_WORKING_DIR
        ##PHSHAREDSIZE
        NO_PERF_PLUGIN
        NO_RTRC_PLUGIN
        PFNFYLEVEL
        JCCL_BASE_DIR
        JCCL_DEFINITION_PATH
        ##VJ_CFG_PATH
        ##NSPR_ROOT
        SNX_BASE_DIR
        VJ_BASE_DIR
        ##VJ_DEPS_DIR
        OSG_FILE_PATH

        Variables appended:
        PYTHONPATH (Windows systems only)
        PATH
        LD_LIBRARY_PATH or LD_LIBRARYN32_PATH (Unix systems only)"""
        ##Set where VE-Suite pre-complied dependencies are installed
        ##NOTE: Receives this from the launcher.
        self.EnvFill("VE_DEPS_DIR", str(self.settings["DependenciesDir"]))
        self.EnvFill("OSG_POLYTRANS_CONFIG_FILE", "ptconfigfile.txt")
        ##Gets working directory
        ##NOTE: Receives this from the launcher.
        self.EnvFill("VE_WORKING_DIR", str(self.settings["Directory"]))
        ##vrJuggler  
        ##These are setup for using VE-Suite dependency install's location
        ##change only if you are using your own build
        if windows:
            vjDir =  os.path.join(os.getenv("VE_DEPS_DIR"))
            ## If we are using the installer on windows then overwrite 
            ## any system defined VJ_BASE_DIR
            self.EnvFill("VJ_BASE_DIR", vjDir, not self.settings["DevMode"] )
        elif self.settings["JugglerDep"] != "None":
            vjDir = self.settings["JugglerDep"]
            self.EnvFill("VJ_BASE_DIR", vjDir, not self.settings["DevMode"])
        ##self.EnvFill("VJ_DEPS_DIR", os.path.join(os.getenv("VE_DEPS_DIR"),
        ##                                         JUGGLER_FOLDER))

        ##Set TAO variables
        self.EnvFill("TAO_MACHINE", str(self.settings["TaoMachine"]), True)
        self.EnvFill("TAO_PORT", str(self.settings["TaoPort"]), True)

        ##Set CFDHOSTNAME
        self.EnvFill("CFDHOSTTYPE", CFD_HOST_TYPE)

        ##Juggler debug output level
        if self.settings["VPRDebug"] < 0:
            self.EnvFill("VPR_DEBUG_ENABLE", "0", overwrite = True)
        else:
            self.EnvFill("VPR_DEBUG_ENABLE", "1", overwrite = True)
        self.EnvFill("VPR_DEBUG_NFY_LEVEL", str(self.settings["VPRDebug"]),
                     overwrite = True)
        if self.settings["OSGNotifyLevel"]:
            self.EnvFill("OSG_NOTIFY_LEVEL",
                         str(self.settings["OSGNotifyLevel"]),
                         overwrite = True)
        else:
            self.EnvFill("OSG_NOTIFY_LEVEL", "[Deleted]", overwrite = True)
        self.EnvFill("NO_PERF_PLUGIN", "TRUE")
        self.EnvFill("NO_RTRC_PLUGIN", "TRUE")

        ##Juggler dependencies
        ##These are currently set relative to VE-Suite's install
        vjBaseDir = os.getenv("VJ_BASE_DIR")
        self.EnvFill("JCCL_BASE_DIR", vjBaseDir)
        if not windows:
            self.EnvFill("JCCL_DEFINITION_PATH",
                         os.path.join(os.getenv("JCCL_BASE_DIR"), "definitions"))
        else:
            self.EnvFill("JCCL_DEFINITION_PATH",
                         os.path.join(os.getenv("JCCL_BASE_DIR"),
                                      "share", "definitions"))
        ##self.EnvFill("VJ_CFG_PATH", os.path.join(vjBaseDir, "definitions")) ##Can get rid of?

        self.EnvFill("SNX_BASE_DIR", vjBaseDir)
        
        ##Append OSG_FILE_PATH
        self.EnvAppend("OSG_FILE_PATH", [
            os.path.join(VELAUNCHER_DIR, "..", "share", "vesuite")+
            os.path.pathsep+
            os.path.join(VELAUNCHER_DIR, "..", "share", "vesuite","glsl")])


        ##Update OSG_FILE_PATH & PATH (and the Library Path for Unix)
        if windows:
            vesLibPath = os.path.join(VELAUNCHER_DIR, '..', 'lib')
            if not os.path.exists( vesLibPath ):
                vesLibPath = os.path.join(VELAUNCHER_DIR, '..', 'lib64')

            pathList = [os.path.join(os.path.join(VELAUNCHER_DIR,"bin")),
                        os.path.join(VELAUNCHER_DIR),
                        os.path.join(os.path.join(VELAUNCHER_DIR,"lib")),
                        vesLibPath,
                        os.path.join(str(os.getenv("VE_DEPS_DIR")), "share"),
                        os.path.join(str(os.getenv("VE_DEPS_DIR")), "bin"),
                        os.path.join(str(os.getenv("VJ_BASE_DIR")), "lib")]

            #if self.settings["BuilderDir"] != None:
            #    pathList.append(os.path.join(str(self.settings["BuilderDir"]), "bin"))
            ##TEST to append 64-bit libraries:
            
            #value = "Software\Wow6432Node"
            #root = _winreg.HKEY_LOCAL_MACHINE
            #p = value.rfind('\\') + 1
            #keyp = value[:p-1]          # -1 to omit trailing slash
            #val = value[p:]
            #aReg = _winreg.ConnectRegistry(None,root)
            #k = _winreg.OpenKeyEx(aReg, keyp)
            #k = _winreg.OpenKeyEx(k, val)
            #flag64 = _winreg.QueryInfoKey( k )
            # need closekey calls
            #vesArchitecture = 
            #if flag64 gt 1:
            
            if architecture()[0] == "64bit":
                pathList.append(os.path.join(str(os.getenv("VJ_BASE_DIR")), "lib64"))
            ##Append the custom dependencies list.
            if architecture()[0] == "64bit":
                libTag = "lib64"
            else:
                libTag = "lib"

            for entry in self.settings["Dependencies"].GetNames():
                pathList.append(os.path.join(entry, "bin"))
                pathList.append(os.path.join(entry, libTag))

            #Add pathEnv value for shell launching mode
            self.EnvAppend("PATH", pathList)
            self.pathEnv = str(os.getenv("PATH"))
            for pathItr in pathList:
                self.pathEnv = pathItr + os.path.pathsep + self.pathEnv 

            #Get dependency base directory
            self.VeDepsDir = os.path.join(str(os.getenv("VE_DEPS_DIR")), "bin")
            self.VeLauncherDir = str(VELAUNCHER_DIR)
            
        elif unix:
            ##Set name of library path
            if darwinPlatform:
                libraryPath = "DYLD_LIBRARY_PATH"
            else:
                libraryPath = "LD_LIBRARY_PATH"
            ##Update the library path
            libList= [os.path.join(VELAUNCHER_DIR),
                      os.path.join(VELAUNCHER_DIR, '..', "lib")]
            ##Outdated paths.
            ##libList += [os.path.join(str(os.getenv("VE_DEPS_DIR")), "bin"),
            ##            os.path.join(VELAUNCHER_DIR, "bin")]

            ##Update the path
            pathList= [os.path.join(VELAUNCHER_DIR),
                       os.path.join(VELAUNCHER_DIR, '..', "lib")]
            # for mac add the app bundle directories to the path
            if darwinPlatform:
                pathList += [exeConductorPrefix, exeXplorerPrefix]
            ##print "sysPath[0]: %s" % os.path.realpath(os.path.abspath(sys.executable))
            ##Outdated paths.
            ##pathList += [os.path.join(VELAUNCHER_DIR, "bin"),
            ##             os.path.join(str(os.getenv("VE_DEPS_DIR")), "bin")]
            ##print "self.settings[builderdir]: %s" % self.settings["BuilderDir"] 
            ##if self.settings["BuilderDir"] != None:
            ##    pathList.append(os.path.join(str(self.settings["BuilderDir"]), "bin"))
            ##Append the custom dependencies list.
            if architecture()[0] == "64bit":
                libTag = "lib64"
            else:
                libTag = "lib"
            for entry in self.settings["Dependencies"].GetNames():
                pathList.append(entry)
                libList.append(os.path.join(entry, libTag))
                libList.append(entry)
                
            pathList.append(os.path.join(str(os.getenv("VJ_BASE_DIR")), "bin"))

            ##TEST to append 64-bit libraries:
            if architecture()[0] == "64bit":
                libList.append(os.path.join(str(os.getenv("VJ_BASE_DIR")), "lib64"))
            else:
                libList.append(os.path.join(str(os.getenv("VJ_BASE_DIR")), "lib"))

            ##Write the libraries & paths.
            self.EnvAppend("PATH", pathList)
            self.EnvAppend(libraryPath, libList)
            
            self.VeLauncherDir = str(VELAUNCHER_DIR)
            ##print "VeLauncherDir: %s" % self.VeLauncherDir
            
        ##Update other vars listed.
        if self.settings["Cluster"]:
            for var in self.settings["ExtraVariables"]:
                if self.settings["Debug"]:
                    if os.getenv(var) != None:
                        print "%s: %s" %(var, os.getenv(var))
                    else:
                        print "%s isn't set." %(var)
                if os.getenv(var) != None:
                    self.WriteToClusterScript(var)

    def EnvAppend(self, var, appendages, sep=os.path.pathsep):
        """Appends appendages (list) to var, using sep to separate them."""
        sep = os.path.pathsep
        originalVar = os.getenv(var, "None")
        ##if not self.settings["DevMode"]:
        modifiedVar = os.getenv(var, None)
        empty = (modifiedVar == None)
        for app in appendages:
            if empty:
                modifiedVar = app
                empty = False
            else:
                modifiedVar = app + sep + modifiedVar 
        os.environ[var] = modifiedVar
        ##Put var in clusterScript
        self.WriteToClusterScript(var)
        ##Debug printout.
        if self.settings["Debug"]:
            ##print "%s: %s" %(var, os.getenv(var))
            self.debugOutput.append(["%s: " %(var), "Bold"])
            self.debugOutput.append(["%s" %os.getenv(var), None])
            self.debugOutput.append(["\n", None])
            self.debugOutput.append(["Original: ", "Bold"])
            self.debugOutput.append(["%s" %originalVar, None])
            self.debugOutput.append(["\n", None])
            self.debugOutput.append(["Added: ", "Bold"])
            self.debugOutput.append(["\n", None])
            for app in appendages:
                self.debugOutput.append([app, None])
                self.debugOutput.append(["\n", None])
            ##self.debugOutput.append(["\n", None])
	    	
    def EnvFill(self, var, default, overwrite = False):
        """Overwrites env var in normal mode, ensures it's filled in dev mode.

        Does not overwrite a filled variable in devMode.
        Overwrites a filled variable in normal mode.
        If overwrite == True, overwrites the variable in both modes."""
        ##Fix to stop errors when passing None as the default.
        if default == None:
            default = "None"
        ##Record original var.
        if not EnvVarEmpty(var):
            originalVar = os.getenv(var)
        else:
            originalVar = "None"
        ##Write the var to the environment.
        if default == "[Deleted]":
            if os.environ.has_key(var):
                del os.environ[var]
        elif not overwrite and not EnvVarEmpty(var):
            os.environ[var] = os.getenv(var, default)
            ##Put var in clusterScript
            self.WriteToClusterScript(var)
        else:
            os.environ[var] = default
            ##Put var in clusterScript
            self.WriteToClusterScript(var)
        ##Debug printout.
        if self.settings["Debug"]:
##            print "%s: %s" %(var, os.getenv(var))
            self.debugOutput.append(["%s: " %(var), "Bold"])
            self.debugOutput.append(["%s" %os.getenv(var), None])
            self.debugOutput.append(["\n", None])
            self.debugOutput.append(["Original: ", "Bold"])
            self.debugOutput.append(["%s" %originalVar, None])
            self.debugOutput.append(["\n\n", None])

    def WriteToClusterScript(self, var):
        """Writes an environmental setting to clusterScript.

        Exact function determined by cluster's default shell."""
        if not self.settings["Cluster"]:
            return
        if unix:
            ##Choose export command based on shell type.
            ##shellName = 'csh'#os.getenv('SHELL', 'None')
            ##if shellName[-3:] != 'csh':
            ##    self.clusterScript += 'export %s="%s"\n' %(var, os.getenv(var))
            ##else:
            ##self.clusterScript += 'setenv %s "%s"\n' %(var, os.getenv(var))
            
            ##Since we hardcoded the shell type to be set to the bash shell, 
            ##now we only use the bash commands. 
            self.clusterScript += 'setenv %s "%s"\n' %(var, os.getenv(var))
        elif windows:
            self.clusterScript += 'set %s=%s\n' %(var, os.getenv(var))
