import os
from wx import DisplaySize
from time import sleep ##Used for delays in launch
from platform import architecture ##Used to test if it's 32/64-bit
from socket import gethostname ##Used to get hostname
from velBase import *
from velJconfDict import *
from velClusterDict import *
from velModes import DEFAULT_JCONF
import string
import subprocess

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
    def __init__(self, settings):
        """Sets environmental vars and calls OS-specific launch code."""
        ##Adapt settings to variables.
##        workingDir = self.settings["Directory"]
##        runName = self.settings["NameServer"]
##        runConductor = self.settings["Conductor"]
##        runXplorer = self.settings["Xplorer"]
##        typeXplorer = self.settings["XplorerType"]
##        jconf = self.settings["JconfPath"]
##        taoMachine = self.settings["TaoMachine"]
##        taoPort = self.settings["TaoPort"]
##        desktopMode = self.settings["DesktopMode"]
##        dependenciesDir = self.settings["DependenciesDir"]
##        cluster = self.settings["ClusterNodes"]
##        master = self.settings["ClusterMaster"]
##        shell = self.settings["Shell"]
##        builderDir = self.settings["BuilderDir"]
##        vesFile = self.settings["VESFile"]
##        if vesFile == None:
##            vesFile = None
        ##Set self's variables
##        print settings ##TESTER
        self.settings = settings
        self.nameserverPids = []
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

    def Windows(self):
        """Launches the chosen programs under an Unix OS.

        Keyword arguments:
        runName, runConductor, runXplorer -- Run NameServer/Conductor/Xplorer?
        typeXplorer -- Which Xplorer program to run.
        jconf -- Which .jconf file to use for Xplorer's settings.
        desktopMode -- Run in Desktop mode."""
        ##Name Server section
        if self.settings["NameServer"]:
            sleep(1)
            print "Starting Name Server."
            pids = []
            pids.append(subprocess.Popen(self.NameServiceCall()).pid)
            sleep(5)
            pids.append(subprocess.Popen(self.ServerCall()).pid)
            sleep(5)
            self.nameserverPids = pids
        ##Cluster Xplorer section
        if self.settings["Cluster"]:
            print "Starting Xplorer on the cluster."
            ##Finish building cluster script
##            self.WriteClusterScriptPost(typeXplorer, jconf, desktopMode)
            self.ReadClusterTemplate()
##            clusterFileName = "cluster.bat"
##            clusterFilePath = os.path.join('C:\\WINDOWS', 'Temp', clusterFileName)
##            print clusterFilePath ##TESTER
            ##Write cluster script
            sourceFile = file(CLUSTER_FILE_PATH, 'w')
##            sourceFile.write(self.clusterScript)
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
        ##Xplorer section
        elif self.settings["Xplorer"]:
            print "Starting Xplorer."
            ##Append argument if desktop mode selected
            subprocess.Popen(self.XplorerCall())
        ##Conductor section
        if self.settings["Conductor"]:
            print "Starting Conductor."
            ##Append argument if desktop mode selected
            if self.settings["VESFile"]:
                sleep(10)
            subprocess.Popen(self.ConductorCall())
        print "Finished sending launch commands."
        return

    def Unix(self):
        """Launches the chosen programs under an Unix OS.

        Keyword arguments:
        runName, runConductor, runXplorer -- Run NameServer/Conductor/Xplorer?
        typeXplorer -- Which Xplorer program to run.
        jconf -- Which .jconf file to use for Xplorer's settings.
        desktopMode -- Run in Desktop mode.
        cluster -- List of slaves in the cluster.
        clusterMaster -- The master of the cluster."""
        ##Kill any screen savers.
        subprocess.Popen(["/usr/X11R6/bin/xset", "-display", ":0.0", "-dpms",
                          "s", "reset", "s", "off"])
        ##Name Server section
        if self.settings["NameServer"]:
            sleep(1)
            print "Starting Name Server."
            pids = []
            pids.append(subprocess.Popen(self.NameServiceCall()).pid)
            sleep(5)
            pids.append(subprocess.Popen(self.ServerCall()).pid)
            self.nameserverPids = pids
##            os.system("%s &" %(self.NameServiceCall()))
##            sleep(5)
##            os.system("%s &" %(self.ServerCall()))
        ##Cluster mode
        if self.settings["Cluster"]:
            print "Starting Xplorer on the cluster."
            ##Finish building cluster script
            self.WriteClusterScriptPost()
            clusterFileName = "cluster.tsh"
            clusterFilePath = os.path.join(VELAUNCHER_DIR, clusterFileName)
            ##Write cluster script
            sourceFile = file(clusterFilePath, 'w')
            sourceFile.write(self.clusterScript)
            sourceFile.close()
            ##Master call
            print "***MASTER CALL: %s***" %self.settings["ClusterMaster"] ##TESTER
            os.system("source %s %s &" %(clusterFilePath,
                                         self.settings["ClusterMaster"]))
            sleep(self.settings["MasterWait"])
            ##Slave calls
            for comp in self.settings["ClusterSlaves"]:
                print "***CLUSTER CALL: %s***" %(comp) ##TESTER
                os.system("source %s %s &" %(clusterFilePath,
                                             self.settings["ClusterMaster"]))
                sleep(self.settings["SlaveWait"])
        ##Xplorer section
        elif self.settings["Xplorer"]:
            print "Starting Xplorer."
            subprocess.Popen(self.XplorerCall())
        ##Conductor section
        if self.settings["Conductor"]:
            print "Starting Conductor."
##            if vesFile != None:
##                sleep(5)
            subprocess.Popen(self.ConductorCall())
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
        c = [exe, "-ORBEndPoint", "iiop://%s" %self.TaoPair()]
        return c

    def ServerCall(self):
        """Returns a generic Server call."""
        if unix:
            exe = "Exe_server"
        elif windows:
            exe = "Winserverd.exe"
        else:
            exe = "Error"
        c = [exe, "-ORBInitRef", self.ServiceArg()]
        if windows:
            c[len(c):] = ["-ORBDottedDecimalAddresses", "1"]
        return c

    def ConductorCall(self):
        """Returns a generic Conductor call."""
        exe = "WinClient"
        if windows:
            exe += "d.exe"
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

    def XplorerCall(self):
        """Returns a generic Xplorer call."""
        ##Append argument if desktop mode selected
        if self.settings["DesktopMode"]:
            w, h = DisplaySize()
            desktop = ["-VESDesktop", str(w), str(h)]
        else:
            desktop = []
        ##Set Xplorer's type
        if self.settings["XplorerType"] == 0: ##OSG selection
            exe = "project_tao_osg"
        elif self.settings["XplorerType"] == 1: ##OSG VEP selection
            exe = "project_tao_osg_vep"
        elif self.settings["XplorerType"] == 2: ##OSG VEPC selection
            exe = "project_tao_osg_vep_cluster"
        ##Tack on the Windows suffix.
        if windows:
            exe += "_d.exe"
        ##Construct the call
        s = [exe, "-ORBInitRef", self.ServiceArg(),
             "%s" %self.settings["JconfPath"]]
        s[len(s):] = desktop
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
            self.clusterScript = "#!%s\n" % os.getenv('SHELL', '/bin/sh')
            self.clusterScript += "ssh $1 << EOF\n"
            ##Turn off comp's screen saver
            self.clusterScript += "/usr/X11R6/bin/xset -display :0.0 -dpms s "+\
                                  "reset s off\n"
            self.WriteToClusterScript("PYTHONPATH")
        elif windows:
            self.clusterScript = ""
            self.clusterScript += "@ECHO OFF\n"
            workingDrive = "%s:" %self.settings["Directory"].split(':')[0]
            self.clusterScript += "net use %s \\\\samba.vrac.iastate.edu\\home\\users\\biv\n" %workingDrive ##TESTER
            self.WriteToClusterScript("PYTHONPATH")
        else:
            self.clusterScript = "ERROR: Unsupported OS type."
        return
            

    def WriteClusterScriptPost(self):
        """Writes the cluster script section after the environment setting."""
        if unix:
            command = "%s &" %(string.join(self.XplorerCall()))
            self.clusterScript+='cd "%s"\n' %self.settings["Directory"]
            self.clusterScript += "%s\n" %(command)
            self.clusterScript += "EOF\n"
        elif windows:
            commandList = self.XplorerCall()
            command = ""
            for word in commandList:
                command += "%s " %str(word)
            command += "\n"
            workingDir = os.getenv("VE_WORKING_DIR","None")
            workingDrive = "%s:" %workingDir.split(':')[0]
            self.clusterScript += "%s\n" %workingDrive
            self.clusterScript += 'cd "%s"\n' %workingDir
            self.clusterScript += "%s\n" %(command)
        else:
            self.clusterScript += "ERROR: OS not supported."

    def ExecuteClusterScript(self, nodeName):
        if windows:
            print "Executing %s!" %nodeName
            ##Do a regular call if the initial machine's the node.
            if gethostname() == nodeName.split('.')[0]:
                print "It is this computer!"
                subprocess.Popen(self.XplorerCall())
                return
            ##Else call the script on the other computer in psexec.
            self.clusterCall[1] = "\\\\%s" %nodeName
            ##Sets the psexec window to stay up after it quits (Debug mode).
            if self.settings["Debug"]:
                subprocess.Popen(["cmd", "/k"] + self.clusterCall)
            else:
                subprocess.Popen(self.clusterCall)
        else:
            print "Error!"


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
        VEXMASTER
        VPR_DEBUG_ENABLE
        VPR_DEBUG_NFY_LEVEL
        OSGNOTIFYLEVEL

        Variables overwritten (when not in dev mode):
        VE_SUITE_HOME
        VE_INSTALL_DIR
        VE_DEPS_DIR
        VE_WORKING_DIR
        PHSHAREDSIZE
        NO_PERF_PLUGIN
        NO_RTRC_PLUGIN
        PFNFYLEVEL
        JCCL_BASE_DIR
        JCCL_DEFINITION_PATH
        VJ_CFG_PATH
        NSPR_ROOT
        SNX_BASE_DIR
        VJ_BASE_DIR
        VJ_DEPS_DIR

        Variables appended:
        PYTHON_PATH (Windows systems only)
        PATH
        LD_LIBRARY_PATH or LD_LIBRARYN32_PATH (Unix systems only)"""
        ##Set where VE-Suite's installed
        self.EnvFill("VE_SUITE_HOME", VELAUNCHER_DIR)
        self.EnvFill("VE_INSTALL_DIR", os.getenv("VE_SUITE_HOME"))
        ##Set where VE-Suite pre-complied dependencies are installed
        ##NOTE: Receives this from the launcher.
        self.EnvFill("VE_DEPS_DIR", str(self.settings["DependenciesDir"]))
        ##Gets working directory
        ##NOTE: Receives this from the launcher.
        self.EnvFill("VE_WORKING_DIR", str(self.settings["Directory"]))
        ##vrJuggler  
        ##These are setup for using VE-Suite dependency install's location
        ##change only if you are using your own build
        self.EnvFill("VJ_BASE_DIR", os.path.join(os.getenv("VE_DEPS_DIR"),
                                                 JUGGLER_FOLDER))
        self.EnvFill("VJ_DEPS_DIR", os.path.join(os.getenv("VE_DEPS_DIR"),
                                                 JUGGLER_FOLDER))

        ##Set TAO variables
        self.EnvFill("TAO_MACHINE", str(self.settings["TaoMachine"]), True)
        self.EnvFill("TAO_PORT", str(self.settings["TaoPort"]), True)

        ##Set CFDHOSTNAME
        self.EnvFill("CFDHOSTTYPE", CFD_HOST_TYPE)

        self.EnvFill("PHSHAREDSIZE", "534773700")

        ##Juggler debug output level
        if self.settings["VPRDebug"] < 0:
            self.EnvFill("VPR_DEBUG_ENABLE", "0", overwrite = True)
        else:
            self.EnvFill("VPR_DEBUG_ENABLE", "1", overwrite = True)
        self.EnvFill("VPR_DEBUG_NFY_LEVEL", str(self.settings["VPRDebug"]),
                     overwrite = True)
        if self.settings["OSGNotifyLevel"]:
            self.EnvFill("OSGNOTIFYLEVEL",
                         str(self.settings["OSGNotifyLevel"]),
                         overwrite = True)        
        self.EnvFill("NO_PERF_PLUGIN", "TRUE")
        self.EnvFill("NO_RTRC_PLUGIN", "TRUE")
        self.EnvFill("PFNFYLEVEL", "0")

        ##Juggler dependencies
        ##These are currently set relative to VE-Suite's install
        vjBaseDir = os.getenv("VJ_BASE_DIR")
        self.EnvFill("JCCL_BASE_DIR", vjBaseDir)
        self.EnvFill("JCCL_DEFINITION_PATH", os.path.join(vjBaseDir,
                                                          "definitions"))
        self.EnvFill("VJ_CFG_PATH", os.path.join(vjBaseDir, "definitions"))
        self.EnvFill("NSPR_ROOT", vjBaseDir)
        self.EnvFill("SNX_BASE_DIR", vjBaseDir)

        ##Set VexMaster
        ##Take the partially-qualified name if
        ##clusterMaster is a fully-qualified name.
        if self.settings["ClusterMaster"]:
            self.EnvFill("VEXMASTER",
                         str(self.settings["ClusterMaster"]).split('.')[0],
                         True)
        ##Python build environment variables
        if windows:
            os.environ["PYTHONPATH"] = os.path.join(os.getenv("VJ_DEPS_DIR"),
                                                    "lib", "python")
        elif unix:
            if os.getenv("OSG_HOME", "None") != "None":
                os.environ["PATH"] = os.path.join(str(os.getenv("OSG_HOME")),
                                                  "share", "OpenSceneGraph",
                                                  "bin") + ":" + \
                                     str(os.getenv("PATH"))


        ##Update PATH (and the Library Path for Unix)
        if windows:
            pathList = [os.path.join(str(os.getenv("VJ_DEPS_DIR")), "bin"),
                        os.path.join(str(os.getenv("VJ_DEPS_DIR")), "lib"),
                        os.path.join(str(os.getenv("VJ_BASE_DIR")), "lib"),
                        os.path.join(str(os.getenv("VE_INSTALL_DIR")), "bin"),
                        os.path.join(str(os.getenv("VE_DEPS_DIR")), "bin"),
                        os.path.join(VELAUNCHER_DIR, "bin")]
            if self.settings["BuilderDir"] != None:
                pathList[:0] = [os.path.join(str(self.settings["BuilderDir"]),
                                             "bin")]
            ##TEST to append 64-bit libraries:
            if architecture()[0] == "64bit":
                pathList[:0]=[os.path.join(str(os.getenv("VJ_BASE_DIR")), "lib64")]
            self.EnvAppend("PATH", pathList, ';')
        elif unix:
            ##Set name of library path
            libraryPath = "LD_LIBRARY_PATH"
            lib = "lib"
            ##Update the library path
            libList= [os.path.join(str(os.getenv("VE_DEPS_DIR")), "bin"),
                      os.path.join(str(os.getenv("VE_INSTALL_DIR")), "bin"),
                      os.path.join(str(os.getenv("VJ_BASE_DIR")), lib)]
            ##TEST to append 64-bit libraries:
            if architecture()[0] == "64bit":
                libList[:0]=[os.path.join(str(os.getenv("VJ_BASE_DIR")), "lib64")]
            self.EnvAppend(libraryPath, libList, ':')
            ##Update the path
            pathList= [os.path.join(str(os.getenv("VE_INSTALL_DIR")), "bin"),
                       os.path.join(str(os.getenv("VE_DEPS_DIR")), "bin"),
                       os.path.join(str(os.getenv("VJ_BASE_DIR")), "bin")]
            if self.settings["BuilderDir"] != None:
                pathList[:0] = [os.path.join(str(self.settings["BuilderDir"]),
                                             "bin")]
            self.EnvAppend("PATH", pathList, ':')


    def EnvAppend(self, var, appendages, sep):
        """Appends appendages (list) to var, using sep to separate them."""
        if not self.settings["DevMode"]:
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
        if self.settings["Debug"]:
            print "%s: %s" %(var, os.getenv(var))


    def EnvFill(self, var, default, overwrite = False):
        """Overwrites env var in normal mode, ensures it's filled in dev mode.

        Does not overwrite a filled variable in devMode.
        Overwrites a filled variable in normal mode.
        If overwrite == True, overwrites the variable in both modes."""
        if self.settings["DevMode"] and not overwrite:
            os.environ[var] = os.getenv(var, default)
        else:
            os.environ[var] = default
        ##Put var in clusterScript
        self.WriteToClusterScript(var)
        if self.settings["Debug"]:
            print "%s: %s" %(var, os.getenv(var))

    def WriteToClusterScript(self, var):
        """Writes an environmental setting to clusterScript.

        Exact function determined by cluster's default shell."""
        if not self.settings["Cluster"]:
            return
        if unix:
            ##Choose export command based on shell type.
            shellName = os.getenv('SHELL', 'None')
            if shellName[-3:] != 'csh':
                self.clusterScript += 'export %s="%s"\n' %(var, os.getenv(var))
            else:
                self.clusterScript += 'setenv %s "%s"\n' %(var, os.getenv(var))
        elif windows:
            self.clusterScript += 'set %s=%s\n' %(var, os.getenv(var))

