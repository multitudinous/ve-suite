import os
from wx import DisplaySize
from time import sleep ##Used for delays in launch
from platform import architecture ##Used to test if it's 32/64-bit
from velBase import *

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
    def __init__(self, launcherWindow = None,
                 workingDir = DIRECTORY_DEFAULT,
                 runName = False, runConductor = False,
                 runXplorer = False, typeXplorer = 0,
                 jconf = DEFAULT_JCONF,
                 taoMachine = DEFAULT_TAO_MACHINE,
                 taoPort = DEFAULT_TAO_MACHINE,
                 desktopMode = False,
                 dependenciesDir = None, cluster = None, master = None,
                 shell = False, builderDir = None, devMode = False):
        """Sets environmental vars and calls OS-specific launch code.

        Keyword arguments:
        launcherWindow -- The caller. Used to close it after the call.
        workingDir, taoMachine, taoPort -- Used for environmental vars.
        runName, runConductor, runXplorer,
        typeXplorer, jconf, desktopMode -- Used for launch code.
        dependenciesDir -- Optional, used for environmental vars.
        cluster -- Optional, unused at the moment.
        master -- Optional, used for sending VEXMASTER to slave nodes.
        shell -- Starts up a VE-Builder shell.
        builderDir -- Sets a path to the builderDir/bin."""
        ##Set devMode
        self.devMode = devMode
        ##Set self.cluster to True if there's cluster functionality.
        ##If so, begin building self.clusterScript
        ##Used in EnvSetup and Windows/Unix.
        if runXplorer and typeXplorer == 2 and cluster != None and \
           CLUSTER_ENABLED:
            self.cluster = True
            ##Set up beginning of clusterScript for env setting.
            self.clusterScript = "#!/bin/csh\n"
            self.clusterScript += "ssh $1 << EOF\n"
            self.WriteToClusterScript("PYTHONPATH")
        else:
            self.cluster = False
        ##Set the environmental variables
        self.EnvSetup(dependenciesDir, workingDir, taoMachine, taoPort,
                      master, builderDir)
        ##Use the user's defined directory as Current Working Dir
        if not shell:
            os.chdir(os.getenv("VE_WORKING_DIR"))
        ##Checks the OS and routes the launcher to the proper subfunction
        ##NOTE: Code out separate Setups, code in the combined Setup
        if shell: ##Shell is activated after destroy VE-Launcher.
            return
        elif windows:
            self.Windows(runName, runConductor, runXplorer,
                         typeXplorer, jconf, desktopMode)
        elif unix:
            self.Unix(runName, runConductor, runXplorer,
                      typeXplorer, jconf, desktopMode, cluster, master)
        else:
            print "ERROR: VE-Suite-Launcher doesn't support this OS."


    def Windows(self, runName = False, runConductor = False,
                runXplorer = False, typeXplorer = 0, jconf = DEFAULT_JCONF,
                desktopMode = False):
        """Launches the chosen programs under an Unix OS.

        Keyword arguments:
        runName, runConductor, runXplorer -- Run NameServer/Conductor/Xplorer?
        typeXplorer -- Which Xplorer program to run.
        jconf -- Which .jconf file to use for Xplorer's settings.
        desktopMode -- Run in Desktop mode."""
        ##Name Server section
        ##NOTE: Name Server starts up in Launcher's window.
        ##Closing the Launcher's DOS window closes Name Server as well.
        ##Closing the Launcher doesn't close the Launcher's DOS window while
        ##Name Server's running, though.
        ##Do we need to give Name Server its own window?
        if runName:
            self.KillNameserver()
            sleep(1)
            print "Starting Name Server."
            os.system("start /B Naming_Service.exe -ORBEndPoint" +
                      " iiop://%TAO_MACHINE%:%TAO_PORT%")
            sleep(5)
            os.system("start /B WinServerd.exe -ORBInitRef" +
                      " NameService=" +
                      "corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService" +
                      " -ORBDottedDecimalAddresses 1")
        ##Conductor section
        if runConductor:
            print "Starting Conductor."
            ##Append argument if desktop mode selected
            if desktopMode:
                desktop = " -VESDesktop"
            else:
                desktop = ""
            os.system('start "%s" /B' % (CONDUCTOR_SHELL_NAME) +
                      " WinClientd.exe -ORBInitRef" +
                      " NameService=" +
                      "corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService" +
                      " -ORBDottedDecimalAddresses 1" + desktop)
        ##Xplorer section
        if runXplorer:
            print "Starting Xplorer."
            ##Append argument if desktop mode selected
            if desktopMode:
                w, h = DisplaySize()
                desktop = " -VESDesktop %s %s" % (w, h)
            else:
                desktop = ""
            ##Set Xplorer's type
            if typeXplorer == 0: ##OSG selection
                executable = "project_tao_osg_d.exe"
            elif typeXplorer == 1: ##OSG VEP selection
                executable = "project_tao_osg_vep_d.exe"
            elif typeXplorer == 2: ##OSG VEPC selection
                executable = "project_tao_osg_vep_cluster_d.exe"
            elif typeXplorer == 3: ##PF selection
                executable = "project_taod.exe"
            else:
                executable = "ERROR"
            ##Xplorer's start call
            os.system('start "%s" /B' %(XPLORER_SHELL_NAME) +
                      ' %s "%s"' %(executable, jconf) +
                      " -ORBInitRef" +
                      " NameService=" +
                      "corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService" +
                      " -ORBDottedDecimalAddresses 1" + desktop)
        print "Finished sending launch commands."
        return

    def Unix(self, runName = False, runConductor = False, runXplorer = False,
             typeXplorer = 0, jconf = DEFAULT_JCONF,
             desktopMode = False, cluster = None, clusterMaster = None):
        """Launches the chosen programs under an Unix OS.

        Keyword arguments:
        runName, runConductor, runXplorer -- Run NameServer/Conductor/Xplorer?
        typeXplorer -- Which Xplorer program to run.
        jconf -- Which .jconf file to use for Xplorer's settings.
        desktopMode -- Run in Desktop mode.
        cluster -- List of slaves in the cluster.
        clusterMaster -- The master of the cluster."""
        ##Name Server section
        if runName:
            self.KillNameserver()
            sleep(1)
            print "Starting Name Server."
            os.system("Naming_Service -ORBEndPoint" +
                      " iiop://${TAO_MACHINE}:${TAO_PORT} &")
            sleep(5)
            os.system("Exe_server -ORBInitRef NameService=" +
                      "corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService" +
                      " -ORBDottedDecimalAddresses 1 &")
        ##Conductor section
        if runConductor:
            print "Starting Conductor."
            ##Append argument if desktop mode selected
            if desktopMode:
                desktop = "-VESDesktop"
            else:
                desktop = ""
            os.system("WinClient -ORBInitRef NameService=corbaloc:iiop:" +
                      "${TAO_MACHINE}:${TAO_PORT}/" +
                      "NameService %s &" % (desktop))
        ##Cluster mode
        if self.cluster:
            print "Starting Xplorer on the cluster."
            ##Finish building cluster script
            launcherDir = str(os.getenv("VE_INSTALL_DIR"))
            xplorerType = XPLORER_TYPE_LIST[typeXplorer]
            taoMachine = str(os.getenv("TAO_MACHINE"))
            taoPort = str(os.getenv("TAO_PORT"))
            workDir = str(os.getenv("VE_WORKING_DIR"))
            depsDir = str(os.getenv("VE_DEPS_DIR"))
            master = str(os.getenv("VEXMASTER"))
            command = self.XplorerCall(typeXplorer,
                                       jconf, desktopMode)
##            command = 'python velauncher.py -x %s' %(xplorerType) + \
##                      ' -j "%s" -t %s -p %s' %(jconf, taoMachine, taoPort) + \
##                      ' -w %s -e %s -m %s' %(workDir, depsDir, clusterMaster)
##            self.clusterScript += "cd %s\n" %(VELAUNCHER_DIR)
            self.clusterScript += "%s\n" %(command)
            self.clusterScript += "EOF\n"
            clusterFileName = "cluster.tsh"
            clusterFilePath = os.path.join(VELAUNCHER_DIR, clusterFileName)
            ##Write cluster script
            sourceFile = file(clusterFilePath, 'w')
            sourceFile.write(self.clusterScript)
            sourceFile.close()
            ##Master call
            print "***MASTER CALL: %s***" %(clusterMaster) ##TESTER
            os.system("source %s %s &" %(clusterFilePath, clusterMaster))
            sleep(MASTER_WAIT)
            ##Slave calls
            for comp in cluster:
                print "***CLUSTER CALL: %s***" %(comp) ##TESTER
                os.system("source %s %s &" %(clusterFilePath, comp))
                sleep(SLAVE_WAIT)
        ##Xplorer section
        elif runXplorer:
            print "Starting Xplorer."
            os.system(self.XplorerCall(typeXplorer, jconf, desktopMode))
##            ##Append argument if desktop mode selected
##            desktop = ""
##            if desktopMode:
##                w, h = DisplaySize()
##                desktop = "-VESDesktop %s %s" % (w, h)
##            ##Set Xplorer's type
##            if typeXplorer == 0: ##OSG selection
##                executable = "project_tao_osg"
##            elif typeXplorer == 1: ##OSG VEP selection
##                executable = "project_tao_osg_vep"
##            elif typeXplorer == 2: ##OSG VEPC selection
##                executable = "project_tao_osg_vep_cluster"
##            ##Xplorer's call
##            os.system("%s -ORBInitRef NameService=" %(executable) +
##                      "corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService " +
##                      '"%s" %s &' %(jconf, desktop))
        print "Finished sending launch commands."
        return

    def XplorerCall(self, typeXplorer, jconf, desktopMode):
        if windows:
            return
        elif unix:
            ##Append argument if desktop mode selected
            desktop = ""
            if desktopMode:
                w, h = DisplaySize()
                desktop = "-VESDesktop %s %s" % (w, h)
            ##Set Xplorer's type
            if typeXplorer == 0: ##OSG selection
                executable = "project_tao_osg"
            elif typeXplorer == 1: ##OSG VEP selection
                executable = "project_tao_osg_vep"
            elif typeXplorer == 2: ##OSG VEPC selection
                executable = "project_tao_osg_vep_cluster"
            ##Construct the call
            xplorerCall = "%s -ORBInitRef NameService=" %(executable) + \
                          "corbaloc:iiop:${TAO_MACHINE}:" + \
                          "${TAO_PORT}/NameService " + \
                          '"%s" %s &' %(jconf, desktop)
            ##Return the call
            return xplorerCall
        else:
            print "Strange occurance in velLaunchCode.Launch.XplorerCall."

    def KillNameserver(self):
        """Kills any Name Servers running on this computer."""
        print "Killing any previous name servers."
        if windows:
            os.system("tskill Naming_Service")
            os.system("tskill WinServerd")
        elif unix:
            os.system("killall Naming_Service Exe_server")
        print "Previous name servers killed."

    def EnvSetup(self, dependenciesDir, workingDir, taoMachine, taoPort,
                 clusterMaster = None, builderDir = None):
        """Sets up the environmental variables to launch VE-Suite's programs.

        Only takes care of basic variables. Coders with custom builds can set
        advanced variables by creating a batch/shell file to set the extra
        variables, then execute the launcher in --dev mode as its last command.
        The environmental settings will carry over.

        Variables overwritten by this class:
        CFDHOSTTYPE (removes parantheses from CFDHOSTTYPE)

        Variables overwritten (when not in dev mode):
        VE_INSTALL_DIR
        VE_DEPS_DIR
        VE_WORKING_DIR
        TAO_MACHINE
        TAO_PORT
        PHSHAREDSIZE
        VPR_DEBUG_ENABLE
        VPR_DEBUG_NFY_LEVEL
        NO_PERF_PLUGIN
        NO_RTRC_PLUGIN
        PFNFYLEVEL
        JCCL_BASE_DIR
        JCCL_DEFINITION_PATH
        VJ_CFG_PATH
        NSPR_ROOT
        SNX_BASE_DIR
        VEXMASTER
        VJ_BASE_DIR
        VJ_DEPS_DIR

        Variables appended:
        PYTHON_PATH (Windows systems only)
        PATH
        LD_LIBRARY_PATH or LD_LIBRARYN32_PATH (Unix systems only)"""
        ##Set where VE-Suite's installed
        if self.devMode:
             self.EnvFill("VE_INSTALL_DIR", os.getenv("VE_SUITE_HOME"))
        else:
             self.EnvFill("VE_INSTALL_DIR", os.getcwd())
        ##Set where VE-Suite pre-complied dependencies are installed
        ##NOTE: Receives this from the launcher.
        self.EnvFill("VE_DEPS_DIR", dependenciesDir)
        ##Gets working directory
        ##NOTE: Receives this from the launcher.
        self.EnvFill("VE_WORKING_DIR", workingDir)
        ##vrJuggler  
        ##These are setup for using VE-Suite dependency install's location
        ##change only if you are using your own build
        self.EnvFill("VJ_BASE_DIR", os.path.join(os.getenv("VE_DEPS_DIR"),
                                                 JUGGLER_FOLDER))
        self.EnvFill("VJ_DEPS_DIR", os.path.join(os.getenv("VE_DEPS_DIR"),
                                                 JUGGLER_FOLDER))
        ##Cluster apps & user-built dependencies were commented out,
        ##therefore they weren't added. Check old setup.bat for more details.
        ##NOTE: Since they were only used for custom builds, setting them
        ##was moved to an external batch/shell file which calls the Launcher
        ##on its last line.

        ##Set TAO variables
        self.EnvFill("TAO_MACHINE", taoMachine)
        self.EnvFill("TAO_PORT", str(taoPort))

        ##Set CFDHOSTNAME
        if windows:
            self.EnvFill("CFDHOSTTYPE", "WIN32")
        elif unix:
            if (os.path.exists("/etc/redhat-release")):
                piped = os.popen("""cat /etc/redhat-release """ +
                                 """| awk -F" " '{print $1}'""", 'r')
                firstWord = piped.read()[:-1]
                ##NOTE: [:-1] is to remove the line break from the read()
                piped.close()
                if firstWord == "Red":
                    piped = os.popen("""cat /etc/redhat-release """ +
                                     """| awk -F" " '{print $3}'""", 'r')
                    thirdWord = piped.read()[:-1]
                    piped.close()
                    if thirdWord == "Enterprise":
                        ##Extract words from file to create similar to RHEL_3
                        piped= os.popen("""cat /etc/redhat-release """ +
                                        """| awk -F" " '{print "RHEL_" $7}'""",
                                        'r')
                        self.EnvFill("CFDHOSTTYPE", piped.read()[:-1])
                        piped.close()
                    else:
                        ##Extract words from file to create
                        ##something like RedHat_8.0
                        piped = os.popen("""cat /etc/redhat-release """ +
                                         """| awk -F" " '""" +
                                         """{print $1 $2 "_" $5}'""",
                                         'r')
                        self.EnvFill("CFDHOSTTYPE", piped.read()[:-1])
                        piped.close()
                elif firstWord == "Fedora":
                    ##Extract words from file to create something like Fedora_1
                    piped= os.popen("""cat /etc/redhat-release """ +
                                    """| awk -F" " '{print $1 "_" $4}'""", 'r')
                    self.EnvFill("CFDHOSTTYPE", piped.read()[:-1])
                    piped.close()
                else:
                    ##NOTE: If the program couldn't identify this type of
                    ##Redhat, just use uname.
                    piped = os.popen("uname")
                    self.EnvFill("CFDHOSTTYPE", piped.read()[:-1])
                    piped.close()
            elif os.path.exists("/etc/SuSE-release"):
                ##Extract words from file to create
                ##something like SuSE_9.2_x86-64
                piped = os.popen("""head -1 /etc/SuSE-release """ +
                                 """| awk -F" " '{print $1 "_" $3 "_" $4}'""",
                                 'r')
                self.EnvFill("CFDHOSTTYPE", piped.read()[:-1])
                piped.close()
            else:
                piped = os.popen("uname")
                self.EnvFill("CFDHOSTTYPE", piped.read()[:-1])
                piped.close()
            ##If CFDHOSTTYPE has parentheses, remove them.
            piped = os.popen("""echo \"$CFDHOSTTYPE\" """ +
                             """| sed -e 's/(//g' | sed -e 's/)//g' """ + 
                             """| sed -e 's/"//g'""", 'r')
            os.environ["CFDHOSTTYPE"] = piped.read()[:-1]
            piped.close()

        self.EnvFill("PHSHAREDSIZE", "534773700")

        ##Juggler debug output level
        self.EnvFill("VPR_DEBUG_ENABLE", "0")
        self.EnvFill("VPR_DEBUG_NFY_LEVEL", "1")
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
        if clusterMaster != None:
            self.EnvFill("VEXMASTER", clusterMaster.split('.')[0])
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
                        os.path.join(os.getcwd(), "bin")]
            if builderDir != None:
                pathList[:0] = [os.path.join(builderDir, "bin")]
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
            if builderDir != None:
                pathList[:0] = [os.path.join(builderDir, "bin")]
            self.EnvAppend("PATH", pathList, ':')


    def EnvAppend(self, var, appendages, sep):
        """Appends appendages (list) to var, using sep to separate them."""
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
##        print var + ": " + os.getenv(var) ##TESTER

    def EnvFill(self, var, default):
        """Overwrites environmental var in normal mode, fills it in dev mode.

        Does not overwrite a filled variable in devMode.
        Overwrites a filled variable in normal mode."""
        if self.devMode:
            os.environ[var] = os.getenv(var, default)
        else:
            os.environ[var] = default
        ##Put var in clusterScript
        self.WriteToClusterScript(var)
##        print var + ": " + os.getenv(var) ##TESTER

    def WriteToClusterScript(self, var):
        """Writes an environmental setting to clusterScript.

        Exact function determined by cluster's default shell."""
        if not self.cluster:
            return
        if os.getenv('SHELL', 'None')[-4:] == 'bash':
            self.clusterScript += "export %s=%s\n" %(var, os.getenv(var))
        else:
            self.clusterScript += "setenv %s %s\n" %(var, os.getenv(var))
