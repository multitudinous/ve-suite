#!/usr/bin/env python
"""Takes VE-Suite settings input and launches chosen VE-Suite programs.

--v1.0 coded by Jeff Groves
This module creates a launcher window so the user can choose which
parts of VE-Suite to launch:
-Name Server
-Conductor
-Xplorer (user also chooses format and configuration)
The user can also select VE-Suite's working directory.

When the user has decided the settings and hits the Launch button,
the module sets up the system's environmental variables and executes
the chosen programs. The launcher automatically quits after Launch.

The launcher is made for standard builds of VE-Suite. To launch a custom build
with it, create a batch/shell file to set the extra environmental variables,
executing the launcher on its last command.
"""
import os ##Used for setting environmental variables, running programs
from time import sleep ##Used for delays in launch
import sys ##Gets command line arguments
from platform import architecture ##Used to test if it's 32/64-bit
import getopt ##Cleans up command line arguments
import wx ##Used for GUI

UNIX_SHELL = os.getenv("SHELL", "/bin/sh") ##Shell program for the Shell mode
##Cluster features
CLUSTER_ENABLED = True
MASTER_WAIT = 10 ##Seconds to wait after starting Master to start Slaves
SLAVE_WAIT = 3 ##Seconds to wait between each Slave start
##Miscellaneous values for launcher's UI
XPLORER_SHELL_NAME = "VE-Xplorer Shell"
CONDUCTOR_SHELL_NAME = "VE-Conductor Shell"
BUILDER_SHELL_NAME = "VE-Builder Shell"
FIXED = False ##Constant var used in MODE_DICT
devMode = False ##Run VE-Suite in dev mode? Turned to True if --dev passed.
##File/Folder settings.
##Note: The HOME_BASE variable will be the one the installer needs to modify.
JUGGLER_FOLDER = "vrJuggler2.0.1"
VELAUNCHER_DIR = os.getcwd()
DIRECTORY_DEFAULT = os.path.join(os.getcwd(), "exampleDatasets")
LOGO_LOCATION = os.path.join(os.getcwd(), "installerImages", "ve_logo.xpm")
CONFIG_FILE = "VE-Suite-Launcher"
DEFAULT_CONFIG = "previous"
RADIO_XPLORER_LIST = ["OpenSceneGraph", "OSG Patented",
                      "OSG Patented Cluster"]
XPLORER_TYPE_LIST = ["OSG", "OSG-VEP", "OSG-VEPC"]
MODE_LIST = ["Desktop", "Tablet", "Computation", "Visualization",
             "Shell", "Custom"]
MODE_DICT = {"Desktop": {"conductor": [FIXED, True],
                         "nameServer": [FIXED, True],
                         "xplorer": [FIXED, True],
                         "xplorerType": [FIXED, 0],
                         "desktop": [FIXED, True],
                         "jconf": [FIXED, "Desktop",
                                   os.path.join(os.getenv("VE_INSTALL_DIR",
                                                          os.getcwd()),
                                                "stereo_desktop",
                                                "desktop.jconf")],
                         "taoMachine": [FIXED, "localhost"]},
             "Tablet": {"conductor": [FIXED, True],
                        "nameServer": [FIXED, False],
                        "xplorer": [FIXED, False],
                        "desktop": [FIXED, False]},
             "Computation": {"conductor": [FIXED, False],
                             "nameServer": [FIXED, True],
                             "xplorer": [FIXED, False]},
             "Visualization": {"conductor": [FIXED, False],
                               "nameServer": [FIXED, False],
                               "xplorer": [FIXED, True],
                               "desktop": [FIXED, False]},
             "Shell": {"conductor": [FIXED, False],
                       "nameServer": [FIXED, False],
                       "xplorer": [FIXED, False],
                       "shell": [FIXED, True]},
             "Custom": {}}
JCONF_CONFIG = "JconfList"
CLUSTER_CONFIG = "Cluster"
DEFAULT_JCONF = os.path.join(os.getcwd(), "stereo_desktop", "desktop.jconf")
##Values for launcher's GUI layout
INITIAL_WINDOW_SIZE = (500, -1)
INITIAL_JCONF_WINDOW_SIZE = (250, 250)
##BACKGROUND_COLOR = wx.Colour(236, 233, 216)
BACKGROUND_COLOR = wx.Colour(200, 200, 200)
JCONF_LIST_DISPLAY_MIN_SIZE = (100, 50)
TOP_SPACE = (75, 75)
BORDER = 5
VERTICAL_SPACE = (-1, BORDER)
HORIZONTAL_SPACE = (BORDER, -1)
LEFT_MARGIN = HORIZONTAL_SPACE
NULL_SPACE = (0, 0)
KILL_WINDOW_SIZE = (200, 100)
##Set up the system ID
windows = (os.name == "nt")
unix = (os.name == "posix")
    
##Set up the config
config = wx.Config(CONFIG_FILE)
config.SetPath(DEFAULT_CONFIG)
    
def Style(window):
    """The uniform style of each window in VE Launcher."""
    ##Set the background color.
    window.SetBackgroundColour(BACKGROUND_COLOR)
    return

def usage():
    """Prints a list of acceptable arguments for command line velauncher.py."""
    print """
LEGAL ARGUMENTS FOR VELAUNCHER.PY
<none>: Start the velauncher GUI.
--dev: Start the velauncher GUI in developer mode. Doesn't work with any
other arguments.

-c, --conductor: Launch VE Conductor.
-n, --nameserver: Launch VE NameServer.
-x <xplorer type>, --xplorer=<xplorer type>: Launch VE Xplorer in
<xplorer type> mode. You can choose OSG, OSG-VEP, or OSG-VEPC.

-s, --shell: Launches a subshell with the VE-Suite environmental
variables set. Overrides -c, -n, and -x.
-b <builder dir>, --builder=<builder dir>: Launches a subshell
with the VE-Suite environmental variables set, including a path
to the VE-Builder directory. Overrides -c, -n, -x and -s.

-k, --desktop: Set VE Conductor and VE Xplorer to Desktop mode.
-j <filepath>, --jconf=<filepath>: Use <filepath> as VE Xplorer's
Juggler configuration.
-w <dir>, --dir=<dir>: Set the Working directory to <dir>.

-t <name>, --taomachine=<name>: Set TAOMACHINE to <name>.
-p <port>, --port=<port>: Set TAOPORT to <port>.
-e <dir>, --dep=<dir>: Set the Dependencies directory to <dir>.
-m <name>, --master=<name>: Set VEXMASTER to <name>."""
    return


class CommandLaunch:
    """Launches VE Suite using arguments from the command line."""
    def __init__(self, opts):
        ##Set up vars.
        self.conductor = False
        self.nameServer = False
        self.xplorer = False
        self.xplorerType = None
        self.desktop = False
        self.taoMachine = None
        self.taoPort = None
        self.mode = None
        self.depDir = None
        self.workDir = None
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
        if self.depDir == None:
            self.depDir = config.Read("DependenciesDir", "None")
        if self.workDir == None:
            self.workDir = config.Read("Directory", DIRECTORY_DEFAULT)
        ##Set default choices if JCONF_CONFIG doesn't exist,
        ##but DependenciesDir does.
        if self.jconf == None:
            if config.HasGroup(JCONF_CONFIG):
                jconfList = JconfList()
                self.jconf= jconfList.GetPath(config.Read("JconfSelection"))
            ##Set default choice if JCONF_CONFIG doesn't exist,
            ##but DependenciesDir does.
            elif config.Read("DependenciesDir", ":::") != ":::":
                self.jconf = DEFAULT_JCONF
            ##If neither exists, bring up an error.
            ##NOTE: Should never be reached.
            else:
                print "ERROR: No Xplorer configuration found and no" + \
                      "Dependencies directory to find the default from."
        ##Set Tao Machine & Port.
        if self.taoMachine == None:
            self.taoMachine = config.Read("TaoMachine", "localhost")
        if self.taoPort == None:
            self.taoPort = config.Read("TaoPort", "1239")
        ##Set Xplorer Type
        if self.xplorerType == None:
            data = config.ReadInt("XplorerType", 0)
            if data >= 0 and data < len(RADIO_XPLORER_LIST):
                self.xplorerType = data
            else:
                self.xplorerType = 0
        ##Launch
        Launch(None, self.workDir,
               self.nameServer, self.conductor, self.xplorer, self.xplorerType,
               self.jconf,
               self.taoMachine, self.taoPort,
               self.desktop,
               self.depDir, master = self.clusterMaster,
               shell = self.shell, builderDir = self.builderDir)
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


class LauncherWindow(wx.Dialog):
    """Manages the launcher's window and the use of data from it.

    LauncherWindow manages the launcher's GUI, saving/loading the
    configuration settings, and the commands to launch the VE-Suite programs.

    Order of steps:
        __init__ & LoadConfig(previous)
        DependenciesCheck & DependenciesGet
        *User selects settings:*
            User chooses directory.
            User choose tao machine & port.
            User chooses mode & mode settings. 
        *If Launch button pressed:*
            Launch
            OnClose & SaveConfig(previous)
            quit
        *Else if window's closed:*
            OnClose & SaveConfig(previous)
            quit

    Functions:
        __init__(parent, ID, title)
        DependenciesGet
        DependenciesCheck
        ModeChanged(event)
        UpdateData
        UpdateDisplay
        GetSelectedJconf
        ChangeMode(event)
        ChooseDirectory(event)
        ChooseSaveConfig(event)
        ChooseLoadConfig(event)
        DeleteConfig(event)
        SaveConfig(config, name)
        LoadConfig(config, name)
        Settings(event)
        Launch(event)
        OnClose(event)        
    """
    def __init__(self, parent, ID, title):
        """Builds the launcher's window and loads the last configuration."""
        wx.Dialog.__init__(self, parent, -1, title,
                           style = wx.DEFAULT_FRAME_STYLE &
                           ~ (wx.RESIZE_BORDER | wx.RESIZE_BOX |
                           wx.MAXIMIZE_BOX) | wx.TAB_TRAVERSAL)
        ##NOTE: wx.TAB_TRAVERSAL disabled until radio box tabbing works.
        ##NOTE: Menu doesn't work with dialog.

        ##Prepare data storage
        ##NOTE: jconfList is a local copy of the Jconf list stored in the
        ##program's config. Changes to jconfList are mirrored in the config.
        self.conductor = False
        self.nameServer = False
        self.xplorer = False
        self.xplorerType = 0
        self.desktop = False
        self.jconfList = None
        self.jconfSelection = None
        self.clusterDict = None
        self.clusterMaster = None
        self.taoMachine = ""
        self.taoPort = ""
        self.dependencies = None
        self.builderDir = None
        self.shell = False

        ##Prepare the logo.
        bmLogo = wx.Bitmap(LOGO_LOCATION, wx.BITMAP_TYPE_XPM)
        sbmLogo = wx.StaticBitmap(self, -1, bmLogo)

        ##Build Dependencies Change button.
        bDepChange = wx.Button(self, -1, "Change Dependencies Folder")
        bDepChange.SetToolTip(wx.ToolTip("Change the VE-Suite Dependencies" +
                                         " folder."))
        ##Build Directory text ctrl.
        self.txDirectory = wx.TextCtrl(self, -1,
                                       DIRECTORY_DEFAULT)
        self.txDirectory.SetToolTip(wx.ToolTip("The path of the" +
                                               " working directory."))
        ##Build Directory button.
        bDirectory = wx.Button(self, -1, "Choose Working Directory")
        bDirectory.SetToolTip(wx.ToolTip("Choose the working directory for" +
                                         " the programs."))
        ##Build Tao Machine text ctrl.
        self.txTaoMachine = wx.TextCtrl(self, -1)
        self.txTaoMachine.SetToolTip(wx.ToolTip("Enter the computing" +
                                                "engine's name."))
        ##Build Tao Port text ctrl.
        self.txTaoPort = wx.TextCtrl(self, -1)
        self.txTaoPort.SetToolTip(wx.ToolTip("Enter the computing" +
                                             " engine's port."))
        ##Build Mode radio box.
        self.rbMode = wx.RadioBox(self, -1, "Launch Mode",
                                  wx.DefaultPosition, wx.DefaultSize,
                                  MODE_LIST, 1, wx.RA_SPECIFY_COLS)
        self.rbMode.SetToolTip(wx.ToolTip("Choose which mode you want to" +
                                          " launch in?"))
        ##Build Mode Settings button.
        self.bCustom = wx.Button(self, -1, "Mode Settings")
        self.bCustom.SetToolTip(wx.ToolTip("View and change settings for" +
                                           " the current mode."))
        ##Build Config buttons.
        bLoadConf = wx.Button(self, -1, "Load Config")
        bLoadConf.SetToolTip(wx.ToolTip("Load a different configuration."))
        bSaveConf = wx.Button(self, -1, "Save Config")
        bSaveConf.SetToolTip(wx.ToolTip("Save this configuration."))
        bDeleteConf = wx.Button(self, -1, "Delete Config")
        bDeleteConf.SetToolTip(wx.ToolTip("Delete a configuration."))
        ##Build Launch button.
        self.bLaunch = wx.Button(self, -1, "Launch VE Suite")
        self.bLaunch.SetToolTip(wx.ToolTip("Run the programs you selected and" +
                                      " close the Launcher."))
        ##Build menu bar
        ##menuBar = wx.MenuBar()
        ##menu = wx.Menu()
        ##menu.Append(110, "Change &Dependencies")
        ##menu.Append(wx.ID_EXIT, "&Quit\tCtrl+Q")
        ##menuBar.Append(menu, "&File")
        ##self.SetMenuBar(menuBar)

        ##Set tool tip popup delay to 2 seconds.
        wx.ToolTip.SetDelay(2000)
        ##Check the dependencies.
        if not devMode:
            dependenciesDir = config.Read("DependenciesDir", ":::")
            if dependenciesDir == ":::":
                dlg = wx.MessageDialog(None,
                                       "Welcome to VE Suite!\n" +
                                       "Before you can begin, I need" +
                                       " to find the VE_Suite_Dependencies " +
                                       "directory.\n" +
                                       "Please select the Dependencies" +
                                       " directory for me.",
                                       "Welcome to VE Suite", wx.OK)
                dlg.ShowModal()
                dlg.Destroy()
                self.DependenciesChange("dead parrot sketch")
            else:
                legitDeps = self.DependenciesCheck(dependenciesDir)
                if not legitDeps:
                    self.DependenciesChange("dead parrot sketch")
            self.dependencies = config.Read("DependenciesDir", ":::")
        ##Event bindings.
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        self.Bind(wx.EVT_BUTTON, self.ChooseDirectory, bDirectory)
        self.Bind(wx.EVT_BUTTON, self.Launch, self.bLaunch)
        self.Bind(wx.EVT_BUTTON, self.Settings, self.bCustom)
        self.Bind(wx.EVT_BUTTON, self.ChooseLoadConfig, bLoadConf)
        self.Bind(wx.EVT_BUTTON, self.ChooseSaveConfig, bSaveConf)
        self.Bind(wx.EVT_BUTTON, self.DeleteConfig, bDeleteConf)
        self.Bind(wx.EVT_RADIOBOX, self.ModeChanged, self.rbMode)
        self.Bind(wx.EVT_BUTTON, self.DependenciesChange, bDepChange)
        ##Restore config values from last time.
        self.LoadConfig(DEFAULT_CONFIG)
        
        ##Layout format settings
        ##Create the overall layout box
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        ##Construct the Directory column
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(self.txDirectory, 1, wx.ALIGN_BOTTOM)
        columnSizer.AddMany([HORIZONTAL_SPACE,
                             bDirectory])
        ##Insert the Directory column.
        rowSizer.Add(wx.StaticText(self, -1, "Working Directory:"))
        rowSizer.Add(columnSizer, 0, wx.EXPAND) 
        ##Construct the Tao column.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(wx.StaticText(self, -1, "CE Name:"),
                        0, wx.ALIGN_CENTER_VERTICAL)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.txTaoMachine, 2)
        columnSizer.Add((HORIZONTAL_SPACE[0]*3, -1))
        columnSizer.Add(wx.StaticText(self, -1, "CE Port:"),
                        0, wx.ALIGN_CENTER_VERTICAL)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.txTaoPort, 1)
        ##Insert the Tao column
        rowSizer.Add(VERTICAL_SPACE)
        rowSizer.Add(columnSizer, 0, wx.EXPAND)
        ##Insert the box grid.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.AddMany([self.rbMode,
                             HORIZONTAL_SPACE])
        columnSizer.Add(self.bCustom, 0, wx.ALIGN_LEFT | wx.ALIGN_BOTTOM)
        rowSizer2 = wx.BoxSizer(wx.VERTICAL)
        rowSizer2.Add(bLoadConf, 0, wx.EXPAND)
        rowSizer2.Add(VERTICAL_SPACE)
        rowSizer2.Add(bSaveConf, 0, wx.EXPAND)
        rowSizer2.Add(VERTICAL_SPACE)
        rowSizer2.Add(bDeleteConf, 0, wx.EXPAND)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(rowSizer2, 0, wx.ALIGN_LEFT | wx.ALIGN_BOTTOM)
        rowSizer.AddMany([VERTICAL_SPACE,
                          columnSizer])
        ##Add the title graphic space
        rowSizer2 = wx.BoxSizer(wx.VERTICAL)
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(sbmLogo)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(bDepChange, 0, wx.ALIGN_LEFT | wx.ALIGN_BOTTOM)
        if devMode:
            bDepChange.Hide()
        rowSizer2.Add(columnSizer)
        rowSizer2.Add(VERTICAL_SPACE)
        rowSizer2.Add(rowSizer, 0, wx.EXPAND)
        ##Set the main sizer, add Launch button.
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer2, 0, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(self.bLaunch, 1, wx.EXPAND)
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
##        self.SetSize(INITIAL_WINDOW_SIZE)
        ##Set the background color.
        Style(self)
        ##Show the window.
        self.Show(True)
        ##Error check: Is there a /bin folder in the launcher's directory?
        ##If so, assume it's in VE Suite's folder. If not, warn the user.
        if not os.path.exists("bin") and os.getenv("VE_INSTALL_DIR") == "":
            dlg = wx.MessageDialog(self,
                                   "VE Suite's /bin directory wasn't found" +
                                   " in the Launcher's directory.\n" +
                                   "The Launcher won't run properly if" +
                                   " it isn't in VE Suite's directory.\n" +
                                   "Please make sure the Launcher" +
                                   " is in VE Suite's directory.",
                                   "Error: /bin Directory Missing", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()

    def DependenciesCheck(self, dependenciesDir):
        """Returns true if Dependencies folder checks out, false if it doesn't.

        Automatically called during __init__.
        Checks if dependenciesDir exists,
        then checks if it looks like the Dependencies directory.
        If any check fails, it returns False. Else it returns True."""
        ##Set name of the file to check in the Dependencies folder
        if os.name == "posix":
            nameServiceFile = "Naming_Service"
        elif os.name == "nt":
            nameServiceFile = "Naming_Service.exe"
        else:
            nameServiceFile = "None"
        ##Check if DependenciesDir's path exists.
        if not os.path.exists(dependenciesDir):
            dlg = wx.MessageDialog(None,
                                   "I can't find the " +
                                   "VE_Suite_Dependencies directory.\n" +
                                   "It may have been moved, renamed," +
                                   " or deleted.\n" +
                                   "Please find it for me.",
                                   "Error: Dependencies Directory" +
                                   " Not Found",
                                   wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return False
        ##Check if DependenciesDir's contents look legitimate.
        elif not os.path.exists(os.path.join(dependenciesDir,
                                "bin", nameServiceFile)):
            dlg = wx.MessageDialog(None,
                                   str(dependenciesDir) + "\n" +
                                   "doesn't look like the Dependencies " +
                                   "directory I need.\n" +
                                   "Are you sure you want to use it?",
                                   "Warning: Dependencies Directory" +
                                   " Looks Unfamiliar",
                                   wx.YES_NO | wx.NO_DEFAULT)
            response = dlg.ShowModal()
            dlg.Destroy()
            if response == wx.ID_NO:
                return False
            else:
                return True
        ##If all checks passed, return True.
        return True

    def DependenciesChange(self, event):
        """Asks user for a new DependenciesDir."""
        legitimateDependenciesDir = False
        while not legitimateDependenciesDir:
            dependenciesDir = self.DependenciesGet()
            if dependenciesDir == None:
                return
            legitimateDependenciesDir = self.DependenciesCheck(dependenciesDir)
        ##Write the new Dependencies directory to default config.
        config.Write("DependenciesDir", dependenciesDir)
        self.dependencies = dependenciesDir
        return

    def DependenciesGet(self):
        """Ask user for DependenciesDir. Called by DependenciesChange.

        Returns the directory path the user chose.
        Helper function for DependenciesCheck."""
        ##Go up a directory if it's a Unix os to get out
        ##of the VE_Suite directory.
        if config.Read("DependenciesDir", ":::") != ":::":
            searchDir = config.Read("DependenciesDir")
        elif os.name == "nt":
            searchDir = os.getcwd()
        elif os.name == "posix":
            searchDir = os.path.split(os.getcwd())[0]
        else:
            searchDir = "dead parrot sketch"
        ##User chooses the directory.
        dlg = wx.DirDialog(None,
                           "Choose the VE Dependencies directory:",
                           searchDir,
                           style=wx.DD_DEFAULT_STYLE)
        if dlg.ShowModal() == wx.ID_OK:
            ##If a directory's chosen, exit the loop and return it.
            searchDir = dlg.GetPath()
            dirChosen = True
            dlg.Destroy()
        elif config.Read("DependenciesDir", ":::") == ":::":
            ##If not, and no existing DependenciesDir exists,
            ##show an error message and ask the user to choose
            ##another directory or quit the launcher.
            dlg.Destroy()
            dlg = wx.MessageDialog(None,
                                   "You didn't choose a Dependencies" +
                                   " directory.\n" +
                                   "VE Suite Launcher won't run" +
                                   " without one.\n" +
                                   "Press OK to find the directory" +
                                   " or Cancel to quit VE Suite Launcher.",
                                   "Error: Directory Not Chosen",
                                   wx.OK | wx.CANCEL)
            ##Quit if the user refuses to choose a Dependencies directory.
            if dlg.ShowModal() == wx.ID_CANCEL:
                self.Hide()
                self.Destroy()
                sys.exit(0)
            else:
                return self.DependenciesChange("dead parrot sketch")
        else:
            ##Else if none chosen & one already exists,
            ##return None.
            searchDir = None
            dlg.Destroy()
        return searchDir

    def ModeChanged(self, event):
        """Saves data & changes settings to match the selected mode."""
        ##Save previous data entered by user.
        self.UpdateData()
        ##Change displayed data to mode's data.
        self.UpdateDisplay()

    def UpdateData(self):
        """Saves the user's input to the launcher's data variables."""
        ##NOTE: Will have to change way user's variables are saved if 
        ##modes allow users to change these in the future.
        ##Probably by grabbing the oldMode and checking its settings.
        if self.txTaoMachine.IsEnabled():
            self.taoMachine = self.txTaoMachine.GetValue()
        if self.txTaoPort.IsEnabled():
            self.taoPort = self.txTaoPort.GetValue()

    def UpdateDisplay(self):
        """Changes settings to match the selected mode."""
        newMode = self.rbMode.GetStringSelection()
        if newMode in MODE_DICT:
            modeRules = MODE_DICT[newMode]
        else:
            modeRules = {}
        ##Go through each rule listed in modeRules.
        ##Change the corresponding controls to the value given.
        ##If it's FIXED, prevent the user from changing it.
        ##If a rule isn't given, it uses its regular value.
        if "taoMachine" in modeRules:
            self.txTaoMachine.SetValue(modeRules["taoMachine"][1])
            self.txTaoMachine.Enable(modeRules["taoMachine"][0])
        else:
            self.txTaoMachine.SetValue(self.taoMachine)
            self.txTaoMachine.Enable(True)
        if "taoPort" in modeRules:
            self.txTaoPort.SetValue(modeRules["taoPort"][1])
            self.txTaoPort.Enable(modeRules["taoPort"][0])
        else:
            self.txTaoPort.SetValue(self.taoPort)
            self.txTaoPort.Enable(True)

    def GetSelectedJconf(self):
        """Returns the path of the selected Jconf file."""
        mode = self.rbMode.GetStringSelection()
        if mode in MODE_DICT:
            modeRules = MODE_DICT[mode] 
        else:
            modeRules = {}
        if ("jconf" in modeRules) and (modeRules["jconf"][0] == FIXED):
            xplorerConfig = modeRules["jconf"][2]
        else:
            xplorerConfig = self.jconfList.GetPath(self.jconfSelection)
        return xplorerConfig

    def ChooseDirectory(self, event):
        """The user chooses the working directory through a dialog."""
        curDir = self.txDirectory.GetValue()
        ##NOTE: If curDir doesn't exist, it automatically goes
        ##to the user's directory
        dlg = wx.DirDialog(self, "Choose VE Suite's working directory:",
                           self.txDirectory.GetValue(),
                           style=wx.DD_DEFAULT_STYLE | wx.DD_NEW_DIR_BUTTON)
        if dlg.ShowModal() == wx.ID_OK:
            self.txDirectory.SetValue(dlg.GetPath())
            self.txDirectory.SetInsertionPointEnd()
        dlg.Destroy()

    def ChooseSaveConfig(self, event):
        """Lets the user choose which name to save a configuration under."""
        dlg = wx.TextEntryDialog(self,
                                 "Enter your configuration's name:",
                                 "Save Configuration")
        if dlg.ShowModal() == wx.ID_OK:
            name = dlg.GetValue()
            dlg.Destroy()
            ##Don't overwrite the default config.
            if name == "previous":
                err = wx.MessageDialog(self,
                                       "You can't name it 'previous'.\n" +
                                       "The default config uses that.",
                                       "Error: Reserved Name Chosen",
                                       wx.OK)
                err.ShowModal()
                err.Destroy()
                return
            ##Check for slashes
            if name.count('/') > 0 or name.count('\\') > 0:
                err = wx.MessageDialog(self,
                                       "You can't use a name with\n" +
                                       "slashes in it.\n",
                                       "Error: Name Contains Slashes",
                                       wx.OK)
                err.ShowModal()
                err.Destroy()
                return
            ##Check if it's empty/spaces
            elif name.isspace() or name == '':
                err = wx.MessageDialog(self,
                                       "You can't use an empty name.",
                                       "Error: Name is Empty",
                                       wx.OK)
                err.ShowModal()
                err.Destroy()
                return
            ##Confirm if it'll overwrite another file.
            config.SetPath("..")
            overwrite = config.Exists(name)
            config.SetPath(DEFAULT_CONFIG)
            if overwrite:
                confirm = wx.MessageDialog(self,
                                           "%s already exists.\n" % name +
                                           "Do you want to overwrite it?",
                                           "Confirm Overwrite",
                                           wx.YES_NO | wx.YES_DEFAULT)
                choice = confirm.ShowModal()
                confirm.Destroy()
                if choice == wx.ID_NO:
                    return
            ##Save the config.
            self.SaveConfig(name)
        else:
            dlg.Destroy()


    def ChooseLoadConfig(self, event):
        """Lets the user choose a confiuration to load."""
        message = "Please choose a\n" + "configuration to load."
        choices = []
        config.SetPath("..")
        configEntry = config.GetFirstGroup()
        while (configEntry[0]):
            if configEntry[1] != DEFAULT_CONFIG:
                choices.append(configEntry[1])
            configEntry = config.GetNextGroup(configEntry[2])
        config.SetPath(DEFAULT_CONFIG)
        ##Return if no configurations are saved.
        if len(choices) <= 0:
            dlg = wx.MessageDialog(self,
                                   "You don't have any\n" +
                                   "saved configurations\n" +
                                   "to choose from.",
                                   "No Configs Saved", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        ##Else ask the user to select a configuration.
        dlg = wx.SingleChoiceDialog(self, message, "Load Configuration",
                                    choices)
        dlg.SetSelection(0)
        if dlg.ShowModal() == wx.ID_OK:
            choice = dlg.GetStringSelection()
            self.LoadConfig(choice)
        dlg.Destroy()

    def DeleteConfig(self, event):
        """Lets the user choose a confiuration to delete."""
        message = "Choose a configuration to delete."
        choices = []
        config.SetPath("..")
        configEntry = config.GetFirstGroup()
        while (configEntry[0]):
            if configEntry[1] != DEFAULT_CONFIG:
                choices.append(configEntry[1])
            configEntry = config.GetNextGroup(configEntry[2])
        config.SetPath(DEFAULT_CONFIG)
        ##Return if no configurations are saved.
        if len(choices) <= 0:
            dlg = wx.MessageDialog(self,
                                   "You don't have any\n" +
                                   "saved configurations\n" +
                                   "to delete!",
                                   "No Configs Saved", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        ##Else ask the user to select a configuration.
        dlg = wx.SingleChoiceDialog(self, message, "Delete Configuration",
                                    choices)
        dlg.SetSelection(0)
        if dlg.ShowModal() == wx.ID_OK:
            choice = dlg.GetStringSelection()
            confirm = wx.MessageDialog(self,
                                       "Are you sure you want to\n" +
                                       "delete the %s configuration?" % choice,
                                       "Confirm Deletion",
                                       wx.YES_NO | wx.NO_DEFAULT)
            if confirm.ShowModal() == wx.ID_YES:
                config.SetPath("..")
                config.DeleteGroup(choice)
                config.SetPath(DEFAULT_CONFIG)
            confirm.Destroy()
        dlg.Destroy()
        
    def SaveConfig(self, name):
        """Saves the current configuration under name.

        Keyword arguments:
        name -- What to name this configuration"""
        ##Update the launcher's data
        self.UpdateData()
        ##Set config
        config.SetPath('..')
        config.SetPath(name)
        ##Save the current configuration under name
        if self.dependencies != None:
            config.Write("DependenciesDir", self.dependencies)
        if self.builderDir != None:
            config.Write("BuilderDir", self.builderDir)
        config.Write("Directory", self.txDirectory.GetValue())
        config.Write("JconfSelection", self.jconfSelection)
        config.Write("NameServer", str(self.nameServer))
        config.Write("Xplorer", str(self.xplorer))
        config.WriteInt("XplorerType", self.xplorerType)
        config.Write("Conductor", str(self.conductor))
        config.Write("TaoMachine", self.taoMachine)
        config.Write("TaoPort", self.taoPort)
        config.Write("DesktopMode", str(self.desktop))
        config.WriteInt("Mode", self.rbMode.GetSelection())
        config.Write("ClusterMaster", self.clusterMaster)
        ##Redo the Jconf/Cluster configs
        config.DeleteGroup(JCONF_CONFIG)
        self.jconfList.WriteConfig()
        config.DeleteGroup(CLUSTER_CONFIG)
        self.clusterDict.WriteConfig()
        ##Return to default config
        config.SetPath('..')
        config.SetPath(DEFAULT_CONFIG)
        return
    
    def LoadConfig(self, name):
        """Loads the configuration under name.

        Keyword arguments:
        name -- Name of configuration to load
        """
        ##Set config
        config.SetPath('..')
        config.SetPath(name)
        ##Set directory, set insertion pt. to end for better initial view.
        if devMode:
            defaultDirectory = os.getcwd()
        else:
            defaultDirectory = DIRECTORY_DEFAULT
        self.txDirectory.SetValue(config.Read("Directory", defaultDirectory))
        ##Sets txDirectory's cursor to end in Linux systems for easier reading.
        ##Acts strange in Windows for some reason; investigate.
        if os.name == "posix":
            self.txDirectory.SetInsertionPointEnd()
        ##Set Cluster list.
        self.clusterDict = ClusterDict()
        ##Set ClusterMaster
        self.clusterMaster = config.Read("ClusterMaster", "")
        ##Set choices for Jconf list.
        defaultSelection = "None"
        if config.HasGroup(JCONF_CONFIG):
            self.jconfList = JconfList()
        ##Set default choices if JCONF_CONFIG doesn't exist,
        ##but DependenciesDir does.
        elif config.Read("DependenciesDir", ":::") != ":::":
            p = DEFAULT_JCONF
            config.SetPath(JCONF_CONFIG)
            defaultSelection = os.path.split(p)[1][:-6]
            config.Write(defaultSelection, p)
            config.SetPath('..')
            self.jconfList = JconfList()
        ##If neither exists, bring up an error. NOTE: Should never be reached.
        else:
            print "ERROR: No Jconf configuration found and failed to make" + \
                  " default Jconf from Dependencies dir."
            print "Substituting dev .jconf path instead."
            config.SetPath(JCONF_CONFIG)
            defaultSelection = "DevDesktop"
            path = os.path.join(os.getcwd(), "..", "..",
                                "VE_Xplorer", "stereo_desktop", "desktop.jconf")
            config.Write(defaultSelection, path)
            config.SetPath('..')
            self.jconfList = JconfList()
        ##Set Jconf cursor.
        self.jconfSelection = config.Read("JconfSelection",
                                          defaultSelection)
        ##Set Tao Machine & Port.
        self.taoMachine = config.Read("TaoMachine", "localhost")
        ##Temporary workaround for error w/ Int TaoPort in last version
        if config.GetEntryType("TaoPort") == 3: ##3: Int entry type",
            self.taoPort = str(config.ReadInt("TaoPort", 1239))
        else:
            self.taoPort = config.Read("TaoPort", "1239")
        ##Set Name Server
        if config.Read("NameServer", "True") == "True":
            self.nameServer = True
        else:
            self.nameServer = False            
        ##Set Xplorer
        if config.Read("Xplorer", "True") == "True":
            self.xplorer = True
        else:
            self.xplorer = False
        ##Set Xplorer Type
        data = config.ReadInt("XplorerType", -1)
        if data >= 0 and data < len(RADIO_XPLORER_LIST):
            self.xplorerType = data
        else:
            self.xplorerType = 0
        ##Set Conductor
        if config.Read("Conductor", "True") == "True":
            self.conductor = True
        else:
            self.conductor = False
        ##Set Desktop Mode
        if config.Read("DesktopMode", "False") == "True":
            self.desktop = True
        else:
            self.desktop = False
        ##Set Mode
        self.rbMode.SetSelection(config.ReadInt("Mode", 0))
        self.UpdateDisplay()
        ##Set Builder Dir
        if config.Exists("BuilderDir"):
            self.builderDir = config.Read("BuilderDir", "ErrOr")
        ##Return to default config
        config.SetPath('..')
        config.SetPath(DEFAULT_CONFIG)
        return

    def Settings(self, event):
        """Launches the Custom Settings window."""
        mode = self.rbMode.GetStringSelection()
        if mode in MODE_DICT:
            modeRules = MODE_DICT[mode]
        else:
            modeRules = {}
        x, y = self.GetPosition()
        x2, y2 = self.rbMode.GetPosition()
        x3 = self.rbMode.GetSize()[0]
        ##Sets the upper-left corner of the Settings window to be just
        ##right of the Mode radio box. Needs some nudging to avoid covering
        ##up important info.
        OFFSET = 20 ##Pixels
        position = wx.Point(x + x2 + x3 + OFFSET,
                            y + y2 + OFFSET)
        frame = SettingsWindow(self, self.jconfList, self.jconfSelection,
                                     self.clusterDict, self.clusterMaster,
                                     self.desktop, self.nameServer,
                                     self.conductor, self.xplorer,
                                     self.xplorerType,
                                     modeRules, mode,
                                     position = position)
        frame.ShowModal()
        ##frame.Destroy()

    def Launch(self, event):
        """Checks input, begins launch if error-free."""
        ##Set mode
        mode = self.rbMode.GetStringSelection()
        if mode in MODE_DICT:
            modeRules = MODE_DICT[mode]
        else:
            modeRules = {}
        ##Set variables
        if ("conductor" in modeRules) and (modeRules["conductor"][0] == FIXED):
            conductor = modeRules["conductor"][1]
        else:
            conductor = self.conductor
        if ("nameServer" in modeRules) and \
           (modeRules["nameServer"][0] == FIXED):
            nameServer = modeRules["nameServer"][1]
        else:
            nameServer = self.nameServer
        if ("xplorer" in modeRules) and (modeRules["xplorer"][0] == FIXED):
            xplorer = modeRules["xplorer"][1]
        else:
            xplorer = self.xplorer
        if ("xplorerType" in modeRules) and \
           (modeRules["xplorerType"][0] == FIXED):
            xplorerType = modeRules["xplorerType"][1]
        else:
            xplorerType = self.xplorerType
        if ("desktop" in modeRules) and (modeRules["desktop"][0] == FIXED):
            desktop = modeRules["desktop"][1]
        else:
            desktop = self.desktop
        if ("shell" in modeRules) and (modeRules["shell"][0] == FIXED):
            self.shell = modeRules["shell"][1]
        else:
            self.shell = False
        ##ERROR CHECK:  Are any programs selected?
        ##              If not, abort launch.
        if not (conductor or nameServer or xplorer or self.shell):
            dlg = wx.MessageDialog(self,
                                   "The launch won't do anything because you"+
                                   " haven't chosen any programs to launch.\n"+
                                   "Please choose some programs to launch" +
                                   " and try again.",
                                   "Launch Error: No Program Selected", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            self.Settings("dead parrot sketch")
            return
        ##ERROR CHECK:  Is the Tao Machine name blank?
        ##              If so, abort launch.
        taoMachine = self.txTaoMachine.GetValue()
        if taoMachine == "" or taoMachine.isspace():
            dlg = wx.MessageDialog(self,
                                   "You haven't entered the CE's Name.\n" +
                                   "Please enter a name for the CE.",
                                   "Launch Error: Missing CE Name", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        ##ERROR CHECK:  Is the Tao Port between 0 and 65535?
        ##              If not, abort launch.
        if not (self.txTaoPort.GetValue().isdigit() and
                int(self.txTaoPort.GetValue()) <= 65535):
            dlg = wx.MessageDialog(self,
                                   "You have entered an illegal CE Port.\n" +
                                   "Please enter a port between 0 and" +
                                   " 65535.",
                                   "Launch Error: Illegal CE Port", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        ##ERROR CHECK:  Does the working directory chosen exist?
        ##              If not, let the user change it.
        if not (os.path.exists(self.txDirectory.GetValue())):
            dlg = wx.MessageDialog(self,
                                   "The working directory you chose,\n" +
                                   "%s,\n" %(self.txDirectory.GetValue()) +
                                   "doesn't exist.\n" +
                                   "Do you want to select a new working " +
                                   "directory and continue the launch?",
                                   "Launch Error: Directory Doesn't Exist",
                                   wx.YES_NO | wx.YES_DEFAULT)
            ##Activate ChooseDirectory & continue if user chooses YES.
            if dlg.ShowModal()==wx.ID_YES:
                self.ChooseDirectory("dead parrot sketch")
                dlg.Destroy()
                ##If the user didn't choose a new directory,
                ##catch it and abort the launch.
                if not (os.path.exists(self.txDirectory.GetValue())):
                    dlg = wx.MessageDialog(self,
                                           "You didn't choose an existing" +
                                           " directory.\n" +
                                           "Aborting the launch. Choose an" +
                                           " existing working directory and" +
                                           " try launching again.",
                                           "Launch Error: Directory Doesn't" +
                                           " Exist", wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()
                    return            self.txDirectory.SetInsertionPointEnd()
            ##If user chooses NO, abort the launch.
            else:
                dlg.Destroy()
                return
        ##ERROR CHECK:  Does the selected Jconf file exist?
        ##              If not, abort the launch.
        GetJconfPath = self.GetSelectedJconf()
        if xplorer and not (os.path.exists(GetJconfPath)):
            dlg = wx.MessageDialog(self,
                                   "The Xplorer configuration file you chose,"+
                                   "\n%s,\n" %(GetJconfPath) +
                                   " doesn't exist.\n" +
                                   "Please select a different one.",
                                   "Launch Error: Jconf File Doesn't Exist",
                                   wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        ##Set the builderDir, if necessary.
        passedBuilderDir = None
        loop = True
        if self.shell and loop:
            dlg = wx.MessageDialog(self,
                                    "Do you want to use this shell\n" +
                                    "to run VE-Builder?",
                                    "Running VE-Builder",
                                    wx.YES_NO)
            choice = dlg.ShowModal()
            dlg.Destroy()
            loop = False
            if choice == wx.ID_YES:
                ##Ask for the builder directory.
                if self.builderDir == None:
                    startBuilderDir = os.getcwd()
                else:
                    startBuilderDir = self.builderDir
                dlg = wx.DirDialog(None,
                                   "Choose the VE-Builder directory:",
                                   startBuilderDir,
                                   style=wx.DD_DEFAULT_STYLE)
                choice = dlg.ShowModal()
                if choice == wx.ID_OK:
                    ##If a directory's chosen, go ahead.
                    self.builderDir = dlg.GetPath()
                    passedBuilderDir = self.builderDir
                    loop = False
                else: ##If not, ask if they want to run VE-Builder again.
                    pass
                dlg.Destroy()
            else:
                loop = False
        ##BANDAID
        ##This DirDialog is used to purge dlg of previous MessageDialogs.
        ##Without it, having a MessageDialog w/o a DirDialog after it
        ##hangs the program's end.
        ##Figure out the cause later so we can remove this.
        dlg = wx.DirDialog(None,
                           "Purges dlg; temporary workaround",
                           "",
                           style=wx.DD_DEFAULT_STYLE)
        dlg.Destroy()
        ##Hide the Launcher.
        self.Hide()
        ##Bring up the Launch progress window.
        ##progress = LaunchStartedWindow(self)
        ##progress.Show()
        ##Go into the Launch
        Launch(self,
               self.txDirectory.GetValue(),
               nameServer, conductor,
               xplorer, xplorerType,
               GetJconfPath,
               self.txTaoMachine.GetValue(), int(self.txTaoPort.GetValue()),
               desktop, cluster = self.clusterDict.GetCheckedLocations(),
               master = self.clusterMaster,
               shell = self.shell, builderDir = passedBuilderDir)
        ##Destroy the Launch progress window.
        ##progress.OnClose("this message does not matter")
        ##Show NameServer kill window if NameServer was started.
        if nameServer:
            win = ServerKillWindow()
        ##Close the Launcher
        self.OnClose("this message does not matter")

    def OnClose(self, event):
        """Saves launcher's current configuration and quits the launcher.

        Called after a successful Launch or when the user manually closes
        the launcher window."""
        ##(Add & to the end of its command.)
        ##Update default config file.
        self.SaveConfig(DEFAULT_CONFIG)
        config.Flush()
        self.Hide()
        self.Destroy()
        ##If a shell's launched, start it here, after cleanup.
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


##class LaunchStartedWindow(wx.Frame):
##    """A window to tell the user the launch is progressing."""
##    def __init__(self, parent = None, title = "Launching Now..."):
##        """Creates the Launch Started Window."""
##        wx.Frame.__init__(self, parent, wx.ID_ANY, title,
##                          style = wx.DEFAULT_FRAME_STYLE &
##                          ~ (wx.RESIZE_BORDER | wx.CLOSE_BOX | wx.MAXIMIZE_BOX))
##        lblMsg = wx.StaticText(self, -1, "VE-Suite is sending the launch code\n"+\
##                                         "to the computer now. Please wait.\n")
##        rowSizer = wx.BoxSizer(wx.VERTICAL)
##        border = 10
##        rowSizer.Add(lblMsg, 2, wx.ALL, border)
##        rowSizer.SetSizeHints(self)
##        Style(self)
##        self.SetSizer(rowSizer)
##        self.CentreOnScreen()
##
##    def OnClose(self, event):
##        self.Hide()
##        self.Destroy()


class SettingsWindow(wx.Dialog):
    """Subwindow for viewing/changing mode settings."""
    def __init__(self, parent,
                 jconf, jconfSelection, clusterDict, clusterMaster,
                 desktop, nameServer, conductor, xplorer, xplorerType,
                 modeRules, modeName, position = wx.DefaultPosition):
        """Creates the Settings window."""
        wx.Dialog.__init__(self, parent, -1, "%s Settings" %(modeName),
                           pos = position,
                           style = wx.DEFAULT_FRAME_STYLE &
                           ~ (wx.RESIZE_BORDER | wx.RESIZE_BOX |
                           wx.MAXIMIZE_BOX))
        ##While creating, go through each rule listed in modeRules.
        ##Change the corresponding controls to the value given.
        ##If it's FIXED, prevent the user from changing it.
        ##If a rule isn't given, it uses its regular value.
        ##Set up data.
        self.jconfList = jconf
        self.clusterDict = clusterDict
        self.clusterMaster = clusterMaster
        self.xplorerTypeFixed = False
        self.desktop = desktop
        if ("xplorerType" in modeRules) and \
           (modeRules["xplorerType"][0] == FIXED):
            self.xplorerTypeFixed = True
        self.desktopFixed = False
        if ("desktop" in modeRules) and (modeRules["desktop"][0] == FIXED):
            self.desktopFixed = True
        ##Jconf pull-down menu.
        self.chJconf = wx.Choice(self, -1)
        self.chJconf.SetToolTip(wx.ToolTip("Choose Xplorer's configuration."))
        ##Edit Jconf button.
        self.bEditJconf = wx.Button(self, -1, "Edit Configuration List")
        self.bEditJconf.SetToolTip(wx.ToolTip("Edit the list of Xplorer" +
                                              " configurations."))
        if "jconf" in modeRules:
            self.chJconf.Append(modeRules["jconf"][1])
            self.chJconf.SetSelection(0)
            self.chJconf.Enable(modeRules["jconf"][0])
            self.bEditJconf.Enable(modeRules["jconf"][0])
        elif ("xplorer" in modeRules) and (modeRules["xplorer"][0] == FIXED) \
             and (modeRules["xplorer"][1] == False):
            self.chJconf.Append("None")
            self.chJconf.SetSelection(0)
            self.chJconf.Enable(False)
            self.bEditJconf.Enable(False)
        else:
            self.UpdateChJconf(jconfSelection)
        ##Name Server checkbox.
        self.cbNameServer = wx.CheckBox(self, -1, "Name Server")
        self.cbNameServer.SetToolTip(wx.ToolTip("Run Name Server at Launch"))
        if "nameServer" in modeRules:
            self.cbNameServer.SetValue(modeRules["nameServer"][1])
            self.cbNameServer.Enable(modeRules["nameServer"][0])
        else:
            self.cbNameServer.SetValue(nameServer)
        ##Conductor checkbox.
        self.cbConductor = wx.CheckBox(self, -1, "Conductor")
        self.cbConductor.SetToolTip(wx.ToolTip("Run Conductor at Launch"))
        if "conductor" in modeRules:
            self.cbConductor.SetValue(modeRules["conductor"][1])
            self.cbConductor.Enable(modeRules["conductor"][0])
        else:
            self.cbConductor.SetValue(conductor)
        ##Xplorer checkbox.
        self.cbXplorer = wx.CheckBox(self, -1, "Xplorer")
        self.cbXplorer.SetToolTip(wx.ToolTip("Run Xplorer at Launch"))
        if "xplorer" in modeRules:
            self.cbXplorer.SetValue(modeRules["xplorer"][1])
            self.cbXplorer.Enable(modeRules["xplorer"][0])
        else:
            self.cbXplorer.SetValue(xplorer)
        ##Desktop checkbox.
        self.cbDesktop = wx.CheckBox(self, -1, "Desktop Mode")
        self.cbDesktop.SetToolTip(wx.ToolTip("Set Desktop Mode for" +
                                             " Conductor and Xplorer"))
        if "desktop" in modeRules:
            self.cbDesktop.SetValue(modeRules["desktop"][1])
            self.cbDesktop.Enable(modeRules["desktop"][0])
        else:
            self.cbDesktop.SetValue(self.desktop)
        ##Xplorer Type radio box.
        self.rbXplorer = wx.RadioBox(self, -1, "Xplorer Type",
                                     wx.DefaultPosition, wx.DefaultSize,
                                     RADIO_XPLORER_LIST, 2, wx.RA_SPECIFY_ROWS)
        self.rbXplorer.SetToolTip(wx.ToolTip("Which Xplorer format do you" +
                                             " want to launch?"))
        if "xplorerType" in modeRules:
            self.rbXplorer.SetSelection(modeRules["xplorerType"][1])
            self.rbXplorer.Enable(modeRules["xplorerType"][0])
        else:
            self.rbXplorer.SetSelection(xplorerType)
        ##Cluster button.
        self.bCluster = wx.Button(self, -1, "Set Cluster Computers")
        self.bCluster.SetToolTip(wx.ToolTip("Set the computers in" +
                                            " the cluster."))
        if os.name == "nt" or not CLUSTER_ENABLED:
            self.bCluster.Hide()
        ##Set up OK button.
        bOk = wx.Button(self, -1, "OK")
        bOk.SetToolTip(wx.ToolTip("Return to the Launcher."))
        self.EvtCheckXplorer("dead parrot sketch")
        ##Also calls self.EvtCheckDesktop as a follow-up
        ##Bind events.
        self.Bind(wx.EVT_CHECKBOX, self.EvtCheckXplorer, self.cbXplorer)
        self.Bind(wx.EVT_RADIOBOX, self.EvtCheckCluster, self.rbXplorer)
        self.Bind(wx.EVT_CHECKBOX, self.EvtCheckDesktop, self.cbConductor)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_BUTTON, self.EditJconf, self.bEditJconf)
        self.Bind(wx.EVT_BUTTON, self.EditCluster, self.bCluster)
        ##Set sizers.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        ##Construct & insert the Jconf column.
        rowSizer.Add(wx.StaticText(self, -1, "Xplorer configuration:"))
        columnSizer.Add(self.chJconf, 1, wx.ALIGN_BOTTOM)
        columnSizer.AddMany([HORIZONTAL_SPACE,
                             self.bEditJconf])
        rowSizer.Add(columnSizer, 0, wx.EXPAND)
        ##Construct & insert the check box/radio box grid.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(self.cbDesktop)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.bCluster)
        gridSizer = wx.FlexGridSizer(3, 2,
                                     VERTICAL_SPACE[1], HORIZONTAL_SPACE[0])
        gridSizer.Add(self.cbNameServer)
        if os.name == "nt":
            gridSizer.Add((-1, -1))
        else:
            gridSizer.Add((-1, self.bCluster.GetSize()[1]))
        gridSizer.AddMany([self.cbConductor, columnSizer,
                           self.cbXplorer, self.rbXplorer])
        ##Insert the Programs to Launch grid.
        rowSizer.AddMany([VERTICAL_SPACE,
                          wx.StaticText(self, -1, "Programs to launch:"),
                          VERTICAL_SPACE,
                          gridSizer])
        ##Set the main sizer, insert OK button.
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer, 0, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(bOk, 1, wx.EXPAND)
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
##        self.SetSize(INITIAL_WINDOW_SIZE)
        ##Set the background color.
        Style(self)

    def EvtCheckXplorer(self, event):
        """Enables/Disables Xplorer's radio box.

        Prevents the user from choosing Xplorer's mode if Xplorer
        won't be launched. The radio box is enabled if Xplorer's
        check box is checked, disabled if it isn't."""
        if self.xplorerTypeFixed == True:
            self.rbXplorer.Enable(False)
        else:
            self.rbXplorer.Enable(self.cbXplorer.IsChecked())
        ##Goes into EvtCheckCluster to check that against cbXplorer, too.
        self.EvtCheckCluster("dead parrot sketch")

    def EvtCheckCluster(self, event):
        """Enables/Disables the Cluster button.

        Prevents the user from choosing Cluster computers if
        Xplorer won't run in OSG Cluster mode."""
        if self.cbXplorer.IsChecked() and self.rbXplorer.GetSelection() == 2:
            self.bCluster.Enable(True)
        else:
            self.bCluster.Enable(False)
        ##Goes into EvtCheckDesktop to check that against rbXplorer, too.
        self.EvtCheckDesktop("dead parrot sketch")

    def EvtCheckDesktop(self, event):
        """Enables/Disables the Desktop button.

        Prevents the user from choosing Desktop mode if
        Conductor and Xplorer won't be launched."""
        if self.desktopFixed == True:
            self.cbDesktop.Enable(False)
        else:
            self.cbDesktop.Enable(self.cbConductor.IsChecked() or
                                  self.cbXplorer.IsChecked())

    def UpdateChJconf(self, selection):
        """Updates the Jconf choice window in the Launcher.

        Keyword arguments:
        selection -- name of selected choice)"""
        ##Rebuild the choice list
        self.chJconf.Clear()
        nameArray = self.jconfList.GetNames()
        for i in range(len(nameArray)):
            self.chJconf.Append(nameArray[i])
        ##Set the selection
        if selection not in nameArray:
            selection = self.chJconf.GetString(0)
        ##Error catcher for lists without any items. Should never happen.
        if self.chJconf.GetCount() == 0:
            cursor = wx.NOT_FOUND
        ##Set the selection
        self.chJconf.SetStringSelection(selection)

    def EditJconf(self, event):
        """Brings up the Jconf editing window."""
        jconfWindow = JconfWindow(self, wx.ID_ANY, "Xplorer Configurations",
                                  self.jconfList,
                                  self.chJconf.GetStringSelection())
        jconfWindow.ShowModal()
        ##jconfWindow.Destroy()        

    def GetSelectedJconf(self):
        """Returns the path of the selected Jconf file."""
        jconfFile = self.jconfList.GetPath(self.chJconf.GetStringSelection())
        return jconfFile

    def EditCluster(self, event):
        """Brings up the Cluster editing window."""
        clusterWindow = ClusterWindow(self, self.clusterDict,
                                      self.clusterMaster)
        clusterWindow.ShowModal()
        ##clusterWindow.Destroy()

    ##Saves the current configuration under the prefs file before closing.
    def OnClose(self, event):
        """Sends current configuration back to parent & closes window."""
        ##Update parent.
        parent = self.GetParent()
        if self.cbConductor.IsEnabled():
            parent.conductor = self.cbConductor.GetValue()
        if self.cbNameServer.IsEnabled():
            parent.nameServer = self.cbNameServer.GetValue()
        if self.cbXplorer.IsEnabled():
            parent.xplorer = self.cbXplorer.GetValue()
        if self.cbXplorer.IsEnabled() or self.rbXplorer.IsEnabled():
            parent.xplorerType = self.rbXplorer.GetSelection()
        if self.cbXplorer.IsEnabled() or self.cbConductor.IsEnabled() or \
           self.cbDesktop.IsEnabled():
            parent.desktop = self.cbDesktop.GetValue()
        if self.chJconf.IsEnabled():
            parent.jconfList = self.jconfList
            parent.jconfSelection = self.chJconf.GetStringSelection()
        parent.clusterMaster = self.clusterMaster
        ##Close.
        self.Hide()
        self.Destroy()


class JconfList:
    """Stores a list of Jconf pairs in this setup:
    {name: path, name: path..}
    under the variable self.jDict.

    Functions:
        __init__()
        Add(name, path)
        Rename(oldName, newName)
        Delete(name)
        UniqueName(name)
        GetPath(name)
        Length() / __len__
        GetNames()
        WriteConfig()
    """
    def __init__(self):
        """Creates a list of .jconf names/paths from the Launcher's Config."""
        self.jDict = {}
        config.SetPath(JCONF_CONFIG)
        bCont = config.GetFirstEntry()
        while (bCont[0]):
            self.jDict[bCont[1]] = config.Read(bCont[1])
            bCont = config.GetNextEntry(bCont[2])
        config.SetPath('..')

    def Add(self, name, path):
        """Adds [name, path] to the list.

        Returns name, modified until it's unique on the list."""
        ##Change name if it matches another one on the list.
        finalName = self.UniqueName(name)
        self.jDict[finalName] = path
        config.SetPath(JCONF_CONFIG)
        config.Write(finalName, path)
        config.SetPath('..')
        ##Return the final name.
        return finalName

    def Rename(self, oldName, newName):
        """Renames the entry at oldName to newName.

        Returns newName, modified until it's unique on the list."""
        ##Don't do anything if newName == pos's old name
        if newName == oldName:
            return newName
        ##Change name if it matches another one on the list.
        finalName = self.UniqueName(newName)
        config.SetPath(JCONF_CONFIG)
        config.RenameEntry(oldName, finalName)
        config.SetPath('..')
        self.jDict[finalName] = self.jDict[oldName]
        del self.jDict[oldName]
        ##Return true if name had to be changed.
        return finalName

    def Delete(self, name):
        """Deletes entry at name."""
        config.SetPath(JCONF_CONFIG)
        config.DeleteEntry(name)
        config.SetPath('..')
        del self.jDict[name]

    def UniqueName(self, name):
        """Replaces name with a unique name in the list."""
        ##Add a numeric suffix to name if it matches a name
        ##already in the list.
        suffix = ""
        maxCount = 0
        curNames = self.GetNames()
        while curNames.count(name + str(suffix)) > maxCount:
            if suffix == "":
                suffix = '1'
            else:
                suffix = str(int(suffix) + 1)
        return name + str(suffix)

    def GetPath(self, name):
        """Returns the path of name's entry."""
        if name in self.jDict:
            return self.jDict[name]
        else:
            return "None"

    def Length(self):
        """Returns the length of self.jDict."""
        return len(self.jDict)

    def __len__(self):
        """Returns the length of self.jDict."""
        return self.Length()

    def GetNames(self):
        """Returns a sorted list of the entries' names."""
        nList = []
        for name in self.jDict:
            nList.append(name)
        nList.sort(lambda x, y: cmp(x.lower(), y.lower()))
        return nList

    def WriteConfig(self):
        """Writes the entire list to config."""
        config.SetPath(JCONF_CONFIG)
        for name in self.jDict:
            config.Write(name, self.jDict[name])
        config.SetPath('..')

class JconfWindow(wx.Dialog):
    """A window for editing a list of Jconf files.

    Functions:
        __init__(parent, ID, title, L, cursor=0)
        DisplayJconfFile(event)
        DeleteEnabledCheck()
        Update(selection)
        AddNew(event)
        Delete(event)
        Rename(event)
        NameChangeWarning(oldName, newName)
        OnClose(event)
    """
    def __init__(self, parent, ID, title, L, cursor):
        """Sets up the Jconf window.

        Keyword arguments:
        L: The linked Jconf dict this window modifies.
        cursor: Name of the current selection in L."""
        wx.Dialog.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.jDict = L
        ##Build displays.
        self.lblPath = wx.StaticText(self, -1)
        self.confList = wx.ListBox(self, -1, size=JCONF_LIST_DISPLAY_MIN_SIZE,
                                   choices=self.jDict.GetNames())
        self.confList.SetStringSelection(cursor)
        self.display = wx.TextCtrl(self, -1, style=wx.TE_READONLY)
        self.DisplayJconfFile("dead parrot sketch")
        ##Build buttons.
        bAdd = wx.Button(self, -1, "Add")
        bAdd.SetToolTip(wx.ToolTip("Add a configuration listing."))
        bRename = wx.Button(self, -1, "Rename")
        bRename.SetToolTip(wx.ToolTip("Rename a configuration listing."))
        self.bDelete = wx.Button(self, -1, "Delete")
        self.bDelete.SetToolTip(wx.ToolTip("Delete a configuration listing."))
        bOk = wx.Button(self, -1, "Ok")
        bOk.SetToolTip(wx.ToolTip("Return to Settings."))
        ##Check if Delete's enabled.
        self.DeleteEnabledCheck()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.AddNew, bAdd)
        self.Bind(wx.EVT_BUTTON, self.Delete, self.bDelete)
        self.Bind(wx.EVT_BUTTON, self.Rename, bRename)
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_LISTBOX, self.DisplayJconfFile, self.confList)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        ##Add/Rename/Delete buttons.
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.AddMany([bAdd, VERTICAL_SPACE,
                          bRename, VERTICAL_SPACE,
                          self.bDelete])
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        ##List field + buttons.
        columnSizer.Add(self.confList, 1, wx.EXPAND)
        columnSizer.AddMany([HORIZONTAL_SPACE, rowSizer])
        ##List field + Path display
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.Add(wx.StaticText(self, -1, "Xplorer Configurations:"))
        rowSizer.Add(columnSizer, 1, wx.EXPAND)
        rowSizer.AddMany([VERTICAL_SPACE,
                          self.lblPath])
        rowSizer.Add(self.display, 0, wx.EXPAND)
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer, 1, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(bOk, 0, wx.EXPAND)
        ##Set size, position.
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.SetSize(INITIAL_JCONF_WINDOW_SIZE)
        self.CenterOnParent(wx.BOTH)
        ##Set the background color.
        Style(self)

    def DisplayJconfFile(self, event):
        """Shows the .jconf file of the selection in the text field."""
        s = self.confList.GetStringSelection()
        if s in self.jDict.GetNames():
            f = self.jDict.GetPath(s)
        else:
            f = "ERROR: Entry missing from list."
        self.display.SetValue(f)
        self.display.SetInsertionPointEnd()
        self.lblPath.SetLabel("%s's path:" %(s))
        
    def DeleteEnabledCheck(self):
        """Disables/Enables the Delete button based on number of entries.

        Disabled if entries <= 1
        Enabled if entries > 1"""
        if self.jDict.Length() <= 1:
            self.bDelete.Enable(False)
        else:
            self.bDelete.Enable(True)

    def Update(self, selection):
        """Updates the shown entries list to match recent changes."""
        self.confList.Set(self.jDict.GetNames())
        self.confList.SetStringSelection(selection)
        self.DisplayJconfFile("dead parrot sketch")

    def AddNew(self, event):
        """User chooses a new Jconf file to add to the list.

        Default name: Name of Jconf file."""
        ##Default directory for the search is the
        ##directory of the currently selected Jconf.
##        c = self.confList.GetSelection()
##        if c in range(len(self.jDict)):
##            p = self.jDict.GetPath(c)
##            f = os.path.split(p)[0]
##        else:
##            f = os.getcwd()
        f = config.Read("DependenciesDir", os.getcwd())
        f = os.path.join(f, JUGGLER_FOLDER, "configFiles")
        dlg = wx.FileDialog(self,
                            "Choose a configuration file.",
                            defaultDir = f,
                            wildcard = "Jconfig (*.jconf)|*.jconf",
                            style=wx.OPEN)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            name = os.path.split(path)[1][:-6]
            finalName = self.jDict.Add(name, path)
            if name != finalName:
                self.NameChangeWarning(name, finalName)
            self.Update(finalName)
            self.DeleteEnabledCheck()
        dlg.Destroy()

    def Delete(self, event):
        """Deletes the selected entry from the list.

        Also moves the selection index if it would be off the list."""
        dlg = wx.MessageDialog(self,
                               "Are you sure you want to delete\n" +
                               self.confList.GetStringSelection() + "?",
                               "Confirm Deletion",
                               wx.YES_NO | wx.NO_DEFAULT)
        dlg.CenterOnParent(wx.BOTH)
        if dlg.ShowModal() == wx.ID_YES:
            cursor = self.confList.GetSelection()
            self.jDict.Delete(self.confList.GetStringSelection())
            ##Move the cursor if it wouldn't be on the list anymore.
            if cursor >= len(self.jDict):
                selection = self.confList.GetString(len(self.jDict) - 1)
            else:
                selection = self.confList.GetString(cursor + 1)
##            if cursor >= self.jDict.Length():
##                cursor = self.jDict.Length() - 1
            self.Update(selection)
            self.DeleteEnabledCheck()
        dlg.Destroy()

    def Rename(self, event):
        """Renames the selected Jconf entry.
        
        Ensures the new name:
        -Contains no slashes.
        -Isn't empty spaces."""
        loop = True
        name = self.confList.GetStringSelection()
        while loop:
            n = self.confList.GetStringSelection()
            p = self.jDict.GetPath(n)
            f = os.path.split(p)[1]
            dlg = wx.TextEntryDialog(self,
                                     "What do you want to rename" + \
                                     " %s to?\n\n" %(n) + \
                                     "Jconf File: %s" %(f),
                                     "Rename %s" %(n), name)
            if dlg.ShowModal() == wx.ID_OK:
                name = dlg.GetValue()
                dlg.Destroy()
                selection = self.confList.GetStringSelection()
                ##Check for slashes
                if name.count('/') > 0 or name.count('\\') > 0:
                    dlg = wx.MessageDialog(self,
                                           "Your new name has slashes" + \
                                           " in it.\n" + \
                                           "Please choose a different name.",
                                           "ERROR: Name Contains Slashes",
                                           wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()
                    name = name.replace('/', '-')
                    name = name.replace('\\', '-')
                ##Check if it's empty/spaces
                elif name.isspace() or name == '':
                    dlg = wx.MessageDialog(self,
                                           "Your new name is empty." + \
                                           " Please choose a different name.",
                                           "ERROR: Name is Empty",
                                           wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()
                    name = self.confList.GetStringSelection()
                ##Else accept it.
                else:
                    finalName = self.jDict.Rename(selection, name)
                    if finalName != name:
                        self.NameChangeWarning(name, finalName)
                    self.Update(finalName)
                    loop = False
            else:
                loop = False

    def NameChangeWarning(self, oldName, newName):
        """Warns user if oldName was changed to newName."""
        dlg = wx.MessageDialog(None,
                               "The name %s already existed" %(oldName) + \
                               " in the list.\n" + \
                               "Your entry was given the" + \
                               " name %s instead." %(newName),
                               "NOTE: Name Changed",
                               wx.OK)
        dlg.ShowModal()
        dlg.Destroy()

    def OnClose(self, event):
        """Closes JconfWindow."""
        self.GetParent().UpdateChJconf(self.confList.GetStringSelection())
        self.Hide()
        self.Destroy()


class ClusterDict:
    """Stores a dictionary of cluster comps in this setup:
    {location: True, location: True,...}
    under the variable self.list.

    Functions:
        __init__()
        Add(location)
        Delete(name)
        GetLocation(index)
        Length / __len__
        GetNames()
        GetLocations()
        GetCheckedLocations()
        WriteConfig()
    """
    def __init__(self):
        """Creates a dict of cluster names/locations from Launcher's Config."""
        self.cluster = {}
        config.SetPath(CLUSTER_CONFIG)
        bCont = config.GetFirstGroup()
        while (bCont[0]):
            name = bCont[1]
            config.SetPath(bCont[1])
            location = config.Read("location", "localhost")
            config.SetPath('..')
            self.cluster[location] = True
            bCont = config.GetNextGroup(bCont[2])
        config.SetPath('..')

    def Add(self, location):
        """Adds location: True to the list & config."""
        ##Add to list.
        self.cluster[location] = True
        ##Add to cluster.
        config.SetPath(CLUSTER_CONFIG)
        config.SetPath(location)
        config.Write("location", location)
        config.SetPath('..')
        config.SetPath('..')
        ##NOTE: Adding the same cluster file twice results in the previous one
        ##being overwritten. This is normal functioning now.

    def Delete(self, name):
        """Deletes name's entry."""
        config.SetPath(CLUSTER_CONFIG)
        config.DeleteGroup(name)
        config.SetPath('..')
        del self.cluster[name]

    def GetLocation(self, name):
        """Returns the location of name's entry."""
        if name in self.cluster:
            return self.cluster[name][0]

    def Length(self):
        """Returns the length of self.cluster."""
        return len(self.cluster)

    def __len__(self):
        """Returns the length of self.cluster."""
        return self.Length()

    def GetNames(self):
        """Returns a list of the entries' names."""
        nList = []
        for name in self.cluster:
            nList.append(name)
        nList.sort(lambda x, y: cmp(x.lower(), y.lower()))
        return nList

    def GetLocations(self):
        """Returns a list of the entries' locations.

        Duplicates GetNames()."""
        return self.GetNames()

    def GetCheckedLocations(self):
        """Returns a list of checked locations.

        Duplicates GetNames()."""
        return self.GetNames()

    def WriteConfig(self):
        """Writes the entire list to config."""
        config.SetPath(CLUSTER_CONFIG)
        for location in self.cluster:
            config.SetPath(location)
            config.Write("location", location)
            config.SetPath('..')
        config.SetPath('..')

class ClusterWindow(wx.Dialog):
    """A window for editing a list of clustered computers.

    Functions:
        __init__(parent, ID, title, L, cursor=0)
        DisplayJconfFile(event)
        DeleteEnabledCheck
        Update(cursor)
        AddNew(event)
        Delete(event)
        Rename(event)
        OnClose(event)
    """
    def __init__(self, parent, D, clusterMaster,
                 ID = -1, title = "Cluster Settings"):
        """Sets up the Jconf window.

        Keyword arguments:
        D: The linked Cluster dictionary this window modifies.
        clusterMaster: Name of the cluster's Master.
        """
        wx.Dialog.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.cDict = D
        self.clusterMaster = clusterMaster
        ##Build displays.
        self.clustList = wx.ListBox(self, -1,
                                         size=JCONF_LIST_DISPLAY_MIN_SIZE)
        ##Build Add & Delete buttons.
        bAdd = wx.Button(self, -1, "Add")
        bAdd.SetToolTip(wx.ToolTip("Add a slave listing."))
        self.bDelete = wx.Button(self, -1, "Delete")
        self.bDelete.SetToolTip(wx.ToolTip("Delete a slave listing."))
        ##Build master display.
        self.masterCtrl = wx.TextCtrl(self, -1)
        self.masterCtrl.SetValue(self.clusterMaster)
        self.masterCtrl.SetToolTip(wx.ToolTip("Name the master computer."))
        ##Build OK button.
        bOk = wx.Button(self, -1, "Ok")
        bOk.SetToolTip(wx.ToolTip("Return to Settings."))
        ##Fill in info.
        self.Update()
        ##Check if Delete's enabled.
        self.DeleteEnabledCheck()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.AddNew, bAdd)
        self.Bind(wx.EVT_BUTTON, self.Delete, self.bDelete)
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        ##Add/Rename/Delete buttons.
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.AddMany([bAdd, VERTICAL_SPACE,
                          self.bDelete])
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        ##List field + buttons.
        columnSizer.Add(self.clustList, 1, wx.EXPAND)
        columnSizer.AddMany([HORIZONTAL_SPACE, rowSizer])
        ##List field + Path display
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.Add(wx.StaticText(self, -1, "Slave names:"))
        rowSizer.Add(columnSizer, 1, wx.EXPAND)
        rowSizer.AddMany([VERTICAL_SPACE,
                          wx.StaticText(self, -1, "Master's name:")])
        rowSizer.Add(self.masterCtrl, 0, wx.EXPAND)
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer, 1, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(bOk, 0, wx.EXPAND)
        ##Set size, position.
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.SetSize(INITIAL_JCONF_WINDOW_SIZE)
        self.CenterOnParent(wx.BOTH)
        ##Set the background color.
        Style(self)

    def DeleteEnabledCheck(self):
        """Disables/Enables the Delete button based on number of entries.

        Disabled if entries <= 1
        Enabled if entries > 1"""
        if self.cDict.Length() <= 0:
            self.bDelete.Enable(False)
        else:
            self.bDelete.Enable(True)
        
    def Update(self, cursor = ""):
        """Updates the shown entries list & checks to match recent changes."""
        ##Set cursor if it's blank.
        if cursor == "":
            cursor = self.clustList.GetStringSelection()
        nameList = self.cDict.GetNames()
        self.clustList.Set(nameList)
        if cursor == wx.NOT_FOUND or self.clustList.GetCount() == 0:
            self.clustList.SetSelection(wx.NOT_FOUND)
        elif self.clustList.FindString(str(cursor)) == wx.NOT_FOUND:
            self.clustList.SetSelection(wx.NOT_FOUND)
        else:
            self.clustList.SetStringSelection(cursor)

    def AddNew(self, event):
        """User chooses a new cluster computer to add to the list.

        Default name: Address of cluster computer."""
        loop = True
        while loop:
            dlg = wx.TextEntryDialog(self,
                                     "Please enter the name of the computer:",
                                     "Add Cluster Computer")
            if dlg.ShowModal() == wx.ID_OK:
                location = dlg.GetValue()
                dlg.Destroy()
                ##Reject if it's empty.
                if location.isspace() or location == '':
                    dlg = wx.MessageDialog(self,
                                           "Your name is empty." + \
                                           " Please try again.",
                                           "ERROR: Name is Empty",
                                           wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()
                ##Reject if it has slashes.
                elif '/' in location or '\\' in location:
                    dlg = wx.MessageDialog(self,
                                           "Your name has slashes in it.\n" + \
                                           "Please try again.",
                                           "ERROR: Name Contains Slashes",
                                           wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()                
                ##Return if this location's already listed.
                elif location in self.cDict.GetLocations():
                    dlg = wx.MessageDialog(self,
                                           "[%s] is already in the" %(location)+
                                           " cluster list.\n" +
                                           "You don't need to add it again.",
                                           "ERROR: Computer Already in List",
                                           wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()
                    return
                else:
                    loop = False
                    self.cDict.Add(location)
                    self.Update()
                    self.DeleteEnabledCheck()
            else:
                loop = False
                dlg.Destroy()

    def Delete(self, event):
        """Deletes the selected entry from the list.

        Also moves the selection index if it would be off the list."""
        ##Error catch if nothing's selected.
        if self.clustList.GetStringSelection() == "":
            dlg = wx.MessageDialog(self, "Can't delete; nothing is selected.",
                                   "Deletion Error: Nothing Selected",
                                   wx.OK)
            dlg.ShowModal()
            return
        ##Confirm delete.
        dlg = wx.MessageDialog(self,
                               "Are you sure you want to delete" +
                               " %s?" %(self.clustList.GetStringSelection()),
                               "Confirm Deletion",
                               wx.YES_NO | wx.NO_DEFAULT)
        dlg.CenterOnParent(wx.BOTH)
        if dlg.ShowModal() == wx.ID_YES:
            name = self.clustList.GetStringSelection()
            self.cDict.Delete(name)
            ##Move the cursor if it wouldn't be on the list anymore.
            cursor = self.clustList.GetSelection()
            if len(self.cDict) == 0:
                cursor = wx.NOT_FOUND
            elif cursor >= len(self.cDict):
                cursor = self.clustList.GetString(len(self.cDict) - 1)
            else:
                cursor = self.clustList.GetString(cursor + 1)
            self.Update(cursor)
            self.DeleteEnabledCheck()

    def OnClose(self, event):
        """Closes ClusterWindow."""
        self.GetParent().clusterMaster = self.masterCtrl.GetValue()
        self.Hide()
        self.Destroy()


class ServerKillWindow(wx.Frame):
    """A window to kill the Nameserver after launch."""
    def __init__(self, parent = None, title = "Kill Name Server"):
        """Creates the Server Kill Window."""
        wx.Frame.__init__(self, parent, wx.ID_ANY, title,
                          style = wx.DEFAULT_FRAME_STYLE &
                          ~ (wx.RESIZE_BORDER | wx.CLOSE_BOX | wx.MAXIMIZE_BOX))
        lblMsg = wx.StaticText(self, -1, "After you're done with VE-Suite,\n"+\
                                         "press the button below to kill\n"+\
                                         "the Name Server.")
        bDone = wx.Button(self, -1, "Kill Name Server")
        self.Bind(wx.EVT_BUTTON, self.KillNameserver, bDone)
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        border = 10
        rowSizer.Add(lblMsg, 2, wx.ALL, border)
        rowSizer.Add(bDone, 1, wx.EXPAND)
        rowSizer.SetMinSize(KILL_WINDOW_SIZE)
        rowSizer.SetSizeHints(self)
        Style(self)
        self.SetSizer(rowSizer)
        self.SetSize(KILL_WINDOW_SIZE)
        self.CentreOnScreen()
        self.Show()
    
    def KillNameserver(self, event):
        """Kills any Nameservers running on this computer."""
        if windows:
            os.system("tskill Naming_Service")
            os.system("tskill WinServerd")
        elif unix:
            os.system("killall Naming_Service Exe_server")
        self.OnClose("this event doesn't exist")

    def OnClose(self, event):
        """Closes ServerKillWindow."""
        self.Hide()
        self.Destroy()
        

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
                 taoMachine = "localhost", taoPort = "1239",
                 desktopMode = False,
                 dependenciesDir = None, cluster = None, master = None,
                 shell = False, builderDir = None):
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
        ##The launch is the final step.
        ##Destroy launcher window before beginning the actual launch.
##        if launcherWindow != None:
##            launcherWindow.Close()
        ##Set self.cluster to True if there's cluster functionality.
        ##If so, begin building self.clusterScript
        ##Used in EnvSetup and Windows/Unix.
        if runXplorer and typeXplorer == 2 and cluster != None and \
           CLUSTER_ENABLED:
            self.cluster = True
            ##Set up beginning of clusterScript for env setting.
            self.clusterScript = "#!/bin/csh\n"
            self.clusterScript += "ssh $1 << EOF\n"
            self.clusterScript += "setenv PYTHONPATH %s\n" \
                                  %(os.getenv("PYTHONPATH"))
        else:
            self.cluster = False
        ##Get dependenciesDir for setting environmental variables.
        if dependenciesDir == None:
            dependenciesDir = config.Read("DependenciesDir", "ERROR")
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
                w, h = wx.DisplaySize()
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
            command = 'python velauncher.py -x %s' %(xplorerType) + \
                      ' -j "%s" -t %s -p %s' %(jconf, taoMachine, taoPort) + \
                      ' -w %s -e %s -m %s' %(workDir, depsDir, clusterMaster)
            self.clusterScript += "cd %s\n" %(VELAUNCHER_DIR)
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
            ##Append argument if desktop mode selected
            desktop = ""
            if desktopMode:
                w, h = wx.DisplaySize()
                desktop = "-VESDesktop %s %s" % (w, h)
            ##Set Xplorer's type
            if typeXplorer == 0: ##OSG selection
                executable = "project_tao_osg"
            elif typeXplorer == 1: ##OSG VEP selection
                executable = "project_tao_osg_vep"
            elif typeXplorer == 2: ##OSG VEPC selection
                executable = "project_tao_osg_vep_cluster"
            ##Xplorer's call
            os.system("%s -ORBInitRef NameService=" %(executable) +
                      "corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService " +
                      '"%s" %s &' %(jconf, desktop))
        print "Finished sending launch commands."
        return

    def KillNameserver(self):
        """Kills any Name Servers running on this computer."""
        print "Killing any previous name servers."
        if windows:
            os.system("tskill Naming_Service")
            os.system("tskill WinServerd")
        elif unix:
            os.system("killall Naming_Service Exe_server")
        print "Previous name servers killed."

    def Shell(self):
        if windows:
            os.system("""start "%s" cmd""" % BUILDER_SHELL_NAME)
        elif unix:
            print "VE-Suite shell started."
            os.system("%s &" % UNIX_SHELL)
        else:
            print "ERROR! This OS isn't supported."

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
        if devMode:
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
                modifiedVar = modifiedVar + sep + app
        os.environ[var] = modifiedVar
        ##Put var in clusterScript
        if self.cluster:
            self.clusterScript += "setenv %s %s\n" %(var, os.getenv(var))
##        print var + ": " + os.getenv(var) ##TESTER

    def EnvFill(self, var, default):
        """Overwrites environmental var in normal mode, fills it in dev mode.

        Does not overwrite a filled variable in devMode.
        Overwrites a filled variable in normal mode."""
        if devMode:
            os.environ[var] = os.getenv(var, default)
        else:
            os.environ[var] = default
        ##Put var in clusterScript
        if self.cluster:
            self.clusterScript += "setenv %s %s\n" %(var, os.getenv(var))
##        print var + ": " + os.getenv(var) ##TESTER


##Get & clean up command line arguments.
arguments = sys.argv[1:]
try:
    opts, args = getopt.getopt(arguments,
                               "cnx:kj:t:p:w:e:m:sb:",
                               ["conductor", "nameserver", "xplorer=",
                                "desktop", "jconf=", "taomachine=", "port=",
                                "dir=", "dep=", "master=", "dev", "shell",
                                "builder="])
except getopt.GetoptError:
    usage()
    sys.exit(2)
##Window boot
if len(opts) == 0 or (len(opts) == 1 and opts[0] == ("--dev", "")):
    if len(opts) == 1 and opts[0] == ("--dev", ""):
        devMode = True
    app = wx.PySimpleApp()
    frame = LauncherWindow(None,-1,'VE Suite Launcher')
    app.MainLoop()
    ##Delete the config link to avoid memory leakage.
    del config
##Command line boot
##Takes arguments passed, uses defaults for the rest, launches immediately.
else:
    CommandLaunch(opts)


