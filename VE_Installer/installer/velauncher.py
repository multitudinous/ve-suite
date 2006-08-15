#!/usr/bin/env python
"""Takes VE-Suite settings input and launches chosen VE-Suite programs.

--v1.0.2 coded by Jeff Groves
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
import sys ##Gets command line arguments
import getopt ##Cleans up command line arguments
import wx ##Used for GUI

##Classes:
##velauncher: Main window and boot code.
##velBase: Contains constants and shared functions. 
##velSettingWin: Settings Window.
##velJconfDict: Jconfiguration data structure.
##velClusterDict: Cluster slave data structure.
##velJconfWindow: Jconfiguration Window.
##velClusterWindow: Cluster Window.
##velDependencies: Contains code for checking & choosing the Dependencies.
##velLaunchCode: Contains environmental var settings & OS-specific launch code.
##velServerKillWindow: Server Kill Window.
from velBase import *
from velJconfDict import *
from velClusterDict import *
from velJconfWindow import *
from velClusterWindow import *
from velLaunchCode import *
from velSettingWin import *
from velCommandLaunch import *
from velServerKillWindow import *
import velDependencies

##Set up the config
config = wx.Config(CONFIG_FILE)
config.SetPath(DEFAULT_CONFIG)
wx.Config.Set(config)

class LauncherWindow(wx.Frame):
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
        wx.Frame.__init__(self, parent, -1, title,
                          style = wx.DEFAULT_FRAME_STYLE &
                          ~ (wx.RESIZE_BORDER | wx.RESIZE_BOX |
                          wx.MAXIMIZE_BOX))

        ##Prepare data storage
        ##NOTE: JconfDict is a local copy of the Jconf list stored in the
        ##program's config. Changes to JconfDict are mirrored in the config.
        self.conductor = False
        self.nameServer = False
        self.xplorer = False
        self.xplorerType = 0
        self.desktop = False
        self.JconfDict = None
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
        ##Build Launch button.
        self.bLaunch = wx.Button(self, -1, "Launch VE Suite")
        self.bLaunch.SetToolTip(wx.ToolTip("Run the programs you selected and" +
                                      " close the Launcher."))
        ##Build menu bar
        menuBar = wx.MenuBar()
        menu = wx.Menu()
        menu.Append(500, "Change De&pendencies\tCtrl+P")
        if devMode:
            ##Disable Change Dependencies in devMode.
            menu.Enable(500, False)
        menu.Append(wx.ID_EXIT, "&Quit\tCtrl+Q")
        menuBar.Append(menu, "&File")
        menu = wx.Menu()
        menu.Append(510, "&Load\tCtrl+L")
        menu.Append(511, "&Save\tCtrl+S")
        menu.AppendSeparator()
        menu.Append(512, "&Delete\tCtrl+D")
        menuBar.Append(menu, "&Configurations")
        self.SetMenuBar(menuBar)

        ##Event bindings.
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        self.Bind(wx.EVT_BUTTON, self.ChooseDirectory, bDirectory)
        self.Bind(wx.EVT_BUTTON, self.Launch, self.bLaunch)
        self.Bind(wx.EVT_BUTTON, self.Settings, self.bCustom)
        self.Bind(wx.EVT_RADIOBOX, self.ModeChanged, self.rbMode)
        self.Bind(wx.EVT_MENU, self.DependenciesChange, id = 500)
        self.Bind(wx.EVT_MENU, self.ChooseLoadConfig, id = 510)
        self.Bind(wx.EVT_MENU, self.ChooseSaveConfig, id = 511)
        self.Bind(wx.EVT_MENU, self.DeleteConfig, id = 512)
        self.Bind(wx.EVT_MENU, self.OnClose, id = wx.ID_EXIT)
        
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
        rowSizer.AddMany([VERTICAL_SPACE,
                          columnSizer])
        ##Add the title graphic space
        rowSizer2 = wx.BoxSizer(wx.VERTICAL)
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(sbmLogo)
        columnSizer.Add(HORIZONTAL_SPACE)
        rowSizer2.Add(columnSizer)
        rowSizer2.Add(VERTICAL_SPACE)
        rowSizer2.Add(rowSizer, 0, wx.EXPAND)
        ##Set the main sizer, add Launch button.
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer2, 0, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(self.bLaunch, 1, wx.EXPAND)
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        ##Set the background color.
        Style(self)
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
                self.DependenciesChange("")
            else:
                legitDeps = velDependencies.Check(dependenciesDir)
                if not legitDeps:
                    self.DependenciesChange("")
            self.dependencies = config.Read("DependenciesDir", ":::")
        ##Restore config values from last time.
        self.LoadConfig(DEFAULT_CONFIG)
        ##Show the window.
        self.Show(True)
        ##ERROR CHECK: Is there a /bin folder in the launcher's directory?
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


    def DependenciesChange(self, event):
        newDeps = velDependencies.Change(self)
        if newDeps != None:
            config.Write("DependenciesDir", newDeps)
            self.dependencies = newDeps

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
            xplorerConfig = self.JconfDict.GetPath(self.jconfSelection)
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
        self.JconfDict.WriteConfig()
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
        ##Sets txDirectory's cursor to end in Unix systems for easier reading.
        ##Acts strange in Windows for some reason; investigate.
        if unix:
            self.txDirectory.SetInsertionPointEnd()
        ##Set Cluster list.
        self.clusterDict = ClusterDict()
        ##Set ClusterMaster
        self.clusterMaster = config.Read("ClusterMaster", "")
        ##Set choices for Jconf list.
        defaultSelection = "None"
        if config.HasGroup(JCONF_CONFIG):
            self.JconfDict = JconfDict()
        ##Set default choices if JCONF_CONFIG doesn't exist,
        ##but DependenciesDir does.
        elif config.Read("DependenciesDir", ":::") != ":::":
            p = DEFAULT_JCONF
            config.SetPath(JCONF_CONFIG)
            defaultSelection = os.path.split(p)[1][:-6]
            config.Write(defaultSelection, p)
            config.SetPath('..')
            self.JconfDict = JconfDict()
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
            self.JconfDict = JconfDict()
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
        frame = SettingsWindow(self, self.JconfDict, self.jconfSelection,
                                     self.clusterDict, self.clusterMaster,
                                     self.desktop, self.nameServer,
                                     self.conductor, self.xplorer,
                                     self.xplorerType,
                                     modeRules, mode,
                                     position = position)
        frame.ShowModal()

    def Launch(self, event):
        """Checks input, begins launch if error-free."""
        ##Set mode
        mode = self.rbMode.GetStringSelection()
        if mode in MODE_DICT:
            modeRules = MODE_DICT[mode]
        else:
            modeRules = {}
        ##Set variables
        dependenciesDir = config.Read("DependenciesDir", "ERROR")
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
               desktop, dependenciesDir = dependenciesDir,
               cluster = self.clusterDict.GetLocations(),
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
        ##Run VE-Suite in dev mode? Turned to True if --dev passed.
        devMode = True
    else:
        devMode = False
    app = wx.PySimpleApp()
    frame = LauncherWindow(None,-1,'VE Suite Launcher')
    app.MainLoop()
    ##Delete the config link to avoid memory leakage.
    del config
##Command line boot
##Takes arguments passed, uses defaults for the rest, launches immediately.
else:
    CommandLaunch(opts)


