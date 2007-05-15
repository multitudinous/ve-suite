#!/usr/bin/env python
"""Takes VE-Suite settings input and launches chosen VE-Suite programs.

--v1.0.5 coded by Jeff Groves
This module creates a launcher window so the user can choose which
parts of VE-Suite to launch:
-Name Server
-Conductor
-Xplorer (user also chooses format and configuration)
The user can also select VE-Suite's working & builder directories,
launch a shell with the VE-Suite variables set.

When the user has decided the settings and hits the Launch button,
the module sets up the system's environmental variables and executes
the chosen programs. The launcher automatically quits after Launch.

The launcher is made for standard builds of VE-Suite. To launch a custom build
with it, create a batch/shell file to set the extra environmental variables,
executing the launcher on its last command in dev mode (--dev).
"""

import os ##Used for setting environmental variables, running programs
import sys ##Gets command line arguments
import getopt ##Cleans up command line arguments
import wx ##Used for GUI
import thread ##Used for the splash banner's thread

try:
    from velBase import *
except:
    sys.path.append(os.path.join(sys.path[0], 'python'))
    from velBase import *
#sys.path.append('python') ##Searches for other modules in python/
from velModes import *
from velCoveredConfig import *
import velDependencies
from velSaveLoadConfig import *
from velSaveConfigWindow import *
from velJconfDict import *
from velClusterDict import *
from velJconfWindow import *
from velClusterWindow import *
from velSettingWin import *
from velServerKillWindow import *
from velCommandLine import *
from velLaunchCode import *
import velLaunchSplash
from velDebugWindow import *
from velSetWaitWindow import *
import velShell
from velRecentFiles import *
from velDepsArray import *
from velDepsWindow import *

##Set up the master config file
config = wx.Config(CONFIG_FILE)
config.SetPath(DEFAULT_CONFIG)
wx.Config.Set(config)

##TESTERS
##print "Path: ", sys.path[0]
##print "Executable: ", sys.executable
##print "CWD: ", os.getcwd()
##print "AbsPath: ", os.path.abspath(sys.executable)
##print "Location of velauncher: ", VELAUNCHER_DIR

class LauncherWindow(wx.Frame):
    """Manages the launcher's main window and its data.

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
        __init__(parent, ID, title, arguments)
        DependenciesChange(event)
        BuilderChange([event])
        UpdateData([depDir])
        React
        UpdateDisplay
        ChooseDirectory([event])
        ChooseSaveConfig([event])
        ChooseLoadConfig([event])
        DeleteConfig([event])
        Settings([event])
        Launch([event])
        OnClose([event])        
    """
    def __init__(self, parent, ID, title, arguments, previousState = None):
        """Builds the launcher's window and loads the last configuration."""
        wx.Frame.__init__(self, parent, -1, title,
                          style = wx.DEFAULT_FRAME_STYLE &
                          ~ (wx.RESIZE_BORDER | wx.RESIZE_BOX |
                          wx.MAXIMIZE_BOX))

        if previousState:
            self.state = previousState
        else:
            ##Prepare data storage
            self.state = CoveredConfig()
            ##Restore config values from last time.
            LoadConfig(DEFAULT_CONFIG, self.state, loadLastConfig = True)
        self.launch = False
        ##Prepare the panel.
        panel = wx.Panel(self)
        ##Prepare the logo.
        bmLogo = wx.Bitmap(LOGO_LOCATION, wx.BITMAP_TYPE_XPM)
        sbmLogo = wx.StaticBitmap(panel, -1, bmLogo)
        ##Build file name display. Unnecessary now.
##        self.fileTypeText = wx.StaticText(self, -1, "TestLabel")
##        self.fileTypeText.SetToolTip(wx.ToolTip("Test."))
##        self.fileNameText = wx.StaticText(self, -1, "TestLabel")
##        self.fileNameText.SetToolTip(wx.ToolTip("Test."))
        ##Build Directory label.
        self.labelDirectory = wx.StaticText(panel, -1, "Working Directory:")
        ##Build Directory text ctrl.
        self.txDirectory = wx.TextCtrl(panel, -1,
                                       DIRECTORY_DEFAULT)
        self.txDirectory.SetToolTip(wx.ToolTip("The path of the" +
                                               " working directory."))
        ##Build Directory button.
        self.bDirectory = wx.Button(panel, -1, "Choose Working Directory")
        self.bDirectory.SetToolTip(wx.ToolTip("Choose the working directory" +
                                              "for the programs."))
        ##Build Tao Machine text ctrl.
        self.txTaoMachine = wx.TextCtrl(panel, -1)
        self.txTaoMachine.SetToolTip(wx.ToolTip("Enter the computing" +
                                                "engine's name."))
        ##Build Tao Port text ctrl.
        self.txTaoPort = wx.TextCtrl(panel, -1)
        self.txTaoPort.SetToolTip(wx.ToolTip("Enter the computing" +
                                             " engine's port."))
        ##Build Mode radio box.
        self.rbMode = wx.RadioBox(panel, -1, "Launch Mode",
                                  wx.DefaultPosition, wx.DefaultSize,
                                  MODE_LIST, 3, wx.RA_SPECIFY_COLS)
        self.rbMode.SetToolTip(wx.ToolTip("Choose which mode you want to" +
                                          " launch in?"))
        ##Build Mode Settings button.
        self.bCustom = wx.Button(panel, -1, "Mode Settings")
        self.bCustom.SetToolTip(wx.ToolTip("View and change settings for" +
                                           " the current mode."))
        ##Build Launch button.
        self.bLaunch = wx.Button(panel, -1, "Launch VE Suite")
        self.bLaunch.SetToolTip(wx.ToolTip("Run the programs you selected and" +
                                           " close the Launcher."))
        ##Build menu bar
        menuBar = wx.MenuBar()
        menu = wx.Menu()
        menu.Append(500, "&Open...\tCtrl+O")
        ##Recent files as submenu
        self.recentMenu = wx.Menu()
        self.ConstructRecentMenu()
        menu.AppendMenu(502, "Open &Recent File", self.recentMenu)
        menu.Append(501, "&Close File\tCtrl+W")
        menu.Append(wx.ID_EXIT, "&Quit\tCtrl+Q")
        ##Recent files as separated add-on
        ##menu.AppendSeparator()
        ##self.ConstructRecentMenu()
        menuBar.Append(menu, "&File")
        menu = wx.Menu()
        menu.Append(510, "&Load\tCtrl+L")
        menu.Append(511, "&Save\tCtrl+S")
        menu.AppendSeparator()
        menu.Append(512, "&Delete\tCtrl+D")
        menuBar.Append(menu, "&Configurations")
        menu = wx.Menu()
        menu.Append(520, "Choose De&pendencies\tCtrl+P")
        if not unix:
            menu.Append(521, "Choose &Builder Folder")
        menu.Append(522, "Set Debug& Levels\tCtrl+G")
        self.autoRunVes = wx.MenuItem(menu, 525, "&Auto-Launch Passed Files",
                                      kind = wx.ITEM_CHECK)
        menu.AppendItem(self.autoRunVes)
        menu.Append(523, "&Cluster Wait Times\tCtrl+C")
        menuBar.Append(menu, "&Options")
        self.SetMenuBar(menuBar)

        ##Event bindings.
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        self.Bind(wx.EVT_BUTTON, self.FileButtonBranch, self.bDirectory)
        self.Bind(wx.EVT_BUTTON, self.Launch, self.bLaunch)
        self.Bind(wx.EVT_BUTTON, self.Settings, self.bCustom)
        self.Bind(wx.EVT_RADIOBOX, self.UpdateData, self.rbMode)
        self.Bind(wx.EVT_MENU, self.DependenciesChange, id = 520)
        if not unix:
            self.Bind(wx.EVT_MENU, self.BuilderChange, id = 521)
        self.Bind(wx.EVT_MENU, self.ChooseLoadConfig, id = 510)
        self.Bind(wx.EVT_MENU, self.ChooseSaveConfig, id = 511)
        self.Bind(wx.EVT_MENU, self.DeleteConfig, id = 512)
        self.Bind(wx.EVT_MENU, self.OnClose, id = wx.ID_EXIT)
        self.Bind(wx.EVT_MENU, self.DebugOptions, id = 522)
        self.Bind(wx.EVT_MENU, self.WaitOptions, id = 523)
        self.Bind(wx.EVT_MENU, self.OpenFile, id = 500)
        self.Bind(wx.EVT_MENU, self.CloseFiles, id = 501)
        self.Bind(wx.EVT_MENU, self.UpdateData, id = 524)
        self.Bind(wx.EVT_MENU, self.UpdateData, id = 525)
        
        ##Layout format settings
        ##Create the overall layout box
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        ##Construct the Directory column
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(self.txDirectory, 1, wx.ALIGN_BOTTOM)
        columnSizer.AddMany([HORIZONTAL_SPACE,
                             self.bDirectory])
        ##Insert the Directory column.
        rowSizer.Add(self.labelDirectory)
        rowSizer.Add(columnSizer, 0, wx.EXPAND) 
        ##Construct the Tao column.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(wx.StaticText(panel, -1, "CE Name:"),
                        0, wx.ALIGN_CENTER_VERTICAL)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.txTaoMachine, 2)
        columnSizer.Add((HORIZONTAL_SPACE[0]*3, -1))
        columnSizer.Add(wx.StaticText(panel, -1, "CE Port:"),
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
        ##Unnecessary.
##        rowSizer3 = wx.BoxSizer(wx.VERTICAL)
##        rowSizer3.Add(self.fileTypeText, 0, wx.EXPAND)
##        rowSizer3.Add(self.fileNameText, 0, wx.EXPAND)
##        columnSizer.Add(rowSizer3, 1)
        rowSizer2.Add(columnSizer)
        rowSizer2.Add(VERTICAL_SPACE)
        rowSizer2.Add(rowSizer, 0, wx.EXPAND)
        ##Set the main sizer, add Launch button.
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer2, 0, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(self.bLaunch, 1, wx.EXPAND)
        mainSizer.SetSizeHints(self)
        panel.SetSizer(mainSizer)
        panel.Layout()
        ##Set the background color.
        Style(self)
        ##Set tool tip popup delay to 2 seconds.
        wx.ToolTip.SetDelay(2000)
        
        ##Set arguments for developer mode.
        if devMode:
            self.state.DevMode()
        ##Set arguments for passed .ves & script files.
        if arguments:
            self.state.InterpretArgument(arguments[0])
        self.React()
        ##Check for auto-run ves file.
##        if arguments and self.state.getSurface("AutoRunVes") \
##           and self.state.getSurface("VESFile"):
##            self.Launch()
        ##Check the dependencies.
        if windows and not devMode:
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
        elif unix and not devMode:
            if len(self.state.GetSurface("Dependencies").GetNames()) == 0 \
               and self.state.GetSurface("JugglerDep") == None and \
               os.getenv("VJ_BASE_DIR") == None:
                dlg = wx.MessageDialog(None,
                                       "Welcome to VE Suite!\n" +
                                       "Before you can begin, please select" +
                                       " the Dependencies directories you" +
                                                       " will be using.",
                                       "Welcome to VE Suite", wx.OK)
                dlg.ShowModal()
                dlg.Destroy()
                self.DependenciesChange("")
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

    def OpenFile(self, event = None):
        self.UpdateData()
        types = "VES files (*.ves)|*.ves"
        if windows:
            types += "|Batch files (*.bat)|*.bat"
        elif unix:
            types += "|Script files (*.sh;*.tsh)|*.sh;*.tsh"
        dlg = wx.FileDialog(self,
                            "Choose a file.",
                            defaultDir = self.state.GetSurface("Directory"),
                            wildcard = types,
                            style=wx.OPEN)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            self.state.InterpretArgument(path)
        dlg.Destroy()
        self.UpdateDisplay()
        return

    def CloseFiles(self, event = None):
        """Closes any VES/script files the user has slated to run."""
        if not self.state.GetSurface("VESFile") and \
           not self.state.GetSurface("ShellScript"):
            ##No files to close notification.
            dlg = wx.MessageDialog(self,
                                   "You don't have any files opened.",
                                   "No Files to Close", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        else:
            confirm = wx.MessageDialog(self,
                                       "Are you sure you want to\n" +
                                       "close any .ves or script\n" +
                                       "files you have opened?",
                                       "Confirm File Close",
                                       wx.YES_NO | wx.YES_DEFAULT)
            if confirm.ShowModal() == wx.ID_YES:
                self.state.InterpretArgument(None)
                self.UpdateDisplay()
            confirm.Destroy()
        return

    def DebugOptions(self, event = None):
        DebugWindow(self, self.state)

    def WaitOptions(self, event = None):
        SetWaitWindow(self, self.state)

    def DependenciesChange(self, event = None):
        """Asks the user to choose a new Dependencies folder."""
        if windows:
            newDeps = velDependencies.Change(self)
            if newDeps != None:
                config.Write("DependenciesDir", newDeps)
                self.UpdateData(depDir = newDeps)
        else:
            depWindow = DepsWindow(self, self.state)
            depWindow.ShowModal()

    def BuilderChange(self, event = None):
        """Asks the user to choose a new Builder folder."""
        ##Ask for the builder directory.
        startBuilderDir = self.state.GetBase("BuilderDir")
        if startBuilderDir == None:
            startBuilderDir = VELAUNCHER_DIR
        dlg = wx.DirDialog(None,
                           "Choose the VE-Builder directory:",
                           startBuilderDir,
                           style=wx.DD_DEFAULT_STYLE)
        choice = dlg.ShowModal()
        builderDir = dlg.GetPath()
        dlg.Destroy()
        if choice == wx.ID_OK:
            ##If a directory's chosen, change it and return True.
            self.state.Edit("BuilderDir", builderDir)
            return True
        else: ##If not, return False.
            return False        
        
    def UpdateData(self, event = None, depDir = None):
        """Saves the user's input to the launcher's data.

        Will React & UpdateDisplay if mode's changed.
        Call before data is saved to file."""
        ##NOTE: Will have to change way user's variables are saved if 
        ##modes allow users to change these in the future.
        ##Probably by grabbing the oldMode and checking its settings.
        react = False
        ##DependenciesDir
        if depDir != None:
            self.state.Edit("DependenciesDir", depDir)
        ##Directory
        if self.txDirectory.IsEnabled() and not self.state.FileLoaded():
            self.state.Edit("Directory", self.txDirectory.GetValue())
        ##Tao Machine
        if self.txTaoMachine.IsEnabled():
            self.state.Edit("TaoMachine", self.txTaoMachine.GetValue())
        ##Tao Port
        if self.txTaoPort.IsEnabled():
            self.state.Edit("TaoPort", self.txTaoPort.GetValue())
        ##Mode
        if self.rbMode.IsEnabled():
            modeChosen = self.rbMode.GetSelection()
            if modeChosen != self.state.GetBase("Mode"):
                self.state.Edit("Mode", modeChosen)
                react = True
        ##Auto-Run Ves Files Mode
        if self.autoRunVes.IsEnabled():
            self.state.Edit("AutoRunVes", self.autoRunVes.IsChecked())
        ##React, then Update Display
        if react:
            self.React()
        return

    def React(self):
        """Covers/uncovers data based on user input."""
        ##Change Mode cover.
        mode = MODE_LIST[self.state.GetSurface("Mode")]
        self.state.ChangeMode(mode)
        ##UpdateDisplay
        self.UpdateDisplay()
        return
    
    def UpdateDisplay(self):
        """Changes GUI to match changes made by React."""
        ##RecentFiles menu
        ##self.recentMenu.Enable(not self.state.GetSurface("RecentFiles").IsEmpty())
        ##DependenciesDir
        self.GetMenuBar().Enable(520, self.state.IsEnabled("DependenciesDir"))
        ##BuilderDir
        if not unix:
            self.GetMenuBar().Enable(521, self.state.IsEnabled("BuilderDir"))
        ##Directory
        if self.state.GetSurface("VESFile") != None:
            self.labelDirectory.SetLabel("VES File Loaded:")
            self.txDirectory.SetValue(self.state.GetSurface("VESFile"))
            self.txDirectory.SetEditable(False)
            self.txDirectory.SetBackgroundColour(READONLY_COLOR)
            self.bDirectory.SetLabel("Close File")
            self.bDirectory.SetToolTip(wx.ToolTip("Close this file."))
            self.bDirectory.Enable(self.state.IsEnabled("Directory"))
        elif self.state.GetSurface("ShellScript") != None:
            self.labelDirectory.SetLabel("Shell Script:")
            self.txDirectory.SetValue(self.state.GetSurface("ShellScript"))
            self.txDirectory.SetEditable(False)
            self.txDirectory.SetBackgroundColour(READONLY_COLOR)
            self.bDirectory.SetLabel("Close File")
            self.bDirectory.SetToolTip(wx.ToolTip("Close this file."))
            self.bDirectory.Enable(self.state.IsEnabled("Directory"))
        else:
            self.labelDirectory.SetLabel("Working Directory:")
            self.txDirectory.SetValue(self.state.GetSurface("Directory"))
            self.txDirectory.SetEditable(True)
            self.txDirectory.SetBackgroundColour(wx.NullColour)
            self.txDirectory.Enable(self.state.IsEnabled("Directory"))
            self.bDirectory.SetLabel("Choose Working Directory")
            self.bDirectory.SetToolTip(wx.ToolTip("Choose the working" +
                                                  " directory for the" +
                                                  " programs."))
            self.bDirectory.Enable(self.state.IsEnabled("Directory"))
        ##TaoMachine
        self.txTaoMachine.SetValue(self.state.GetSurface("TaoMachine"))
        self.txTaoMachine.Enable(self.state.IsEnabled("TaoMachine"))
        ##TaoPort
        self.txTaoPort.SetValue(self.state.GetSurface("TaoPort"))
        self.txTaoPort.Enable(self.state.IsEnabled("TaoPort"))
        ##Mode
        self.rbMode.SetSelection(self.state.GetSurface("Mode"))
        self.rbMode.Enable(self.state.IsEnabled("Mode"))
        ##VES/script files.
        fileLoaded = self.state.GetSurface("VESFile") or \
                     self.state.GetSurface("ShellScript")
        ##Remove Files menu.
        self.GetMenuBar().Enable(501, bool(fileLoaded))
        ##AutoRun Ves menu.
        self.autoRunVes.Enable(self.state.IsEnabled("AutoRunVes"))
        self.autoRunVes.Check(self.state.GetSurface("AutoRunVes"))
        ##Loaded file name. Under work.
##        if self.state.GetSurface("VESFile"):
##            self.fileTypeText.SetLabel("VES File:")
##            fileName = os.path.basename(self.state.GetSurface("VESFile"))
##            self.fileNameText.SetLabel(fileName)
##        elif self.state.GetSurface("ShellScript"):
##            self.fileTypeText.SetLabel("Script File:")
##            fileName = os.path.basename(self.state.GetSurface("ShellScript"))
##            self.fileNameText.SetLabel(fileName)
##        else:
##            self.fileTypeText.SetLabel("")
##            self.fileNameText.SetLabel("")            
        return

    def ConstructRecentMenu(self, event = None):
        """Constructs the recent items menu.
        NOTE: IDs idStart+ used for these."""
        self.recentMenu = wx.Menu()
        self.recentArchive = []
        nameList = self.state.GetSurface("RecentFiles").GetNames()
        currentId = RECENT_MENU_ID
        ##If no recent items, put a disabled None in the menu.
        if len(nameList) == 0:
            self.recentMenu.Append(currentId, "None")
            self.recentMenu.Enable(currentId, False)
            return
        ##Else put every name in the list in the menu.
        for i in range(len(nameList)):
            self.recentMenu.Append(currentId, nameList[i])
            self.recentArchive.append(self.state.GetSurface("RecentFiles").GetPath(i))
            self.Bind(wx.EVT_MENU, self.ChooseRecentFile, id = currentId)
            currentId += 1
        return

    def ChooseRecentFile(self, event):
        """Gets the path for the Recent Files item chosen
        and loads it into the program."""
        placeChosen = event.GetId() - RECENT_MENU_ID
        path = self.recentArchive[placeChosen]
        self.state.InterpretArgument(path)
        self.React()
        return

    def DeleteRecentMenuItem(self, badFile):
        """Deletes a non-existant recent file."""
        itemID = RECENT_MENU_ID + self.recentArchive.index(badFile)
        self.recentMenu.Delete(itemID)
        
##UNDER CONSTRUCTION
##    def ClearRecentMenu(self, event = None):
##        """Clears the recent items menu."""
##        self.recentMenu = wx.Menu()
##        self.recentMenu.
##
##    def Append

    def FileButtonBranch(self, event = None):
        """Goes to Close Files function if VES/Script file's loaded,
        ChooseDirectory if a file isn't loaded."""
        if self.state.GetSurface("VESFile") or \
           self.state.GetSurface("ShellScript"):
            self.CloseFiles()
        else:
            self.ChooseDirectory()

    def ChooseDirectory(self, event = None):
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
        self.UpdateData()

    def ChooseSaveConfig(self, event = None):
        """Lets the user choose which name to save a configuration under."""
        self.UpdateData()
        dlg = SaveConfigWindow(self, self.state)
        dlg.ShowModal()

    def ChooseLoadConfig(self, event = None):
        """Lets the user choose a configuration to load."""
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
            LoadConfig(choice, self.state)
            self.React()
        dlg.Destroy()

    def DeleteConfig(self, event = None):
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
            ##Delete the config if confirmed.
            if confirm.ShowModal() == wx.ID_YES:
                config.SetPath("..")
                config.DeleteGroup(choice)
                config.SetPath(DEFAULT_CONFIG)
            confirm.Destroy()
        dlg.Destroy()

    def Settings(self, event = None):
        """Launches the Custom Settings window."""
        x, y = self.GetPosition()
        x2, y2 = self.rbMode.GetPosition()
        x3 = self.rbMode.GetSize()[0]
        ##Sets the upper-left corner of the Settings window to be just
        ##right of the Mode radio box. Is nudged to avoid covering
        ##up important info.
        OFFSET = 20 ##Pixels
        position = wx.Point(x + x2 + x3 + OFFSET,
                            y + y2 + OFFSET)
        frame = SettingsWindow(self, self.state, position = position)
        frame.ShowModal()

    def Launch(self, event = None):
        """Checks input, begins launch if error-free."""
        self.UpdateData()
        self.launch = True
        ##Launch data retrieved from the Surface;
        ##Save data retrieved from the Base. (See velCoveredState.)
        v = self.state.GetSurface
        ##ERROR CHECK:  Are any programs selected?
        ##              If not, abort launch.
        if not (v("Conductor") or v("NameServer")
                or v("Xplorer") or v("Shell")):
            dlg = wx.MessageDialog(self,
                                   "The launch won't do anything because you"+
                                   " haven't chosen any programs to launch.\n"+
                                   "Please choose some programs to launch" +
                                   " and try again.",
                                   "Launch Error: No Program Selected", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            self.Settings()
            return
        ##ERROR CHECK:  Is the Tao Machine name blank?
        ##              If so, abort launch.
        taoMachine = v("TaoMachine")
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
        if not (v("TaoPort").isdigit() and
                int(v("TaoPort")) <= 65535):
            dlg = wx.MessageDialog(self,
                                   "You have entered an illegal CE Port.\n" +
                                   "Please enter a port between 0 and" +
                                   " 65535.",
                                   "Launch Error: Illegal CE Port", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        ##ERROR CHECK:  Does the file chosen exist? If not, remove it.
        if (v("VESFile") != None and not (os.path.exists(v("VESFile")))) or \
           (v("ShellScript") != None and not (os.path.exists(v("ShellScript")))):
            if v("VESFile") != None:
                badFile = v("VESFile")
                self.state.SetVesFile(None)
            elif v("ShellScript") != None:
                badFile = v("ShellScript")
                self.state.SetScript(None)
            else:
                badFile = None
            badFileName = os.path.basename(badFile)
            dlg = wx.MessageDialog(self,
                                   "The %s file could not be found.\n" % (badFileName) +
                                   "Either the path is incorrect or the file\n" +
                                   "has been moved.\n" +
                                   "\n" +
                                   "This file has been removed from the launcher;\n" +
                                   "please choose another one.",
                                   "Launch Error: Loaded File Doesn't Exist",
                                   wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            if self.state.IsEnabled("RecentFiles"):
                self.state.GetBase("RecentFiles").Delete(badFile)
            self.DeleteRecentMenuItem(badFile)
            self.UpdateDisplay()
            return
        ##ERROR CHECK:  Does the working directory chosen exist?
        ##              If not, let the user change it.
        if not (os.path.exists(v("Directory"))):
            dlg = wx.MessageDialog(self,
                                   "The working directory you chose,\n" +
                                   "%s,\n" %(v("Directory")) +
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
                if not (os.path.exists(v("Directory"))):
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
                    return
            ##If user chooses NO, abort the launch.
            else:
                dlg.Destroy()
                return
        ##ERROR CHECK:  Does the selected Jconf file exist?
        ##              If not, abort the launch.
        jconfPath = v("JconfDict").GetPath(v("JconfSelection"))
        if v("Xplorer") and not (os.path.exists(jconfPath)):
            dlg = wx.MessageDialog(self,
                                   "The Xplorer configuration file you chose,"+
                                   "\n%s,\n" %(jconfPath) +
                                   " doesn't exist.\n" +
                                   "Please select a different one.",
                                   "Launch Error: Jconf File Doesn't Exist",
                                   wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        ##Set the builderDir, if necessary.
        passedBuilderDir = None
##        if v("Shell") and v("BuilderShell") == None:
        if v("Shell") and v("BuilderDir") == None and \
           self.state.IsEnabled("BuilderDir"):
            dlg = wx.MessageDialog(self,
                                    "Do you want to use this shell\n" +
                                    "to run VE-Builder?",
                                    "Create a VE-Builder Shell?",
                                    wx.YES_NO)
            choice = dlg.ShowModal()
            dlg.Destroy()
            if choice == wx.ID_YES:
                while v("BuilderDir") == None:
                    ##Force the user to choose a Builder directory if he
                    ##hasn't chosen one yet.
                    dirChosen = self.BuilderChange()
                    if dirChosen:
                        pass
                    else:
                        dlg = wx.MessageDialog(self,
                                               "You didn't choose a\n" +
                                               "directory for the Builder.\n" +
                                               "Please choose one.",
                                               "Error: No Directory Chosen",
                                                wx.OK)
                        dlg.ShowModal()
                        dlg.Destroy()
            elif choice == wx.ID_NO:
                ##Hide the BuilderDir
                self.state.React(True, "BuilderDir", None)
            else:
                return
        ##If the DependenciesDir is generic, change it to system-specific.
        alternateDep = velDependencies.GetSpecific(v("DependenciesDir"))
        self.state.React(alternateDep, "DependenciesDir", alternateDep)
        ##Hide the Launcher.
        self.Hide()
        ##Add any user-chosen launched files to RecentFiles list.
        fileVariables = ["ShellScript", "VESFile"]
        for fileVar in fileVariables:
            if self.state.IsEnabled(fileVar) and v(fileVar) != None:
                self.state.GetBase("RecentFiles").Add(v(fileVar))
        ##Save data before launching.
        self.UpdateData()
        SaveConfig(DEFAULT_CONFIG, self.state, saveLastConfig = True)
        ##Launch splash screen
        velLaunchSplash.LaunchSplash()
        ##thread.start_new_thread(velLaunchSplash.LaunchSplash, ())
        ##Go into the Launch
        try:
            launchInstance = Launch(self.state.GetLaunchSurface())
            ##Show NameServer kill window if NameServer was started.
            if v("NameServer"):
                window = ServerKillWindow(pids = launchInstance.GetNameserverPids())
        except QuitLaunchError:
            dlg = wx.MessageDialog(self,
                                   "Launch aborted by user.",
                                   "Launch Aborted",
                                   wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
        ##Close the Launcher
        self.OnClose()

    def OnClose(self, event = None):
        """Saves launcher's current configuration and quits the launcher.

        Called after a successful Launch or when the user manually closes
        the launcher window."""
        ##Update default config file.
        self.UpdateData()
        SaveConfig(DEFAULT_CONFIG, self.state, saveLastConfig = True)
        self.Hide()
        self.Destroy()
        ##If a shell's launched, start it here, after cleanup.
        if self.state.GetSurface("Shell") == True and self.launch == True:
            velShell.Start(self.state.GetSurface("ShellScript"))

##START MAIN PROGRAM
##Get & clean up command line arguments.
arguments = sys.argv[1:]

VALID_ARGUMENTS = {'c' : "conductor",
                   'n' : "nameserver",
                   'x' : "xplorer",
                   'l:' : "cluster=",
##                   'k' : "desktop",
                   'j:' : "jconf=",
                   't:' : "taomachine=",
                   'p:' : "port=",
                   'w:' : "dir=",
##                   'm:' : "master=",
                   'd' : "dev",
                   's' : "shell",
                   'q' : "quick",
                   'b' : "debug",
                   'g:' : "config=",
                   'v' : "version"}
shortArgs = ""
longArgs = []
for arg in VALID_ARGUMENTS:
    shortArgs = shortArgs + arg
    longArgs.append(VALID_ARGUMENTS[arg])
try:
    opts, args = getopt.getopt(arguments, shortArgs, longArgs)
except getopt.GetoptError:
    usage()
    sys.exit(2)

##Check if dev mode's on
if ("--dev", "") in opts or ("-d", "") in opts:
    ##Run VE-Suite in dev mode? Turned to True if --dev passed.
    devMode = True
    ##Change Desktop mode's jconf for dev mode.
    devDesktopName = "DevDesktop"
    devDesktop = JconfDict({devDesktopName: DEFAULT_DEV_JCONF})
    MODE_DICT["Desktop"]["JconfDict"] = devDesktop
    MODE_DICT["Desktop"]["JconfSelection"] = devDesktopName
    BASE_CONFIG["JconfDict"] = devDesktop
    BASE_CONFIG["JconfSelection"] = devDesktopName
    BASE_CONFIG["Directory"] = VELAUNCHER_DIR
else:
    devMode = False

##Prepare previous config
##Prepare data storage
previousState = CoveredConfig()
##Restore config values from last time.
LoadConfig(DEFAULT_CONFIG, previousState, loadLastConfig = True)

app = wx.PySimpleApp()
if not CommandLine(opts, args, previousState).AutoLaunched():
    frame = LauncherWindow(None, -1, 'VE Suite Launcher', args, previousState)
app.MainLoop()
##Command Line Check, then Window Boot (if necessary)
del config

####Window boot
##if not (len(args) > 0 and previousState.GetSurface("AutoRunVes")) and \
##   (len(opts) == 0 or (len(opts) == 1 and devMode)):
##    ##Launch the application
##    app = wx.PySimpleApp()
##    frame = LauncherWindow(None, -1, 'VE Suite Launcher', args, previousState)
##    app.MainLoop()
##    ##Delete the config link to avoid memory leakage.
##    del config
####Command line boot
####Takes arguments passed, uses defaults for the rest, launches immediately.
##else:
##    app = wx.PySimpleApp()
##    app.MainLoop()
##    CommandLine(opts, args, previousState)
