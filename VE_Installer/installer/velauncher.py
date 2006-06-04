"""Takes VE-Suite settings input and launches chosen VE-Suite programs.

This module creates a launcher window so the user can choose which
parts of VE-Suite to launch:
-Name Server
-Xplorer (user also chooses its mode and jconf file)
-Conductor
The user can also select VE-Suite's working directory.

When the user has decided the settings and hits the Launch button,
the module sets up the system's environmental variables and executes
the chosen programs. The launcher automatically quits after Launch.

The launcher is made for standard builds of VE-Suite. To launch a custom build
with it, create a batch/shell file to set the extra environmental variables,
execute the launcher on its last command.
"""
import os ##Used for setting environmental variables, running programs
import time ##Used only for sleep() func in the NameServer call

import wx ##Used for GUI

##Temporary override; sends standard Jconf files to launcher.
##Set JCONF_STANDARD to True if you want the standard configuration,
##set it to False if you want to test choosing your own Jconf file.
##CODE NOTE: Used in Launch.UnixLaunch and Launch.WindowsLaunch
JCONF_STANDARD = True
##File/Folder settings.
##Note: The HOME_BASE variable will be the one the installer needs to modify.
JUGGLER_FOLDER = "vrJuggler2.0.1"
DIRECTORY_DEFAULT = os.path.join(os.getcwd(), "exampleDatasets")
CONFIG_FILE = "VE-Suite-Launcher"
DEFAULT_CONFIG = "previous"
RADIO_XPLORER_LIST = ["OpenSceneGraph", "OSG Patented",
                      "OSG Patented Cluster", "Performer"]
JCONF_CONFIG = "JconfList"
DEFAULT_JCONF = "simstandalone.jconf"
##Values for launcher's GUI layout
INITIAL_WINDOW_SIZE = (500, -1)
INITIAL_JCONF_WINDOW_SIZE = (200, 200)
BACKGROUND_COLOR = wx.Colour(149, 149, 251)
JCONF_LIST_DISPLAY_MIN_SIZE = (100, 50)
TOP_SPACE = (75, 75)
BORDER = 5
VERTICAL_SPACE = (-1, BORDER)
HORIZONTAL_SPACE = (BORDER, -1)
LEFT_MARGIN = HORIZONTAL_SPACE
NULL_SPACE = (0, 0)
##Other values for launcher's UI
XPLORER_SHELL_NAME = "Xplorer Shell"
CONDUCTOR_SHELL_NAME = "Conductor Shell"

class LauncherWindow(wx.Frame):
    """Manages the launcher's window and the use of data from it.

    LauncherWindow manages the launcher's GUI, saving/loading the
    configuration settings, and the commands to launch the VE-Suite programs.

    Order of steps:
        __init__ & LoadConfig(previous)
        DependenciesCheck & DependenciesGet
        *User selects settings:*
            User manages Jconf choices: EditJconf, UpdateJconf
            User chooses directory: 
            User turns on Name_Server, Conductor, Xplorer: 
        *If Launch button pressed:*
            Launch
            OnClose & SaveConfig(previous)
            quit
        *Else if window's closed:*
            OnClose & SaveConfig(previous)
            quit

    Functions:
        __init__(parent, ID, title)
        DependenciesCheck
        DependenciesGet
        EvtCheckXplorer(event)
        UpdateChJconf(cursor)
        EditJconf
        GetSelectedJconf
        ChooseDirectory(event)
        SaveConfig(config, name)
        LoadConfig(config, name)
        Launch(event)
        OnClose(event)        
    """
    def __init__(self, parent, ID, title):
        """Builds the launcher's window and loads the last configuration."""
        wx.Frame.__init__(self, parent, -1, title,
                          style=wx.DEFAULT_FRAME_STYLE)

        ##Get the logo.
        ##imLogo = wx.Image(os.path.join("images", "ve_logo.gif"),
        ##                  wx.BITMAP_TYPE_GIF)

        ##Prepare data storage
        ##NOTE: jconfList is a local copy of the Jconf list stored in the
        ##program's config. Changes to jconfList are mirrored in the config.
        self.jconfList = []
        ##Build buttons.
        ##NOTE: Save/load configs disabled for now.
        ##self.bLoad = wx.Button(self, ID_LOAD, "Load Settings")
        ##self.bSave = wx.Button(self, ID_SAVE, "Save Settings")
        bDirectory = wx.Button(self, -1, "Choose Working Directory")
        bDirectory.SetToolTip(wx.ToolTip("Choose the working directory for" +
                                         " the programs."))
        bEditJconf = wx.Button(self, -1, "Edit Juggler Configurations")
        bEditJconf.SetToolTip(wx.ToolTip("Edit the list of Juggler configuration" +
                                         " files displayed in the Launcher."))
        bLaunch = wx.Button(self, -1, "Launch VE Suite")
        bLaunch.SetToolTip(wx.ToolTip("Run the programs you selected and" +
                                      " close the Launcher."))
        ##Build text controls.
        self.txDirectory = wx.TextCtrl(self, -1,
                                       DIRECTORY_DEFAULT)
                                       ##style=wx.TE_READONLY)
        self.txDirectory.SetToolTip(wx.ToolTip("The path of the" +
                                               " working directory."))
        self.chJconf = wx.Choice(self, -1)
        self.chJconf.SetToolTip(wx.ToolTip("Choose the Juggler configuration" +
                                           " Xplorer will use for its" +
                                           " configuration settings."))
        self.txTaoPort = wx.TextCtrl(self, -1)
        self.txTaoPort.SetToolTip(wx.ToolTip("Enter VE Suite's port."))
        self.txTaoMachine = wx.TextCtrl(self, -1)
        self.txTaoMachine.SetToolTip(wx.ToolTip("Enter VE Suite's machine."))
        ##Build checkboxes.
        self.cbNameServer = wx.CheckBox(self, -1, "Name Server")
        self.cbNameServer.SetToolTip(wx.ToolTip("Run the Name Server at Launch"))
        self.cbXplorer = wx.CheckBox(self, -1, "Xplorer")
        self.cbXplorer.SetToolTip(wx.ToolTip("Run the Xplorer at Launch"))
        self.cbConductor = wx.CheckBox(self, -1, "Conductor")
        self.cbConductor.SetToolTip(wx.ToolTip("Run the Conductor at Launch"))
        ##Build radio buttons.
        self.rbXplorer = wx.RadioBox(self, -1, "Xplorer Type",
                                     wx.DefaultPosition, wx.DefaultSize,
                                     RADIO_XPLORER_LIST, 2, wx.RA_SPECIFY_ROWS)
        self.rbXplorer.SetToolTip(wx.ToolTip("Which Xplorer format do you" +
                                             " want to launch?"))
        ##Set tool tip popup delay to 1 second
        wx.ToolTip.SetDelay(1000)
        ##Check the dependencies.
        self.DependenciesCheck()
        ##Event bindings.
        ##NOTE: Save/load configs disabled for now.
        ##self.Bind(wx.EVT_BUTTON, self.OnSave, self.bSave)
        ##self.Bind(wx.EVT_BUTTON, self.OnLoad, self.bLoad)
        self.Bind(wx.EVT_CHECKBOX, self.EvtCheckXplorer, self.cbXplorer)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        self.Bind(wx.EVT_BUTTON, self.ChooseDirectory, bDirectory)
        self.Bind(wx.EVT_BUTTON, self.Launch, bLaunch)
        self.Bind(wx.EVT_BUTTON, self.EditJconf, bEditJconf)
        ##Restore config values from last time.
        self.LoadConfig(DEFAULT_CONFIG)
        
        ##Layout format settings
        ##Save/Load column Sizer
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        ##NOTE: Save/load config buttons disabled for now
##        columnSizer.AddMany([self.bLoad,
##                             HORIZONTAL_SPACE,
##                             self.bSave])
        ##Create the overall layout box
        rowSizer = wx.BoxSizer(wx.VERTICAL)
##        ##Add the Load/Save Config bar
##        rowSizer.AddMany([columnSizer,
##                          VERTICAL_SPACE])
        ##Construct the Directory column
        columnSizer.Add(self.txDirectory, 1, wx.ALIGN_BOTTOM)
        columnSizer.AddMany([HORIZONTAL_SPACE,
                             bDirectory])
        ##Construct the check box/radio box grid.
        gridSizer = wx.FlexGridSizer(3, 2,
                                     VERTICAL_SPACE[1], HORIZONTAL_SPACE[0])
        gridSizer.AddMany([self.cbNameServer, NULL_SPACE,
                           self.cbConductor, NULL_SPACE,       
                           self.cbXplorer, self.rbXplorer])
        ##Insert the Directory column.
        rowSizer.Add(wx.StaticText(self, -1, "Working Directory:"))
        rowSizer.Add(columnSizer, 0, wx.EXPAND) 
        ##Construct the Jconf column.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(self.chJconf, 1, wx.ALIGN_BOTTOM)
        columnSizer.AddMany([HORIZONTAL_SPACE,
                             bEditJconf])
        columnSizer.Add((-1, -1), 1)
        ##Insert the Jconf column.
        rowSizer.Add(VERTICAL_SPACE)
        rowSizer.Add(wx.StaticText(self, -1, "Jconfiguration file:"))
        rowSizer.Add(columnSizer, 0, wx.EXPAND)
        ##Construct the Tao column.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(wx.StaticText(self, -1, "Tao Machine:"),
                        0, wx.ALIGN_CENTER_VERTICAL)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.txTaoMachine, 2)
        columnSizer.Add((HORIZONTAL_SPACE[0]*5, -1))
        columnSizer.Add(wx.StaticText(self, -1, "Tao Port:"),
                        0, wx.ALIGN_CENTER_VERTICAL)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.txTaoPort, 1)
        columnSizer.Add(HORIZONTAL_SPACE, 1)
        ##Insert the Tao column, box grid and Launch button.
        rowSizer.Add(VERTICAL_SPACE)
        rowSizer.Add(columnSizer, 0, wx.EXPAND)
        rowSizer.AddMany([VERTICAL_SPACE,
                          wx.StaticText(self, -1, "Programs to launch:"),
                          VERTICAL_SPACE,
                          gridSizer])
        rowSizer.Add(VERTICAL_SPACE, 1)
        rowSizer.Add(bLaunch, 0, wx.ALIGN_RIGHT)
        ##Add the left margin
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.AddSpacer(LEFT_MARGIN)
        columnSizer.Add(rowSizer, 1, wx.EXPAND)
        ##Add the title graphic space
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        ##rowSizer.Add(imLogo)
        rowSizer.Add(TOP_SPACE) ##Replace with line above later.
        rowSizer.Add(columnSizer, 1, wx.EXPAND)
        ##Set the main sizer.
        mainSizer = wx.BoxSizer(wx.HORIZONTAL)
        mainSizer.Add(rowSizer, 1, wx.BOTTOM | wx.RIGHT | wx.EXPAND, BORDER)
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.SetSize(INITIAL_WINDOW_SIZE)
        ##Set the background color.
        self.SetBackgroundColour(BACKGROUND_COLOR)
        ##Show the window.
        self.Show(True)
        ##Error check: Is there a /bin folder in the launcher's directory?
        ##If so, assume it's in VE Suite's folder. If not, warn the user.
        if not os.path.exists("bin"):
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

    def DependenciesCheck(self):
        """Ask user for DependenciesDir if one doesn't exist in default config.

        Automatically called during __init__.
        Checks if default/DependenciesDir exists in default config,
        then checks if that directory exists,
        then checks if that directory looks like the Dependencies directory.
        If any check fails, it asks the user for a Dependencies directory."""
        ##Load DependenciesDir from default config file
        config = wx.Config(CONFIG_FILE)
        config.SetPath(DEFAULT_CONFIG)
        dependenciesDir = config.Read("DependenciesDir", ":::")
        legitimateDependenciesDir = False
##        print "Dependencies check." ##TESTER
        ##Set name of the file to check in the Dependencies folder
        if os.name == "posix":
            nameServiceFile = "Naming_Service"
        elif os.name == "nt":
            nameServiceFile = "Naming_Service.exe"
        else:
            nameServiceFile = "None"
        while not legitimateDependenciesDir:
            ##Check if DependenciesDir exists in default config.
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
                dependenciesDir = self.DependenciesGet()
            ##Check if DependenciesDir's path exists.
            elif not os.path.exists(dependenciesDir):
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
                dependenciesDir = self.DependenciesGet()
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
                if dlg.ShowModal() == wx.ID_NO:
                    dlg.Destroy()
                    dependenciesDir = self.DependenciesGet()
                else:
                    legitimateDependenciesDir = True
                    dlg.Destroy()
            ##If all checks passed, exit the loop
            else:
                legitimateDependenciesDir = True
        ##Write the new Dependencies directory to default config.
        config.Write("DependenciesDir", dependenciesDir)


    def DependenciesGet(self):
        """Ask user for DependenciesDir. Called by DependenciesCheck.

        Returns the directory path the user chose.
        Helper function for DependenciesCheck."""
        dirChosen = False
        while not dirChosen:
            ##Go up a directory if it's a Unix os to get out
            ##of the VE_Suite directory.
            if os.name == "nt":
                searchDir = os.getcwd()
            elif os.name == "posix":
                searchDir = os.path.split(os.getcwd())[0]
            else:
                searchDir = "dead parrot sketch"
##            print "Search dir: " + searchDir ##TESTER
            dlg = wx.DirDialog(None,
                               "Choose the VE_Suite_Dependencies directory:",
                               searchDir,
                               style=wx.DD_DEFAULT_STYLE)
            if dlg.ShowModal() == wx.ID_OK:
                ##If a directory's chosen, exit the loop and return it.
                searchDir = dlg.GetPath()
                dirChosen = True
                dlg.Destroy()
            else:
                ##If not, show an error message and ask the user to choose
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
                    OnClose("dead parrot sketch")    
        return searchDir

    def EvtCheckXplorer(self, event):
        """Enables/Disables Xplorer's radio box.

        Prevents the user from choosing Xplorer's mode if Xplorer
        won't be launched.
        The radio box is enabled if Xplorer's check box is checked,
        disabled if it isn't."""
        self.rbXplorer.Enable(event.GetEventObject().IsChecked())
        self.rbXplorer.Enable(event.GetEventObject().IsChecked())

    def UpdateChJconf(self, cursor):
        """Updates the Jconf choice window in the Launcher.

        Keyword arguments:
        cursor -- ID of selected choice (changed to 0 if out of range)"""
        ##Rebuild the choice list
        self.chJconf.Clear()
        nameArray = self.jconfList.GetNames()
        for i in range(len(nameArray)):
            self.chJconf.Append(nameArray[i])
        ##Set the cursor
        if cursor == wx.NOT_FOUND or cursor < 0 \
           or cursor >= self.chJconf.GetCount():
            cursor = 0
        ##Error catcher for lists without any items.
        if self.chJconf.GetCount() == 0:
            cursor = wx.NOT_FOUND
        self.chJconf.SetSelection(cursor)
        ##NOTE: Put in "Add new Jconf" option as last item.

    def EditJconf(self, event):
        """Brings up the Jconf editing window."""
        jconfWindow = JconfWindow(self, wx.ID_ANY, "Edit Jconf List",
                                  self.jconfList, self.chJconf.GetSelection())
        jconfWindow.ShowModal()
        jconfWindow.Destroy()        

    def GetSelectedJconf(self):
        """Returns the path of the Jconf file selected in the launcher window."""
        xplorerConfig = self.jconfList.GetPath(self.chJconf.GetSelection())
        print "Jconf file: " + xplorerConfig ##TESTER
        return xplorerConfig

    ##The user chooses the directory path
    def ChooseDirectory(self, event):
        """The user chooses the working directory through a directory dialog."""
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

    def SaveConfig(self, name):
        """Saves the current configuration under name.

        Keyword arguments:
        name -- What to name this configuration"""
        ##Save the current configuration under name
        config = wx.Config(CONFIG_FILE)
        config.SetPath("/" + name)
        config.Write("Directory", self.txDirectory.GetValue())
        config.WriteInt("JconfCursor", self.chJconf.GetSelection())
        config.Write("NameServer", str(self.cbNameServer.GetValue()))
        config.Write("Xplorer", str(self.cbXplorer.GetValue()))
        config.WriteInt("XplorerType", self.rbXplorer.GetSelection())
        config.Write("Conductor", str(self.cbConductor.GetValue()))
        config.Write("TaoMachine", self.txTaoMachine.GetValue())
        config.Write("TaoPort", self.txTaoPort.GetValue())
##        print "Saved configuration." ##TESTER
        return
    
    def LoadConfig(self, name):
        """Loads the configuration under name.

        Keyword arguments:
        name -- Name of configuration to load
        """
        ##Load the configuration file under name
        config = wx.Config(CONFIG_FILE)
        config.SetPath("/" + name)
        ##Set directory, set insertion pt. to end of it for better initial view.
        self.txDirectory.SetValue(config.Read("Directory", DIRECTORY_DEFAULT))
        ##Sets txDirectory's cursor to end in Linux systems for easier reading.
        ##Acts strange in Windows for some reason; investigate.
        if os.name == "posix":
            self.txDirectory.SetInsertionPointEnd()
        ##Set choices for Jconf list.
        if config.HasGroup(JCONF_CONFIG):
            self.jconfList = JconfList(name)
        ##Set default choices if JCONF_CONFIG doesn't exist,
        ##but DependenciesDir does.
        elif config.Read("DependenciesDir", ":::") != ":::":
            p = os.path.join(config.Read("DependenciesDir"),
                              JUGGLER_FOLDER, "configFiles",
                              DEFAULT_JCONF)
            config.SetPath(JCONF_CONFIG)
            config.Write(os.path.split(p)[1][:-6], p)
            self.jconfList = JconfList(name)
        ##If neither exists, bring up an error. NOTE: Should never be reached.
        else:
            print "ERROR: No Jconf configuration found and failed to make" + \
                  "default Jconf from Dependencies dir."
        ##Set Jconf cursor & Jconf choices list.
        self.UpdateChJconf(config.ReadInt("JconfCursor", 0))
        ##Set Tao Machine & Port
        self.txTaoMachine.SetValue(config.Read("TaoMachine", "localhost"))
        ##Temporary workaround for error w/ Int TaoPort in last version
        if config.GetEntryType("TaoPort") == 3: ##3 == Type_Integer
            self.txTaoPort.SetValue(str(config.ReadInt("TaoPort", 1239)))
        ##Normal functioning for Str TaoPort
        else:
            self.txTaoPort.SetValue(config.Read("TaoPort", "1239"))
        ##Set Name Server
        if config.Read("NameServer", "True") == "True":
            self.cbNameServer.SetValue(True)
        else:
            self.cbNameServer.SetValue(False)            
        ##Set Xplorer
        if config.Read("Xplorer", "True") == "True":
            self.cbXplorer.SetValue(True)
        else:
            self.cbXplorer.SetValue(False)
        ##Show/Fade the radiobox
        self.rbXplorer.Enable(self.cbXplorer.IsChecked())
        ##Set Xplorer Type
        data = config.ReadInt("XplorerType", -1)
        if data >= 0 and data < len(RADIO_XPLORER_LIST):
            self.rbXplorer.SetSelection(data)
        else:
            self.rbXplorer.SetSelection(0)
        ##Set Conductor
        if config.Read("Conductor", "True") == "True":
            self.cbConductor.SetValue(True)
        else:
            self.cbConductor.SetValue(False)
##        print "Configuration loaded." ##TESTER

    ##MODIFY FOR CONFIG CHANGE
    ##NOTE: Disabled until Save/Load Configuration is implemented
    ##for the Launcher.
##    def OnSave(self, event):
##        dlg = wx.FileDialog(self, message="Save settings as...",
##                            defaultDir=os.getcwd(),
##                            defaultFile="", wildcard=SUFFIX_LIST,
##                            style=wx.SAVE)
##        if dlg.ShowModal() == wx.ID_OK:
##            self.SaveConfig(dlg.GetPath())
##
##    def OnLoad(self, event):
##        dlg = wx.FileDialog(self, message="Choose a file",
##                            defaultDir=os.getcwd(),
##                            defaultFile="", wildcard=SUFFIX_LIST,
##                            style=wx.OPEN | wx.CHANGE_DIR)
##        if dlg.ShowModal() == wx.ID_OK:
##            self.LoadConfig(dlg.GetPath())

    def Launch(self, event):
        """Checks input, begins launch if error-free."""
        ##ERROR CHECK:  Are any programs selected?
        ##              If not, abort launch.
        if not (self.cbNameServer.IsChecked() or self.cbConductor.IsChecked()
                or self.cbXplorer.IsChecked()):
            dlg = wx.MessageDialog(self,
                                   "The launch won't do anything because you " +
                                   "haven't selected any programs.\n" +
                                   "Please select some programs and try " +
                                   "launching again.",
                                   "Launch Error: No Program Selected", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        ##ERROR CHECK:  Is the Tao Port between 0 and 65535?
        ##              If not, abort launch.
        if not (self.txTaoPort.GetValue().isdigit() and
                int(self.txTaoPort.GetValue()) <= 65535):
            dlg = wx.MessageDialog(self,
                                   "You have entered an illegal Tao Port.\n" +
                                   "Please enter a Tao Port between 0 and" +
                                   " 65535.",
                                   "Launch Error: Illegal Tao Port", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        ##ERROR CHECK:  Does the working directory chosen exist?
        ##              If not, let the user change it.
        if not (os.path.exists(self.txDirectory.GetValue())):
            dlg = wx.MessageDialog(self,
                                   "The working directory you chose " +
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
                    return
            ##If user chooses NO, abort the launch.
            else:
                dlg.Destroy()
                return
        ##ERROR CHECK:  Does the selected Jconf file exist?
        ##              If not, abort the launch.
        if not (os.path.exists(self.GetSelectedJconf())):
            dlg = wx.MessageDialog(self,
                                   "The Juggler configuration file you chose " +
                                   "doesn't exist.\n" +
                                   "Please select a different one.",
                                   "Launch Error: Jconf File Doesn't Exist",
                                   wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        ##Go into the Launch
        Launch(self,
               self.txDirectory.GetValue(),
               self.cbNameServer.IsChecked(), self.cbConductor.IsChecked(),
               self.cbXplorer.IsChecked(), self.rbXplorer.GetSelection(),
               self.GetSelectedJconf(),
               self.txTaoMachine.GetValue(), int(self.txTaoPort.GetValue()))

    ##Saves the current configuration under the prefs file before closing.
    def OnClose(self, event):
        """Saves launcher's current configuration and quits the launcher.

        Called after a successful Launch or when the user manually closes
        the launcher window."""
        ##CODE NOTE: Known error running on Unix where the window isn't
        ##closed, and closing it causes VE-Suite to quit. Cause is a
        ##direct call to the Xplorer program. (Find error tag: $$ERROR_1$$)
        ##ANTICIPATED FIX: Wait until Xplorer doesn't require text input
        ##for the .param file anymore, call it as a separate thread.
        ##(Add & to the end of its command.)
        ##Update default config file.
        self.SaveConfig(DEFAULT_CONFIG)
        self.Hide()
        self.Destroy()


class JconfList:
    """Stores a list of Jconf pairs in this setup:
    [[name, path], [name, path]..]
    under the variable self.list.

    Functions:
        __init__(configName)
        SetConfig(configName)
        Add(name, path)
        Rename(pos, newName)
        Delete(pos)
        GetPath(index)
        Length / __len__
        GetNames
    """
    def __init__(self, configName):
        """Creates a list of .jconf names/paths from the Launcher's Config."""
        self.list = []
        self.SetConfig(configName)
        bCont = self.config.GetFirstEntry()
##        print self.config.GetPath() ##Tester
        while (bCont[0]):
            self.list.append([bCont[1], self.config.Read(bCont[1])])
            bCont = self.config.GetNextEntry(bCont[2])

    def SetConfig(self, configName):
        """Sets self.config to the folder containing configName's Jconf info."""
        self.config = wx.Config(CONFIG_FILE)
        self.config.SetPath(configName)
        self.config.SetPath(JCONF_CONFIG)

    def Add(self, name, path):
        """Adds [name, path] to the list."""
        self.list.append([name, path])
        self.config.Write(name, path)
        ##NOTE: Adding the same Jconf file twice results in two entries w/
        ##same name; correct that later.

    def Rename(self, pos, newName):
        """Renames the entry at pos to newName, appending a number if necessary.

        These suffixes are added to newName until a unique name's generated:
        [none], 1, 2, 3, ... 9, 10, 11, etc."""
        curNames = self.GetNames()
        ##Add a numeric suffix to newName if it matches a name
        ##already in the list.
        suffix = ""
        x = curNames.count(newName + str(suffix))
        while x > 0 and \
              (newName + str(suffix)) != curNames[pos]:
            if suffix == "":
                suffix = '1'
            else:
                suffix = str(int(suffix) + 1)
            x = curNames.count(newName + str(suffix))
        self.config.RenameEntry(self.list[pos][0], newName + suffix)
        self.list[pos][0] = newName + suffix
        ##Warns user if name had suffix added to it.
        if self.list[pos][0] != newName:
            dlg = wx.MessageDialog(None,
                                   "The name " + newName +" already existed" + \
                                   " in the list.\n" + \
                                   "Your entry was given the" + \
                                   " name " + newName + suffix + " instead.",
                                   "NOTE: Name Changed",
                                   wx.OK)
            dlg.ShowModal()

    def Delete(self, pos):
        """Deletes pos's entry."""
        self.config.DeleteEntry(self.list[pos][0])
        self.list.pop(pos)

    def GetPath(self, index):
        """Returns the path of index's entry."""
        return self.list[index][1]

    def Length(self):
        """Returns the length of self.list."""
        return len(self.list)

    def __len__(self):
        return self.Length()

    def GetNames(self):
        """Returns a list of the entries' names."""
        nList = []
        for i in range(len(self.list)):
            nList.append(str(self.list[i][0]))
        return nList


class JconfWindow(wx.Dialog):
    """A window for editing a list of Jconf files.

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
    def __init__(self, parent, ID, title, L, cursor = 0):
        """Sets up the Jconf window.

        Keyword arguments:
        L: The linked Jconf list this window modifies.
        cursor: Index of the current selection in L."""
        wx.Dialog.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE)
        ##Data storage.
        self.list = L
        ##Build displays.
        self.confList = wx.ListBox(self, -1, size=JCONF_LIST_DISPLAY_MIN_SIZE,
                                   choices=self.list.GetNames())
        self.confList.SetSelection(cursor)
        self.display = wx.TextCtrl(self, -1, style=wx.TE_READONLY)
        self.DisplayJconfFile("dead parrot sketch")
        ##Build buttons.
        bAdd = wx.Button(self, -1, "Add")
        bRename = wx.Button(self, -1, "Rename")
        self.bDelete = wx.Button(self, -1, "Delete")
        ##Check if Delete's enabled.
        self.DeleteEnabledCheck()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.AddNew, bAdd)
        self.Bind(wx.EVT_BUTTON, self.Delete, self.bDelete)
        self.Bind(wx.EVT_BUTTON, self.Rename, bRename)
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
        rowSizer.Add(columnSizer, 1, wx.EXPAND)
        rowSizer.AddMany([VERTICAL_SPACE,
                          wx.StaticText(self, -1, "Selection's Jconf file:")])
        rowSizer.Add(self.display, 0, wx.EXPAND)
        mainSizer = wx.BoxSizer(wx.HORIZONTAL)
        mainSizer.Add(rowSizer, 1, wx.ALL | wx.EXPAND, BORDER)
        ##Set size, position.
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.SetSize(INITIAL_JCONF_WINDOW_SIZE)
        self.CenterOnParent(wx.BOTH)

    def DisplayJconfFile(self, event):
        """Shows the .jconf file of the selection in the text field."""
        c = self.confList.GetSelection()
        if c in range(len(self.list)):
            p = self.list.GetPath(c)
            f = os.path.split(p)[1]
        else:
            f = "ERROR: Entry missing from list."
        self.display.SetValue(f)
        
    def DeleteEnabledCheck(self):
        """Disables/Enables the Delete button based on number of entries.

        Disabled if entries <= 1
        Enabled if entries > 1"""
        if self.list.Length() <= 1:
            self.bDelete.Enable(False)
        else:
            self.bDelete.Enable(True)

    def Update(self, cursor):
        """Updates the shown entries list to match recent changes."""
        self.confList.Set(self.list.GetNames())
        self.confList.SetSelection(cursor)
        self.DisplayJconfFile("dead parrot sketch")

    def AddNew(self, event):
        """User chooses a new Jconf file to add to the list.

        Default name: Name of Jconf file."""
        ##Default directory for the search is the
        ##directory of the currently selected Jconf.
        c = self.confList.GetSelection()
        if c in range(len(self.list)):
            p = self.list.GetPath(c)
            f = os.path.split(p)[0]
        else:
            f = os.getcwd()
        dlg = wx.FileDialog(self,
                           "Choose a configuration file.",
                           defaultDir = f,
                           wildcard = "Jconfig (*.jconf)|*.jconf",
                           style=wx.OPEN)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            self.list.Add(os.path.split(path)[1][:-6], path)
            self.Update(self.confList.GetSelection())
            self.DeleteEnabledCheck()

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
            self.list.Delete(cursor)
            ##Move the cursor if it wouldn't be on the list anymore.
            if cursor >= self.list.Length():
                cursor = self.list.Length() - 1
            self.Update(cursor)
            self.DeleteEnabledCheck()

    def Rename(self, event):
        """Renames the selected Jconf entry.
        
        Ensures the new name:
        -Contains no slashes.
        -Isn't empty spaces."""
        loop = True
        name = self.confList.GetStringSelection()
        while loop:
            f= os.path.split(self.list.GetPath(self.confList.GetSelection()))[1]
            dlg = wx.TextEntryDialog(self,
                                     "What do you want to rename " + \
                                     self.confList.GetStringSelection() + \
                                     " to?\n\n" + \
                                     "Jconf File: " + f,
                                     "Rename",
                                     name)
            if dlg.ShowModal() == wx.ID_OK:
                name = dlg.GetValue()
                dlg.Destroy()
                cursor = self.confList.GetSelection()
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
                                           "Please choose a different name.",
                                           "ERROR: Name is Empty",
                                           wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()
                    name = self.confList.GetStringSelection()
                ##Else accept it.
                else:
                    self.list.Rename(cursor, name)
                    self.Update(cursor)
                    loop = False
            else:
                loop = False

    def OnClose(self, event):
        """Closes JconfWindow."""
        self.GetParent().UpdateChJconf(self.confList.GetSelection())
        self.Hide()
        self.Destroy()


class Launch:
    """Prepares the environment and launches the chosen programs.

    Order of steps:
        Change directory to chosen working directory.
        Set the environmental variables.
        WindowsLaunch or UnixLaunch [based on os type]
        OnClose [quits the launcher]

    Functions:
        __init__(launcherWindow, workingDir, runName, runConductor, runXplorer,
                 typeXplorer, jconf)
        Windows(runName, runConductor, runXplorer, typeXplorer, jconf)
        Unix(runName, runConductor, runXplorer, typeXplorer, jconf)
        EnvSetup(dependenciesDir, workingDir)
        EnvFill(var, default)
        """
    def __init__(self, launcherWindow,
                 workingDir,
                 runName, runConductor, runXplorer, typeXplorer,
                 jconf,
                 taoMachine, taoPort):
        """Sets environmental vars and calls OS-specific launch code.

        Keyword arguments:
        launcherWindow -- The caller. Used to close it after the call.
        workingDir, taoMachine, taoPort -- Used for environmental vars.
        runName, runConductor, runXplorer,
        typeXplorer, jconf -- Used for launch code."""
        ##The launch is the final step.
        ##Destroy launcher window before beginning the actual launch. 
        launcherWindow.Close()
        ##Get dependenciesDir for setting environmental variables
        config = wx.Config(CONFIG_FILE)
        config.SetPath("/" + DEFAULT_CONFIG)
        dependenciesDir = config.Read("DependenciesDir", "ERROR")
        ##Set the environmental variables
        self.EnvSetup(dependenciesDir, workingDir, taoMachine, taoPort)
        ##Use the user's defined directory as Current Working Dir
##        print "Changing to directory: " + self.txDirectory.GetValue() ##TESTER
        ##os.chdir(os.getenv("VE_WORKING_DIR"))
##        print os.name ##TESTER
##        print os.getcwd() ##TESTER
        ##Checks the OS and routes the launcher to the proper subfunction
        ##NOTE: Code out separate Setups, code in the combined Setup
        if os.name == "nt":
            self.Windows(runName, runConductor, runXplorer,
                               typeXplorer, jconf)
        elif os.name == "posix":
            self.Unix(runName, runConductor, runXplorer,
                            typeXplorer, jconf)
        else:
            print "ERROR: VE-Suite-Launcher doesn't support this OS."
        return

    def Windows(self, runName, runConductor, runXplorer, typeXplorer, jconf):
        """Launches the chosen programs under an Unix OS.

        Keyword arguments:
        runName, runConductor, runXplorer -- Run NameServer/Conductor/Xplorer?
        typeXplorer -- Which Xplorer program to run.
        jconf -- Which .jconf file to use for Xplorer's settings."""
        ##Name Server section
        ##NOTE: Name Server starts up in Launcher's window.
        ##Closing the Launcher's DOS window closes Name Server as well.
        ##Closing the Launcher doesn't close the Launcher's DOS window while
        ##Name Server's running, though.
        ##Do we need to give Name Server its own window?
        if runName:
            os.system("start /B Naming_Service.exe -ORBEndPoint" +
                      " iiop://%TAO_MACHINE%:%TAO_PORT%")
            time.sleep(5)
            os.system("start /B WinServerd.exe -ORBInitRef" +
                      " NameService=" +
                      "corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService" +
                      " -ORBDottedDecimalAddresses 1")
        ##Conductor section
        if runConductor:
            os.system('start "' + CONDUCTOR_SHELL_NAME + '" ' +
                      "WinClientd.exe -ORBInitRef" +
                      " NameService=" +
                      "corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService" +
                      " -ORBDottedDecimalAddresses 1")
        ##Xplorer section
        if runXplorer:
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
            ##JCONF OVERRIDE: Temp feature; remove later.
            if JCONF_STANDARD:
                jconf = str(os.getenv("VJ_BASE_DIR")) + \
                        "\\configFiles\\simstandalone.jconf"
            ##Xplorer's start call
            os.system('start "' + XPLORER_SHELL_NAME + '" ' +
                      executable +
                      " " + jconf +
                      " -ORBInitRef" +
                      " NameService=" +
                      "corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService" +
                      " -ORBDottedDecimalAddresses 1")
##        print "Done." ##TESTER
        return

    def Unix(self, runName, runConductor, runXplorer, typeXplorer, jconf):
        """Launches the chosen programs under an Unix OS.

        Keyword arguments:
        runName, runConductor, runXplorer -- Run NameServer/Conductor/Xplorer?
        typeXplorer -- Which Xplorer program to run.
        jconf -- Which .jconf file to use for Xplorer's settings."""
        ##Name Server section
        if runName:
            os.system("VES -nserv &")
        ##Conductor section
        if runConductor:
            os.system("VES -menu &")
        ##Xplorer section
        if runXplorer:
            if typeXplorer == 0: ##OSG selection
                ##os.system("VES -simosg")
                executable = "project_tao_osg"
            elif typeXplorer == 1: ##OSG VEP selection
                ##os.system("VES -simosgvep")
                executable = "project_tao_osg_vep"
            elif typeXplorer == 2: ##OSG VEPC selection
                ##os.system("VES -clusterOSGVEP")
                executable = "project_tao_osg_vep_cluster"
            elif typeXplorer == 3: ##PF selection
                ##os.system("VES -sim")
                executable = "project_tao_pf"
            ##JCONF OVERRIDE: Temp feature; remove later.
            if JCONF_STANDARD:
                jconf = str(os.getenv("VJ_BASE_DIR")) + \
                        "/configFiles/sim.base.jconf " + \
                        str(os.getenv("VJ_BASE_DIR")) + \
                        "/configFiles/sim.wand.mixin.jconf"
            ##Xplorer's call
            ##Error tag: $$ERROR_1$$
            os.system(executable +
                      " -ORBInitRef" +
                      " NameService=" +
                      "corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService " +
                      jconf +
                      " ; /usr/share/Performer/bin/rmsem")
        print "Done." ##TESTER
        return


    def EnvSetup(self, dependenciesDir, workingDir, taoMachine, taoPort):
        """Sets up the environmental variables to launch VE-Suite's programs.

        Only takes care of basic variables. Coders with custom builds can set
        advanced variables by creating a batch/shell file to set the extra
        variables, then execute the launcher as its last command.
        The environmental settings will carry over.

        Variables overwritten by this class:
        VJ_BASE_DIR
        VJ_DEPS_DIR
        CFDHOSTTYPE (removes parantheses from CFDHOSTTYPE)

        Variables not overwritten, but set to a default value if empty:
        VE_INSTALL_DIR
        VE_SUITE_HOME
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

        Variables appended:
        PYTHON_PATH (Windows systems only)
        PATH
        LD_LIBRARY_PATH or LD_LIBRARYN32_PATH (Unix systems only)"""

        ##Determine the OS
        windows = (os.name == "nt")
        unix = (os.name == "posix")
##        print "Setup begins." ##TESTER
        ##Set where VE-Suite's installed
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
        os.environ["VJ_BASE_DIR"] = os.path.join(os.getenv("VE_DEPS_DIR"),
                                                 JUGGLER_FOLDER)
        os.environ["VJ_DEPS_DIR"] = os.path.join(os.getenv("VE_DEPS_DIR"),
                                                 JUGGLER_FOLDER)
##        print "VJ_BASE_DIR: " + str(os.getenv("VJ_BASE_DIR")) ##TESTER
##        print "VJ_DEPS_DIR: " + str(os.getenv("VJ_DEPS_DIR")) ##TESTER

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
##            print str(os.path.exists("/etc/redhat-release")) ##TESTER
            if (os.path.exists("/etc/redhat-release")):
                piped = os.popen("""cat /etc/redhat-release """ +
                                 """| awk -F" " '{print $1}'""", 'r')
                firstWord = piped.read()[:-1]
                ##NOTE: [:-1] is to remove the line break from the read()
                piped.close()
##                print "First word: "+firstWord ##TESTER
                if firstWord == "Red":
                    piped = os.popen("""cat /etc/redhat-release """ +
                                     """| awk -F" " '{print $3}'""", 'r')
                    thirdWord = piped.read()[:-1]
                    piped.close()
##                    print "Third word: "+thirdWord ##TESTER
                    if thirdWord == "Enterprise":
                        ##Extract words from file to create something like RHEL_3
                        piped = os.popen("""cat /etc/redhat-release """ +
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
                    piped = os.popen("""cat /etc/redhat-release """ +
                                     """| awk -F" " '{print $1 "_" $4}'""", 'r')
                    self.EnvFill("CFDHOSTTYPE", piped.read()[:-1])
                    piped.close()
                else:
                    ##NOTE: If the program couldn't identify this type of
                    ##Redhat, print an error & just use uname.
##                    print "ERROR: UnixSetup wasn't able to catch a redhat" + \
##                          "version to create the CFDHOSTTYPE var." ##TESTER
##                    print "Using generic uname as CFDHOSTTYPE instead." ##TESTER
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
##            print "CFDHOSTTYPE: " + str(os.getenv("CFDHOSTTYPE")) ##TESTER

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

        ##Python build environment variables
        if windows:
            os.environ["PYTHONPATH"] = os.path.join(os.getenv("VJ_DEPS_DIR"),
                                                    "lib", "python")
        elif unix:
            ##os.environ["PYTHONPATH"] = os.path.join(os.getenv("VJ_DEPS_DIR"),
            ##                                        "lib", "python")
            if os.getenv("OSG_HOME", "None") != "None":
                os.environ["PATH"] = os.path.join(str(os.getenv("OSG_HOME")),
                                                  "share", "OpenSceneGraph",
                                                  "bin") + ":" + \
                                     str(os.getenv("PATH"))

        ##Update PATH (and the Library Path for Unix)
        if windows:
            os.environ["PATH"] = str(os.getenv("PATH")) + ";" + \
                                 os.path.join(str(os.getenv("VJ_DEPS_DIR")),
                                              "bin") + ";" + \
                                 os.path.join(str(os.getenv("VJ_DEPS_DIR")),
                                              "lib") + ";" + \
                                 os.path.join(str(os.getenv("VJ_BASE_DIR")),
                                              "lib") + ";" + \
                                 os.path.join(str(os.getenv("VE_INSTALL_DIR")),
                                              "bin") + ";" + \
                                 os.path.join(str(os.getenv("VE_DEPS_DIR")),
                                              "bin") + ";" + \
                                 os.path.join(str(os.getenv("CD")),
                                              "bin")
        ##    os.environ["PATH"] = str(os.getenv("PATH")) + ";" + \
        ##                         str(os.getenv("VTK_HOME")) + r"\bin;" + \
        ##                         str(os.getenv("WX_HOME")) + r"\lib\vc_dll"
        ##    os.environ["PATH"] = str(os.getenv("PATH")) + ";" + \
        ##                         str(os.getenv("ACE_ROOT")) + r"\bin;" + \
        ##                         str(os.getenv("XERCESCROOT")) + \
        ##                         r"\Build\Win32\VC7\Debug"
        ##    os.environ["PATH"] = str(os.getenv("PATH")) + ";" + \
        ##                         str(os.getenv("OSGHOME")) + r"\bin"
        elif unix:
            ##Determine name of library path
            if os.getenv("CFDHOSTTYPE") == "IRIX64":
                libraryPath = "LD_LIBRARYN32_PATH"
                lib = "lib32"
            else:
                libraryPath = "LD_LIBRARY_PATH"
                lib = "lib"
            ##Prepare the current library path
            currentLibraryPath = str(os.getenv(libraryPath)) + ":"
            if currentLibraryPath == "None:":
                currentLibraryPath = ""
            print "CURRENT LIBRARY PATH: " + str(currentLibraryPath) ##TESTER
            ##Update the library path
            os.environ[libraryPath] = currentLibraryPath + \
                                      os.path.join(str(os.getenv("VE_DEPS_DIR")),
                                                   "bin") + ":" + \
                                      os.path.join(str(os.getenv("VE_INSTALL" + \
                                                                 "_DIR")),
                                                   "bin") + ":" + \
                                      os.path.join(str(os.getenv("VJ_BASE_DIR")),
                                                   lib)
            ##Update the path
            os.environ["PATH"] = str(os.getenv("PATH")) + ":" + \
                                 os.path.join(str(os.getenv("VE_INSTALL_DIR")),
                                              "bin") + ":" + \
                                 os.path.join(str(os.getenv("VE_DEPS_DIR")),
                                              "bin") + ":" + \
                                 os.path.join(str(os.getenv("VJ_BASE_DIR")),
                                              "bin")
##        print "\nPATH: \n" + str(os.getenv("PATH")) ##TESTER
##        print "\nLibrary Path " + libraryPath + ": \n" + \
##              str(os.getenv(libraryPath)) ##TESTER
##        print "Setup done." ##TESTER

    def EnvFill(self, var, default):
        """Sets environmental variable var to default if it is empty."""
        os.environ[var] = os.getenv(var, default)
##        print var + ": " + os.getenv(var) ##TESTER

##Jconf Override warning.
if JCONF_STANDARD:
    print "Jconf override ON: Standard Jconf files used; custom choices" + \
          " won't be used."
##The main loop
app = wx.PySimpleApp()
frame = LauncherWindow(None,-1,'VE Suite Launcher')
app.MainLoop()
