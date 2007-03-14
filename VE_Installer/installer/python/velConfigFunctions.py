"""Functions for saving/loading configurations in VE-Launcher."""
import wx
from velJconfDict import *
from velClusterDict import *
from velCoveredConfig import *
from velRecentFiles import *
from velBase import *

def SaveConfig(name, state, saveLastConfig = False):
    """Saves the current configuration under name.

    Keyword arguments:
    name -- What to name this configuration
    state -- Launcher's data
    saveLastConfig -- Is it saving the default configuration?"""
    ##Set config
    config = wx.Config.Get()
    config.SetPath('..')
    config.SetPath(name)
    ##Save the current configuration under name
    strWrites = ["DependenciesDir",
                 "BuilderDir",
                 "JconfSelection",
                 "NameServer",
                 "Xplorer",
                 "XplorerType",
                 "Conductor",
                 "TaoMachine",
                 "TaoPort",
                 "DesktopMode",
                 "ClusterMaster",
                 "OSGNotifyLevel",
                 "User",
                 "FileDir",
                 "ExtraVariables"]
    if saveLastConfig:
        ##Variables that only the main config stores.
        strWrites.append("Directory")
        strWrites.append("Debug")
        strWrites.append("AutoRunVes")
        strWrites.append("JugglerDep")
        state.GetBase("RecentFiles").WriteConfig() ##Has config.DeleteGroup
        state.GetBase("Dependencies").WriteConfig() ##Ditto.
    intWrites = ["Mode",
                 "VPRDebug",
                 "MasterWait",
                 "SlaveWait"]
    for var in strWrites:
        if state.GetBase(var) != None:
            config.Write(var, str(state.GetBase(var)))
    for var in intWrites:
        if state.GetBase(var) != None:
            config.WriteInt(var, state.GetBase(var))
    ##Redo the Jconf/Cluster configs
    config.DeleteGroup(JCONF_CONFIG)
    state.GetBase("JconfDict").WriteConfig()
    config.DeleteGroup(CLUSTER_CONFIG)
    state.GetBase("ClusterDict").WriteConfig()
    ##Return to default config
    config.SetPath('..')
    config.SetPath(DEFAULT_CONFIG)
    config.Flush()
    return

def LoadConfig(name, state, loadLastConfig = False):
    """Loads the configuration under name.

    Keyword arguments:
    name -- Name of configuration to load
    state -- Launcher's data
    loadLastConfig -- Is it loading the default configuration?"""
    ##Set config
    config = wx.Config.Get()
    path = '../%s' % name
    if not config.HasGroup(path) and path != '../%s' % DEFAULT_CONFIG:
        raise NonexistantConfigError(name)
    config.SetPath(path)
    ##Set the configs read in.
    strReads = ["ClusterMaster",
                "DependenciesDir",
                "JconfSelection",
                "TaoMachine",
                "BuilderDir",
                "OSGNotifyLevel",
                "User",
                "FileDir",
                "ExtraVariables"]
    intReads = ["Mode",
                "VPRDebug",
                "MasterWait",
                "SlaveWait"]
    boolReads = ["NameServer",
                 "Conductor",
                 "Xplorer",
                 "DesktopMode"]
    ##Load these if it's loading the initial configuration.
    if loadLastConfig:
        strReads.append("Directory")
        boolReads.append("Debug")
        boolReads.append("AutoRunVes")
        strReads.append("JugglerDep")
        if not EnvVarEmpty("VJ_BASE_DIR"):
            state.Edit("JugglerDep", os.getenv("VJ_BASE_DIR"))
        if config.Exists(RECENTFILES_CONFIG):
            state.Edit("RecentFiles", RecentFiles())
        if config.Exists(DEPS_CONFIG):
            state.Edit("Dependencies", DepsArray())
    ##Workaround for error w/ Int TaoPort in earlier version (could probably be removed now)
    if config.GetEntryType("TaoPort") == 3: ##3: Int entry type
        intReads.append("TaoPort")
    else:
        strReads.append("TaoPort")
    ##Workaround for int/str XplorerType change.
    if config.GetEntryType("XplorerType") == 3 or \
       str(config.Read("XplorerType")).isdigit(): ##3: Int entry type
        if config.ReadInt("XplorerType") >= 2:
            state.Edit("XplorerType", DEFAULT_CLUSTER_XPLORER)
        else:
            state.Edit("XplorerType", DEFAULT_SOLO_XPLORER)
    else:
        strReads.append("XplorerType")
    ##Read in the configs.
    for var in strReads:
        if config.Exists(var):
            state.Edit(var, config.Read(var))
    for var in intReads:
        if config.Exists(var):
            state.Edit(var, config.ReadInt(var))
    for var in boolReads:
        if config.Exists(var):
            if config.Read(var) == "True":
                result = True
            elif config.Read(var) == "False":
                result = False
            else:
                print 'ERROR! Saved boolean var not "True" or "False".'
                result = None
            state.Edit(var, result)
    ##Set Jconf dictionary.
    if config.Exists(JCONF_CONFIG):
        state.Edit("JconfDict", JconfDict())
    ##Set Cluster list.
    if config.Exists(CLUSTER_CONFIG):
        state.Edit("ClusterDict", ClusterDict())
    ##Set RecentFiles list.
    ##Return to default config
    config.SetPath('..')
    config.SetPath(DEFAULT_CONFIG)
    return
