"""Functions for saving/loading configurations in VE-Launcher."""
import wx
from velJconfDict import *
from velClusterDict import *
from velCoveredConfig import *
from velBase import *

def SaveConfig(name, state, saveLastConfig = False):
    """Saves the current configuration under name.

    Keyword arguments:
    name -- What to name this configuration
    state -- Launcher's data"""
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
                 "Conductor",
                 "TaoMachine",
                 "TaoPort",
                 "DesktopMode",
                 "ClusterMaster",
                 "OSGNotifyLevel",
                 "User"]
    if saveLastConfig:
        strWrites.append("Directory")
        strWrites.append("Debug")
    intWrites = ["XplorerType",
                 "Mode",
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
    state -- Launcher's data"""
    ##Set config
    config = wx.Config.Get()
    config.SetPath('..')
    config.SetPath(name)
    ##Set the configs read in.
    strReads = ["ClusterMaster",
                "DependenciesDir",
                "JconfSelection",
                "TaoMachine",
                "BuilderDir",
                "OSGNotifyLevel",
                "User"]
    intReads = ["XplorerType",
                "Mode",
                "VPRDebug",
                "MasterWait",
                "SlaveWait"]
    boolReads = ["NameServer",
                 "Conductor",
                 "Xplorer",
                 "DesktopMode"]
    if loadLastConfig:
        strReads.append("Directory")
        boolReads.append("Debug")
    ##Workaround for error w/ Int TaoPort in earlier version
    if config.GetEntryType("TaoPort") == 3: ##3: Int entry type
        intReads.append("TaoPort")
    else:
        strReads.append("TaoPort")
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
    ##Restrict XplorerType's value.
    test = state.GetBase(var = "XplorerType")
    if test < 0 or test >= len(RADIO_XPLORER_LIST):
        state.Edit("XplorerType", 0)
    ##Return to default config
    config.SetPath('..')
    config.SetPath(DEFAULT_CONFIG)
    return
