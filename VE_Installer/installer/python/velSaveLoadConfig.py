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
    ##Set the Overridden list.
    ##Stores the saved variables overwritten by the environment.
    overriddenVariables = []
    ##Set the configs read in.
    strReads = {"ClusterMaster": os.getenv("VEXMASTER"),
                "DependenciesDir": os.getenv("VE_DEPS_DIR"),
                "JconfSelection": None,
                "TaoMachine": os.getenv("TAO_MACHINE"),
                "TaoPort": os.getenv("TAO_PORT"),
                "BuilderDir": None,
                "OSGNotifyLevel": os.getenv("OSGNOTIFYLEVEL"),
                "User": None,
                "FileDir": None,
                "ExtraVariables": None}
    intReads = {"Mode": None,
                "VPRDebug": GetVPRDebug(),
                "MasterWait": None,
                "SlaveWait": None}
    boolReads = {"NameServer": None,
                 "Conductor": None,
                 "Xplorer": None,
                 "DesktopMode": None}
    ##Load these if it's loading the initial configuration.
    if loadLastConfig:
        strReads["Directory"] = os.getenv("VE_WORKING_DIR")
        boolReads["Debug"] = None
        boolReads["AutoRunVes"] = None
        strReads["JugglerDep"] = os.getenv("VJ_BASE_DIR")
        if config.Exists(RECENTFILES_CONFIG):
            state.Edit("RecentFiles", RecentFiles())
        if config.Exists(DEPS_CONFIG):
            state.Edit("Dependencies", DepsArray())
    ##Workaround for int/str XplorerType change.
    if config.GetEntryType("XplorerType") == 3 or \
       str(config.Read("XplorerType")).isdigit(): ##3: Int entry type
        if config.ReadInt("XplorerType") >= 2:
            state.Edit("XplorerType", DEFAULT_CLUSTER_XPLORER)
        else:
            state.Edit("XplorerType", DEFAULT_SOLO_XPLORER)
    else:
        strReads["XplorerType"] = None
    ##Read in the configs.
    for var in strReads:
        if strReads[var] != None:
            overriddenVariables.append(var)
            state.Edit(var, strReads[var])
        elif config.Exists(var):
            state.Edit(var, config.Read(var))
    for var in intReads:
        if intReads[var] != None:
            overriddenVariables.append(var)
            state.Edit(var, intReads[var])
        elif config.Exists(var):
            state.Edit(var, config.ReadInt(var))
    for var in boolReads:
        if boolReads[var] != None:
            overriddenVariables.append(var)
            envBool = boolReads[var]
            if envBool.lower() in ["f", "false", "0"]:
                envBool = "False"
            else:
                envBool = "True"
            state.Edit(var, boolReads[var])
        elif config.Exists(var):
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
    ##TESTER; NOTE: Replace with better response later.
    if len(overriddenVariables) > 0:
        print "Overridden by environment:"
        for var in overriddenVariables:
            print var
    return

def GetVPRDebug():
    """Grabs the env's VPR Debug from VPR_DEBUG _ENABLE & _NFY_LEVEL."""
    vprEnable = os.getenv("VPR_DEBUG_ENABLE")
    vprLevel = os.getenv("VPR_DEBUG_NFY_LEVEL")
    if vprEnable == "0":
        return -1
    elif vprEnable == "1" and vprLevel != None:
        return int(vprLevel)
    else:
        return None
