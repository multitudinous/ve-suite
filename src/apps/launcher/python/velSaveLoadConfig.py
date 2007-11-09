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
                 "ExtraVariables",
                 "AutoShutDown",
                 "EnableVSync",
                 "CurrentConfig",
                 "EnableDefWorkingDir",
                 "DefaultWorkingDir",
                 "RunDebugPrograms"]
    if saveLastConfig:
        ##Variables that only the main config stores.
        strWrites.append("Directory")
        strWrites.append("Debug")
        strWrites.append("AutoRunVes")
        strWrites.append("JugglerDep")
        state.GetBase("RecentFiles").WriteConfig() ##Has config.DeleteGroup
        state.GetBase("Dependencies").WriteConfig() ##Ditto.
    intWrites = ["Mode",
                 "AddMode",
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
    ##Jump to the Global JconfList
    path = '../%s' % DEFAULT_CONFIG
    config.SetPath(path)
    config.DeleteGroup(JCONF_CONFIG)
    state.GetBase("JconfDict").WriteConfig()
    
     ##Back to the own configuration  
    path = '../%s' % name
    config.SetPath(path)
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
                "OSGNotifyLevel": None,
                "User": None,
                "FileDir": None,
                "DefaultWorkingDir": None,
                "CurrentConfig": None,
                "ExtraVariables": None}
    intReads = {"Mode": None,
                "AddMode": None,
                "VPRDebug": GetVPRDebug(),
                "MasterWait": None,
                "SlaveWait": None}
    boolReads = {"NameServer": None,
                 "Conductor": None,
                 "Xplorer": None,
                 "DesktopMode": None,
                 "AutoShutDown": None,
                 "EnableVSync": None,
                 "EnableDefWorkingDir": None,
                 "RunDebugPrograms": None}
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
                result = False
            state.Edit(var, result)

    ##Set Jconf dictionary.
    ##Jump to the Global JconfList
    path = '../%s' % DEFAULT_CONFIG
    config.SetPath(path)
    
    if config.Exists(JCONF_CONFIG):
        state.Edit("JconfDict", JconfDict())
    ##Set Cluster list.
    ##Back to the own configuration  
    path = '../%s' % name
    config.SetPath(path)
    
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
