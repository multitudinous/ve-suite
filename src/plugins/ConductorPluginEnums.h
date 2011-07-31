/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CONDUCTORPLUGINENUMS_H
#define CONDUCTORPLUGINENUMS_H

enum
{
    UPDATE_HIER_TREE = 9999,
    PLUGIN_BEGIN_INDEX = 6000,

    //
    //ADPlugin.h
    //
    ADPLUGIN_SET_UNIT,
    ADPLUGIN_OPEN_SIM,
    ADPLUGIN_SHOW_ASPEN_SIMULATION,
    ADPLUGIN_HIDE_ASPEN_SIMULATION,
    ADPLUGIN_CLOSE_ASPEN_SIMULATION,
    ADPLUGIN_RUN_ASPEN_NETWORK,
    ADPLUGIN_REINITIALIZE_ASPEN_SIMULATION,
    ADPLUGIN_STEP_ASPEN_NETWORK,
    ADPLUGIN_SAVE_SIMULATION,
    ADPLUGIN_SAVEAS_SIMULATION,
    ADPLUGIN_ASPEN_MENU,


    //
    //ADOpenDialog
    //
    ADOPENDIALOG_CANCELBUTTON,
    ADOPENDIALOG_OKBUTTON,


    //
    //APPlugin.h
    //
    APPLUGIN_SET_UNIT,
    APPLUGIN_OPEN_SIM,
    APPLUGIN_SHOW_ASPEN_SIMULATION,
    APPLUGIN_HIDE_ASPEN_SIMULATION,
    APPLUGIN_CLOSE_ASPEN_SIMULATION,
    APPLUGIN_RUN_ASPEN_NETWORK,
    APPLUGIN_REINITIALIZE_ASPEN_SIMULATION,
    APPLUGIN_DISCONNECT_ASPEN_SIMULATION,
    APPLUGIN_STEP_ASPEN_NETWORK,
    APPLUGIN_SAVE_SIMULATION,
    APPLUGIN_SAVEAS_SIMULATION,
    APPLUGIN_ASPEN_MENU,


    //
    //APOpenDialog
    //
    APOPENDIALOG_CANCELBUTTON,
    APOPENDIALOG_OKBUTTON,

    
    //
    //DSPlugin.h
    //
    DSPLUGIN_SET_UNIT,
    DSPLUGIN_OPEN_SIM,
    DSPLUGIN_DYNSIM_MENU,
    DSPLUGIN_CREATE_OPC_LIST,
    DSPLUGIN_CONNECT,
    DSPLUGIN_TIMER_ID,
    DSPLUGIN_ADDVAR,
    DSPLUGIN_ALLVAR,

    //
    //DSOpenDialog
    //
    DSOPENDIALOG_CANCELBUTTON,
    DSOPENDIALOG_OKBUTTON,

    //
    //DWPlugin.h
    //
    DWPLUGIN_SET_UNIT,
    DWPLUGIN_OPEN_SIM,
    DWPLUGIN_SHOW_SIMULATION,
    DWPLUGIN_HIDE_SIMULATION,
    DWPLUGIN_CLOSE_SIMULATION,
    DWPLUGIN_RUN_NETWORK,
    DWPLUGIN_REINITIALIZE_SIMULATION,
    DWPLUGIN_STEP_NETWORK,
    DWPLUGIN_SAVE_SIMULATION,
    DWPLUGIN_SAVEAS_SIMULATION,
    DWPLUGIN_MENU,
    DWPLUGIN_INPUTS,
    DWPLUGIN_OUTPUTS,
    DWPLUGIN_SET_INPUTS,


    //
    //DWOpenDialog
    //
    DWOPENDIALOG_CANCELBUTTON,
    DWOPENDIALOG_OKBUTTON,

    
    //
    //DW Var Dialog
    //
    DW_VAR_ID_MONITORBUTTON,
    DW_VAR_ID_CANCELBUTTON,
    DW_VAR_ID_SETBUTTON,
    DW_VAR_ID_WXGRID,

    //
    //ADUOPlugin.h
    //
    ADUOPLUGIN_SHOW_ASPEN_NAME,
    ADUOPLUGIN_QUERY_DYNAMICS,
    ADUOPLUGIN_ASPEN_MENU,
    ADUOPLUGIN_TIMER_ID,
    ADUOPLUGIN_STOP_TIMER,
    
    //
    //AD Var Dialog
    //
    AD_VAR_ID_MONITORBUTTON,
    AD_VAR_ID_CANCELBUTTON,
    AD_VAR_ID_SETBUTTON,
    AD_VAR_ID_WXGRID,

    //
    //APUOPlugin.h
    //
    APUOPLUGIN_SHOW_ASPEN_NAME,
    APUOPLUGIN_QUERY_INPUTS,
    APUOPLUGIN_QUERY_OUTPUTS,
    APUOPLUGIN_REINIT_BLOCK,
    APUOPLUGIN_ASPEN_MENU,

    //
    //DSUOPlugin.h
    //
    DSUOPLUGIN_DYNSIM_MENU,

    //
    //OPCUOPlugin.h
    //
    OPCUOPLUGIN_SHOW_VALUE,
    OPCUOPLUGIN_SIM_MENU,
    OPCUOPLUGIN_TIMER_ID,
    OPCUOPLUGIN_START_TIMER,
    OPCUOPLUGIN_STOP_TIMER,
    OPCUOPLUGIN_ALL_VAR,
    //
    //PSPlugin.h
    //
    PS_PLUGIN_OPEN,
    PS_PLUGIN_MENU,

    PLUGIN_END_INDEX
};

#endif //CONDUCTORPLUGINENUMS_H
