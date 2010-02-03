#ifndef CONDUCTORPLUGINENUMS_H
#define CONDUCTORPLUGINENUMS_H

enum
{
    UPDATE_HIER_TREE = 9999,
    PLUGIN_BEGIN_INDEX = 6000,

    //
    //ADPlugin.h
    //
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
    DSPLUGIN_OPEN_SIM,
    DSPLUGIN_DYNSIM_MENU,
    DSPLUGIN_CREATE_OPC_LIST,
	DSPLUGIN_CONNECT,
	DSPLUGIN_TIMER_ID,


    //
    //DSOpenDialog
    //
    DSOPENDIALOG_CANCELBUTTON,
    DSOPENDIALOG_OKBUTTON,

    //
    //ADUOPlugin.h
    //
    ADUOPLUGIN_SHOW_ASPEN_NAME,
    ADUOPLUGIN_QUERY_DYNAMICS,
    ADUOPLUGIN_ASPEN_MENU,

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
    //
    //PSPlugin.h
    //
    PS_PLUGIN_OPEN,
    PS_PLUGIN_MENU,

    PLUGIN_END_INDEX
};

#endif //CONDUCTORPLUGINENUMS_H
