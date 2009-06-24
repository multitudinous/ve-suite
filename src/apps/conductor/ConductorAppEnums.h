//http://docs.wxwidgets.org/stable/wx_eventhandlingoverview.html#eventhandlingoverview
#ifndef CONDUCTORAPPENUMS_H
#define CONDUCTORAPPENUMS_H
    
enum
{
    //for updating hierarchy tree
    APPFRAME_UPDATE_HIER_TREE = 9999,
    APP_BEGIN_INDEX = 1000,

    //
    //AppFrame.h
    //
    APPFRAME_OPEN,
    APPFRAME_SAVE,
    APPFRAME_SAVEAS,
    APPFRAME_RUN,
    APPFRAME_NEW,
    APPFRAME_START_DATALOGGING,
    APPFRAME_STOP_DATALOGGING,

    APPFRAME_V21ID_CONNECT,
    APPFRAME_V21ID_CONNECT_VE,
    APPFRAME_V21ID_SUBMIT,
    APPFRAME_V21ID_LOAD,
    APPFRAME_QUERY_FROM_SERVER,
    APPFRAME_CONDUCTOR_FIND,
    APPFRAME_V21ID_DISCONNECT,
    APPFRAME_V21ID_DISCONNECT_VE,
    APPFRAME_V21ID_UNDO,
    APPFRAME_V21ID_REDO,
    APPFRAME_V21ID_ZOOMIN,
    APPFRAME_V21ID_ZOOMOUT,
    APPFRAME_V21ID_ZOOMALL,
    APPFRAME_V21ID_START_CALC,
    APPFRAME_V21ID_STOP_CALC,
    APPFRAME_V21ID_PAUSE_CALC,
    APPFRAME_V21ID_RESUME_CALC,
    APPFRAME_V21ID_VIEW_RESULT,
    APPFRAME_V21ID_GLOBAL_PARAM,
    APPFRAME_V21ID_BASE,
    APPFRAME_V21ID_SOUR,
    APPFRAME_V21ID_REI_BASE,
    APPFRAME_V21ID_REI_SOUR,
    APPFRAME_V21ID_SWEET,
    APPFRAME_V21ID_CO_DISPOSAL,
    APPFRAME_MYLOG,
    APPFRAME_V21ID_HELP,
    APPFRAME_V21ID_ABOUT,
    APPFRAME_V21ID_REVISION,
    APPFRAME_V21ID_CONTACTS,
    APPFRAME_V21ID_PLATFORM,
    
    APPFRAME_XPLORER_DEVICE,
    APPFRAME_WAND,
    APPFRAME_KEYBOARD_MOUSE,
    APPFRAME_GLOVES,
    APPFRAME_DEVICE_PROPERTIES,
    
    APPFRAME_XPLORER_DISPLAY,
    APPFRAME_FRAME_RATE,
    APPFRAME_COORDINATE_SYSTEM,
    APPFRAME_XPLORER_VIEW,
    
    APPFRAME_FRAME_ALL,
    APPFRAME_FRAME_SELECTION,
    APPFRAME_RESET,
    APPFRAME_XPLORER_NAVIGATION,
    APPFRAME_XPLORER_VIEWPOINTS,
    APPFRAME_XPLORER_STREAMLINE,

    APPFRAME_JUGGLER_STEREO,
    APPFRAME_JUGGLER_MONO,
    APPFRAME_JUGGLER_SETTINGS,

    APPFRAME_XPLORER_SCENES,
    APPFRAME_XPLORER_EXIT,
    APPFRAME_XPLORER_COLOR,
    APPFRAME_XPLORER_EPHEMERIS,
    APPFRAME_PREFERENCES,
    APPFRAME_CHANGE_XPLORER_VIEW,
    APPFRAME_CHANGE_XPLORER_VIEW_NETWORK,
    APPFRAME_CHANGE_XPLORER_VIEW_CAD,
    APPFRAME_CHANGE_XPLORER_VIEW_LOGO,
    APPFRAME_XPLORER_DATALOGGING_LOOPING,
    
    APPFRAME_CLEAR_RECENT_FILES,
    //For debugging purposes
    APPFRAME_v21ID_DUMMY,

    //Always enum these last
    APPFRAME_OPEN_RECENT_CONNECTION_MENU,
    APPFRAME_v21ID_BASE_RECENT,
    //Export menu
    APPFRAME_EXPORT_MENU_OPT,
    //change working dir
    APPFRAME_CHANGE_WORKING_DIRECTORY,
    APPFRAME_TIMER_ID,

    APPFRAME_MINERVA_ADD_PLANET,
    APPFRAME_MINERVA_REMOVE_PLANET,

    //
    //AvailableModules.h
    //
    AVAILABLEMODULES_DESC,
    AVAILABLEMODULES_HELP,
    AVAILABLEMODULES_ADD,
    AVAILABLEMODULES_TREE_CTRL,


    //
    //DeviceProperties.h
    //
    DEVICEPROPERTIES_SPLITTERWINDOW,
    DEVICEPROPERTIES_LISTBOX,
    DEVICEPROPERTIES_TRACKBALL_PANEL,
    DEVICEPROPERTIES_WAND_PANEL,
    DEVICEPROPERTIES_ANIMATE_CHECKBOX,


    //
    //GUI Enum Control ID Start
    //
    EPHEMERISDIALOG_AMPM,
    EPHEMERISDIALOG_LONHEMISPHERE,
    EPHEMERISDIALOG_LONMINUTESSYMBOL,
    EPHEMERISDIALOG_LONGITUDEMINUTES,
    EPHEMERISDIALOG_DEGREELONSYMBOL,
    EPHEMERISDIALOG_LONGITUDEDEGREE,
    EPHEMERISDIALOG_LATHEMISPHERE,
    EPHEMERISDIALOG_MINUTESSYMBOL,
    EPHEMERISDIALOG_LATITUDEMINUTES,
    EPHEMERISDIALOG_DEGREESYMBOL,
    EPHEMERISDIALOG_LATDEGREES,
    EPHEMERISDIALOG_MINUTES,
    EPHEMERISDIALOG_HOURCOLON,
    EPHEMERISDIALOG_HOUR,
    EPHEMERISDIALOG_CALENDAR,
    EPHEMERISDIALOG_LATITUDELONGITUDE,
    EPHEMERISDIALOG_DATETIME,
    EPHEMERISDIALOG_DATAENTRYPAGES,
    EPHEMERISDIALOG_AUTO_DATE_TIME,
    EPHEMERISDIALOG_SAVE_LOCATION_BUTTON,
    EPHEMERISDIALOG_LOAD_LOCATION_BUTTON,
    EPHEMERISDIALOG_LOAD_HEIGHT_MAP,
    EPHEMERISDIALOG_TOGGLE_EPHEMERIS,


    //
    //ExportMenu.h
    //
    EXPORTMENU_SCREEN_SHOT = 1950,
    EXPORTMENU_DOT_FILE,


    //
    //HierarchyTree.h
    //
    HIERARCHYTREE_CTRL,
    
    //
    //NavigationPane.h
    //
    NAVIGATIONPANE_LEFT_B,
    NAVIGATIONPANE_RIGHT_B,
    NAVIGATIONPANE_UP_B,
    NAVIGATIONPANE_DOWN_B,
    NAVIGATIONPANE_FORWARD_B,
    NAVIGATIONPANE_BACKWARD_B,
    NAVIGATIONPANE_CCW_B,
    NAVIGATIONPANE_CW_B,
    NAVIGATIONPANE_TRANS_STEP_SLIDER,
    NAVIGATIONPANE_ROT_STEP_SLIDER,
    NAVIGATIONPANE_HEAD_ROTATE_CHK,
    NAVIGATIONPANE_SUB_ZERO_CHK,
    NAVIGATIONPANE_RESET_NAV_POSITION,
    NAVIGATIONPANE_STORE_START_POSITION,
    NAVIGATIONPANE_UPDATE_TIMER_ID,  
    
    
    //
    //Splitter.h
    //
    SPLITTER_SPLIT_WINDOW,


    //
    //UITeacherTab.h
    //
    UITEACHERTAB_RBOX,
    UITEACHERTAB_CLEAR_BUTTON,
    UITEACHERTAB_RECORD_SCENE,


    //
    //UserPrefences.h
    //
    USERPREFENCES_NAVIGATION_CHKBX,
    USERPREFENCES_BACKGROUND_COLOR_BUTTON,
    USERPREFENCES_SHUTDOWN_XPLORER,
    USERPREFENCES_GEOMETRY_LOD_SCALE_SLIDER,
    USERPREFENCES_CONDUCTOR_CHKBX,
    USERPREFENCES_NEAR_FAR_CHKBX,
    USERPREFENCES_NEAR_FAR_RATIO,

    //
    //ViewLocPane.h
    //
    VIEWLOCPANE_LOAD_BUTTON,
    VIEWLOCPANE_ACCEPTNEWVPNAME_BUTTON,
    VIEWLOCPANE_CANCELNEWVPNAME_BUTTON,
    VIEWLOCPANE_REMOVEVP_COMBOBOX,
    VIEWLOCPANE_MOVETOVP_COMBOBOX,
    VIEWLOCPANE_NEWFLY_BUTTON,
    VIEWLOCPANE_ACCEPTNEWFLYNAME_BUTTON,
    VIEWLOCPANE_CANCELNEWFLYNAME_BUTTON,
    VIEWLOCPANE_ACTIVEFLYSEL_COMBOBOX,
    VIEWLOCPANE_ADDVPTOFLYSEL_COMBOBOX,
    VIEWLOCPANE_INSERTVPINFLYSEL_COMBOBOX,
    VIEWLOCPANE_REMOVEVPFROMFLYSEL_COMBOBOX,
    VIEWLOCPANE_RUNFLY_BUTTON,
    VIEWLOCPANE_STOPFLY_BUTTON,
    VIEWLOCPANE_FLYBUILDER_LISTBOX,
    VIEWLOCPANE_DELETEFLYSEL_COMBOBOX,
    VIEWLOCPANE_SPEED_CONTROL_SLIDER,
    VIEWLOCPANE_LOAD_FILE,
    VIEWLOCPANE_SAVE_FILE,
    VIEWLOCPANE_REMOVE_VIEW_PT_BUTTON,
    VIEWLOCPANE_SPEED_CONTROL_SPIN,
    
    //
    //AppToolBar.h
    //
    APP_TOOL_BAR_NULL,
    
    APP_TOOL_BAR_NEW,
    APP_TOOL_BAR_OPEN,
    APP_TOOL_BAR_SAVE,
    
    APP_TOOL_BAR_CURSOR,
    APP_TOOL_BAR_CURSOR_SELECT,
    APP_TOOL_BAR_WORLD_NAVIGATION,
    APP_TOOL_BAR_WORLD_NAVIGATION_SELECT,
    APP_TOOL_BAR_OBJECT_NAVIGATION,
    APP_TOOL_BAR_OBJECT_NAVIGATION_SELECT,
    APP_TOOL_BAR_UNSELECT,
    
    APP_TOOL_BAR_MANIPULATOR,
    APP_TOOL_BAR_MANIPULATOR_SELECT,
    APP_TOOL_BAR_MANIPULATOR_TRANSLATE,
    APP_TOOL_BAR_MANIPULATOR_TRANSLATE_SELECT,
    APP_TOOL_BAR_MANIPULATOR_TRANSLATE_DISABLED,
    APP_TOOL_BAR_MANIPULATOR_ROTATE,
    APP_TOOL_BAR_MANIPULATOR_ROTATE_SELECT,
    APP_TOOL_BAR_MANIPULATOR_ROTATE_DISABLED,
    APP_TOOL_BAR_MANIPULATOR_SCALE,
    APP_TOOL_BAR_MANIPULATOR_SCALE_SELECT,
    APP_TOOL_BAR_MANIPULATOR_SCALE_DISABLED,
    APP_TOOL_BAR_MANIPULATOR_COMBO,
    APP_TOOL_BAR_MANIPULATOR_COMBO_SELECT,
    APP_TOOL_BAR_MANIPULATOR_COMBO_DISABLED,
    
    APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP,
    APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP_SELECT,
    APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP,
    APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP_SELECT,
    APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP,
    APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP_SELECT,
    APP_TOOL_BAR_BB_CENTER_POINT_JUMP,
    APP_TOOL_BAR_BB_CENTER_POINT_JUMP_SELECT,
    APP_TOOL_BAR_RESET_CENTER_POINT,
    
    APP_TOOL_BAR_PHYSICS,
    APP_TOOL_BAR_PHYSICS_SELECT,
    APP_TOOL_BAR_PHYSICS_CHARACTER,
    APP_TOOL_BAR_PHYSICS_CHARACTER_SELECT,
    APP_TOOL_BAR_PHYSICS_CHARACTER_DISABLED,
    APP_TOOL_BAR_PHYSICS_RESET,
    APP_TOOL_BAR_PHYSICS_RESET_DISABLED,
    APP_TOOL_BAR_PHYSICS_PAUSE,
    APP_TOOL_BAR_PHYSICS_PAUSE_SELECT,
    APP_TOOL_BAR_PHYSICS_PAUSE_DISABLED,
    APP_TOOL_BAR_PHYSICS_PLAY,
    APP_TOOL_BAR_PHYSICS_PLAY_SELECT,
    APP_TOOL_BAR_PHYSICS_PLAY_DISABLED,
    APP_TOOL_BAR_PHYSICS_STEP,
    APP_TOOL_BAR_PHYSICS_STEP_DISABLED,
    
    APP_TOOL_BAR_SUMMIT_JOB
};

#endif //CONDUCTORAPPENUMS_H
