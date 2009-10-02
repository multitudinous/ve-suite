#ifndef CONDUCTORLIBENUMS_H
#define CONDUCTORLIBENUMS_H

enum CONDUCTOR_LIB_ENUMS
{
    LIB_BEGIN_INDEX = 2000,

    //
    //AdvancedContours.h
    //
    ADVANCEDCONTOURS_OPACITY_SLIDER,
    ADVANCEDCONTOURS_WARPED_SCALE_SLIDER,
    ADVANCEDCONTOURS_LOD_SLIDER,
    ADVANCEDCONTOURS_CONTOUR_TYPE_RBOX,
    ADVANCEDCONTOURS_WARP_OPTION_CHK,


    //
    //AdvancedIsosurface.h
    //
    ADVANCEDISOSURFACE_MIN_SPINCTRL,
    ADVANCEDISOSURFACE_MAX_SPINCTRL,
    ADVANCEDISOSURFACE_MIN_SLIDER,
    ADVANCEDISOSURFACE_MAX_SLIDER,
    ADVANCEDISOSURFACE_SELECT_SCALAR,


    //
    //AdvancedStreamlines.h
    //
    ADVANCEDSTREAMLINES_PARTICLE_TRACKING_BUTTON,
    ADVANCEDSTREAMLINES_USE_SEED_POINT_CHK,
    ADVANCEDSTREAMLINES_PROPOGATION_SLIDER,
    ADVANCEDSTREAMLINES_INTEGRATION_STEP_SLIDER,
    ADVANCEDSTREAMLINES_ARROWS_CHK,
    ADVANCEDSTREAMLINES_SPHERE_SIZE_SLIDER,
    ADVANCEDSTREAMLINES_LINE_DIAMETER_SLIDER,
    ADVANCEDSTREAMLINES_GLOW_SLIDER,
    ADVANCEDSTREAMLINES_PARTICLE_TRACKING,
    ADVANCEDSTREAMLINES_PARTICLE_TRACKING_DIALOG,

    //
    //AdvancedVectors.h
    //
    ADVANCEDVECTORS_MAX_SLIDER,
    ADVANCEDVECTORS_MIN_SLIDER,
    ADVANCEDVECTORS_SCALE_SLIDER,
    ADVANCEDVECTORS_RATIO_SLIDER,
    ADVANCEDVECTORS_SCALAR_BY_VECTOR_CHK,

    //
    //Canvas.h
    //
    CANVAS_UPDATE_NETWORK_DATA,

    //
    //Contours.h
    //
    CONTOURS_DIR_RBOX,
    CONTOURS_MULTIPLE_PRECONTOUR_RBUTTON,
    CONTOURS_MULTIPLE_PRECONTOUR_CHK,
    CONTOURS_SINGLE_PRECONTOUR_RBUTTON,
    CONTOURS_SINGLE_PRECONTOUR_CHK,
    CONTOURS_PLANE_SLIDER,
    CONTOURS_ADD_CONTOUR_PLANE_BUTTON,
    CONTOURS_ADVANCED_CONTOUR_BUTTON,
    CONTOURS_PLANE_SPINNER,
    CONTOURS_GPU_TOOLS_CHK,

    //
    //FinancialDialog.h
    //
    FINANCIALDIALOG_RADIO_FDA,
    FINANCIALDIALOG_RADIO_FDB,
    FINANCIALDIALOG_CC00,
    FINANCIALDIALOG_CC01,
    FINANCIALDIALOG_CC02,
    FINANCIALDIALOG_CC03,
    FINANCIALDIALOG_CC04,
    FINANCIALDIALOG_CC05,
    FINANCIALDIALOG_CC06,
    FINANCIALDIALOG_CC07,
    FINANCIALDIALOG_CC08,
    FINANCIALDIALOG_OM00,
    FINANCIALDIALOG_OM01,
    FINANCIALDIALOG_OM02,
    FINANCIALDIALOG_OM03,


    //
    //FindDialog.h
    //
    FINDDIALOG_STREAMLABEL,
    FINDDIALOG_WXCHOICE2,
    FINDDIALOG_UNITLABEL,
    FINDDIALOG_WXCHOICE1,
    FINDDIALOG_FINDBUTTON,


    //
    //GlobalParamDialog.h
    //
    GLOBALPARAMDIALOG_PLANT_CAP,
    GLOBALPARAMDIALOG_YEAR_COSTS,
    GLOBALPARAMDIALOG_CST_CUR_DOLLAR,
    GLOBALPARAMDIALOG_FIXED_CHARGE,
    GLOBALPARAMDIALOG_DISCNT_RATE,
    GLOBALPARAMDIALOG_INFLATION_RATE,
    GLOBALPARAMDIALOG_PLANT_LIFE,
    GLOBALPARAMDIALOG_BOND_INTEREST,
    GLOBALPARAMDIALOG_PREFERRED_STOCK_RETURN,
    GLOBALPARAMDIALOG_COMMON_STOCK_RETURN,
    GLOBALPARAMDIALOG_PERCENT_DEBT,
    GLOBALPARAMDIALOG_PERCENT_P_EQUITY,
    GLOBALPARAMDIALOG_PERCENT_C_EQUITY,
    GLOBALPARAMDIALOG_FED_TAX,
    GLOBALPARAMDIALOG_STATE_TAX,
    GLOBALPARAMDIALOG_PROPERTY_TAX,
    GLOBALPARAMDIALOG_INVEST_TAX_CREDIT,
    GLOBALPARAMDIALOG_RADIO_A,
    GLOBALPARAMDIALOG_RADIO_B,


    //
    //IconChooser.h
    //
    ICONCHOOSER_OK,
    ICONCHOOSER_CANCEL,
    ICONCHOOSER_CLICK,


    //
    //IsoSurfaces.h
    //
    ISOSURFACES_RBUTTON,
    ISOSURFACES_PRECOMPUTED_ISO_CHK,
    ISOSURFACES_PLANE_SLIDER,
    ISOSURFACES_ADD_ISOSURFACE_BUTTON,
    ISOSURFACES_ADVANCED_ISOSURFACE_BUTTON,
    ISOSURFACES_SPINCTRL,
    ISOSURFACES_SCALAR_SPINCTRL,

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
    //Network.h
    //
    NETWORK_ADD_TAG = 3250,
    NETWORK_EDIT_TAG,
    NETWORK_DEL_TAG,
    NETWORK_DELETE_NETWORK,
    NETWORK_UPDATE_TIMER_ID,

    //
    //PolyData.h
    //
    POLYDATA_RBUTTON,
    POLYDATA_WARPED_SURFACE_CHK,
    POLYDATA_PLANE_SLIDER,
    POLYDATA_ADD_POLYDATA_BUTTON,
    POLYDATA_ADVANCED_POLYDATA_BUTTON,


    //
    //QueryInputsDlg.h
    //
    QUERYINPUTSDLG_STATICTEXT2,
    QUERYINPUTSDLG_STATICTEXT1,
    QUERYINPUTSDLG_BUTTON4,
    QUERYINPUTSDLG_BUTTON3,
    QUERYINPUTSDLG_BUTTON2,
    QUERYINPUTSDLG_BUTTON1,
    QUERYINPUTSDLG_LISTBOX2,
    QUERYINPUTSDLG_LISTBOX1,


    //
    //ResultPanel.h
    //
    RESULTPANEL_MW_GROSS,
    RESULTPANEL_MW_NET,
    RESULTPANEL_NET_EFF,
    RESULTPANEL_COAL_IN,
    RESULTPANEL_WATER_IN,
    RESULTPANEL_OXID_IN,
    RESULTPANEL_NOX_CONS,
    RESULTPANEL_CO2_IN,
    RESULTPANEL_CO2_OUT,
    RESULTPANEL_CO2_CAP,
    RESULTPANEL_CAPITAL_CST,
    RESULTPANEL_ELEC_CST,


    //
    //StreamLines.h
    //
    STREAMLINES_CURSOR_RBOX,
    STREAMLINES_DIRECTION_RBOX,
    STREAMLINES_INTEGRATION_DIR_RBOX,
    STREAMLINES_PLANE_SIZE_SLIDER,
    STREAMLINES_NUMBER_PTS_SLIDER,
    STREAMLINES_ADVANCED_STREAMLINE_BUTTON,
    STREAMLINES_COMPUTE_STREAMLINE_BUTTON,
    STREAMLINES_SET_SEED_POINTS_BUTTON,


    //
    //UIPluginBase.h
    //
    UIPLUGINBASE_BEGIN_MENU,
    UIPLUGINBASE_SHOW_RESULT,
    UIPLUGINBASE_PARAVIEW,
    UIPLUGINBASE_SHOW_DESC,
    UIPLUGINBASE_USER_DIALOG,
    UIPLUGINBASE_SHOW_FINANCIAL,
    UIPLUGINBASE_SHOW_ICON_CHOOSER,
    UIPLUGINBASE_ZOOM,
    UIPLUGINBASE_GEOMETRY,
    UIPLUGINBASE_NAVTO,
    UIPLUGINBASE_NAVTO_SELECT,
    UIPLUGINBASE_DATASET,
    UIPLUGINBASE_MODEL_INPUTS,
    UIPLUGINBASE_MODEL_RESULTS,
    UIPLUGINBASE_VISUALIZATION,
    UIPLUGINBASE_SET_UI_PLUGIN_NAME,
    UIPLUGINBASE_SET_ACTIVE_MODEL,
    UIPLUGINBASE_ACTIVE_MODEL_SOUNDS,
    UIPLUGINBASE_DEL_MOD,
    UIPLUGINBASE_ADD_INPUT_PORT,
    UIPLUGINBASE_ADD_OUTPUT_PORT,
    UIPLUGINBASE_DELETE_PORT,
    UIPLUGINBASE_TOGGLE_ALL_ON,
    UIPLUGINBASE_TOGGLE_PLUGIN_ON,
    UIPLUGINBASE_MAKE_HIER,
    UIPLUGINBASE_DIALOG_PLUGIN_UPDATE,
    UIPLUGINBASE_TOGGLE_MENU,
    UIPLUGINBASE_ASPEN_ICON,
    UIPLUGINBASE_SET_ACTIVE_PLUGIN,
    UIPLUGINBASE_XPLORER_MENU,
    UIPLUGINBASE_CONDUCTOR_MENU,
    UIPLUGINBASE_PORT_MENU,
    UIPLUGINBASE_END_MENU,


    //
    //VisTab.h
    //
    VISTAB_CONTOUR_DLG,
    VISTAB_VECTOR_DLG,
    VISTAB_STREAMLINE_DLG,
    VISTAB_ISOSURFACE_DLG,
    VISTAB_TEXTURE_BASED_DLG,
    VISTAB_POLYDATA_DLG,
    VISTAB_CONTOUR_BUTTON,
    VISTAB_VECTOR_BUTTON,
    VISTAB_STREAMLINE_BUTTON,
    VISTAB_ISOSURFACE_BUTTON,
    VISTAB_TEXTURE_BASED_BUTTON,
    VISTAB_POLYDATA_BUTTON,
    VISTAB_CLEAR_ALL_BUTTON,
    VISTAB_MIN_SPINCTRL,
    VISTAB_MAX_SPINCTRL,
    VISTAB_MIN_MAX_SLIDERS,
    VISTAB_MIN_SLIDER,
    VISTAB_MAX_SLIDER,
    VISTAB_CLOSE_BUTTON,
    VISTAB_DATA_WIREFRAME_CB,
    VISTAB_DATA_BBOX_CB,
    VISTAB_DATA_AXES_CB,
    VISTAB_DATA_UPDATE_AXES,
    VISTAB_DATA_SCALAR_BAR,
    VISTAB_DIALOG,
    VISTAB_TOOLBAR,
    VISTAB_TOOL,
    VISTAB_TOOL1,
    VISTAB_TOOL2,
    VISTAB_TOOL3,
    VISTAB_TOOL4,
    VISTAB_TOOL5,
    VISTAB_COMBOBOX,
    VISTAB_LISTBOX,
    VISTAB_LISTBOX1,
    VISTAB_BUTTON,
    VISTAB_COMBOBOX1,

    //Link.h
    LINK_DEL,
    LINK_DEL_CON,
    LINK_SHOW_CONT,
	LINK_SET_NAME,
    LINK_ADD_CON,
    LINK_SET_ACTIVE,
    LINK_MENU,
    LINK_ASPEN_PLUS_SHOW_NAME,
    LINK_ASPEN_PLUS_INPUTS,
    LINK_ASPEN_PLUS_RESULTS,
    LINK_ASPEN_DYN_SHOW_NAME,
    LINK_ASPEN_DYN_ALL_VARS,
    
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
    //UITeacherTab.h
    //
    UITEACHERTAB_RBOX,
    UITEACHERTAB_CLEAR_BUTTON,
    UITEACHERTAB_RECORD_SCENE,
    
    //
    //MinervaDialog.h
    //
    MINERVA_DIALOG_ADD_ELEVATION_LAYER,
    MINERVA_DIALOG_REMOVE_ELEVATION_LAYER,
    MINERVA_DIALOG_ADD_RASTER_LAYER,
    MINERVA_DIALOG_REMOVE_RASTER_LAYER    
};

#endif //CONDUCTORLIBENUMS_H
