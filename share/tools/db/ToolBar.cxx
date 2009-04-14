
// --- VE-Suite Includes --- //
//Don't move AppFrame.h below ToolBar.h
#include "AppFrame.h"
#include "ToolBar.h"
//#include "ConductorAppEnums.h"

//#include <ves/conductor/xpm/ToolBar/NewDocumentButton.xpm>

//#include <ves/conductor/util/CORBAServiceList.h>

//#include <ves/open/xml/Command.h>
//#include <ves/open/xml/DataValuePair.h>

BEGIN_EVENT_TABLE( ToolBar, wxToolBar )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ToolBar::ToolBar()
    :
    wxToolBar()
{
    LoadToolBarBitmaps();
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
ToolBar::ToolBar(
    wxWindow* parent,
    wxWindowID id,
    const wxPoint& pos,
    const wxSize& size,
    long style,
    const wxString& name )
    :
    wxToolBar( parent, id, pos, size, style, name )
{
    LoadToolBarBitmaps();
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
ToolBar::~ToolBar()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ToolBar::LoadToolBarBitmaps()
{
    //mToolbarBitmaps[ std::string( "newBitmap" )] =
        //wxBitmap( NewDocumentButton_xpm );
}
////////////////////////////////////////////////////////////////////////////////
void ToolBar::CreateGUI()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetToolBitmapSize( wxSize( 32, 32 ) );
    SetToolSeparation( 10 );

    /*
    AddTool(
        MAINTOOLBAR_NEW, wxT( "" ),
        mToolbarBitmaps[ "newBitmap" ],
        wxT( "New" ), wxITEM_NORMAL );
    AddTool(
        MAINTOOLBAR_OPEN, wxT( "" ),
        mToolbarBitmaps[ "openBitmap" ],
        wxT( "Open" ), wxITEM_NORMAL );
    AddTool(
        MAINTOOLBAR_SAVE, wxT( "" ),
        mToolbarBitmaps[ "saveBitmap" ],
        wxT( "Save" ), wxITEM_NORMAL );
    AddSeparator();

    AddTool(
        MAINTOOLBAR_SELECTION, wxT( "" ),
        mToolbarBitmaps[ "cursorBitmap" ],
        wxT( "Selection" ), wxITEM_RADIO );
    AddTool(
        MAINTOOLBAR_WORLD_NAVIGATION, wxT( "" ),
        mToolbarBitmaps[ "worldNavigationSelectBitmap" ],
        wxT( "World Navigation" ), wxITEM_RADIO );
    AddTool(
        MAINTOOLBAR_OBJECT_NAVIGATION, wxT( "" ),
        mToolbarBitmaps[ "objectNavigationBitmap" ],
        wxT( "Object Navigation" ), wxITEM_RADIO );
    AddTool(
        MAINTOOLBAR_UNSELECT, wxT( "" ),
        mToolbarBitmaps[ "unselectBitmap" ],
        wxT( "Unselect Objects" ), wxITEM_NORMAL );
    AddSeparator();

    AddTool(
        MAINTOOLBAR_SMALL_CENTER_POINT_JUMP, wxT( "" ),
        mToolbarBitmaps[ "smallCenterPointSelectBitmap" ],
        wxT( "Small Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        MAINTOOLBAR_MEDIUM_CENTER_POINT_JUMP, wxT( "" ),
        mToolbarBitmaps[ "mediumCenterPointBitmap" ],
        wxT( "Medium Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        MAINTOOLBAR_LARGE_CENTER_POINT_JUMP, wxT( "" ),
        mToolbarBitmaps[ "largeCenterPointBitmap" ],
        wxT( "Large Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        MAINTOOLBAR_BB_CENTER_POINT_JUMP, wxT( "" ),
        mToolbarBitmaps[ "bbCenterPointBitmap" ],
        wxT( "Bounding Box Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        MAINTOOLBAR_RESET_CENTER_POINT, wxT( "" ),
        mToolbarBitmaps[ "resetCenterPointBitmap" ],
        wxT( "Reset Centerpoint" ), wxITEM_NORMAL );
    AddSeparator();

    AddTool(
        MAINTOOLBAR_PHYSICS, wxT( "" ),
        mToolbarBitmaps[ "physicsBitmap" ],
        wxT( "Physics On/Off" ), wxITEM_CHECK );
#ifdef WIN32
#ifdef CHARACTER_CONTROLLER
    AddTool(
        MAINTOOLBAR_CHARACTER, wxT( "" ),
        mToolbarBitmaps[ "characterBitmap" ],
        mToolbarBitmaps[ "characterDisabledBitmap" ], wxITEM_CHECK,
        wxT( "Character Controller" ) );
#endif //CHARACTER_CONTROLLER
    AddTool(
        MAINTOOLBAR_RESET, wxT( "" ),
        mToolbarBitmaps[ "resetBitmap" ],
        mToolbarBitmaps[ "resetDisabledBitmap" ], wxITEM_NORMAL,
        wxT( "Reset Simulation" ) );
    AddTool(
        MAINTOOLBAR_PAUSE, wxT( "" ),
        mToolbarBitmaps[ "pauseBitmap" ],
        mToolbarBitmaps[ "pauseDisabledBitmap" ], wxITEM_CHECK,
        wxT( "Pause Simulation" ) );
    AddTool(
        MAINTOOLBAR_PLAY, wxT( "" ),
        mToolbarBitmaps[ "playBitmap" ],
        mToolbarBitmaps[ "playDisabledBitmap" ], wxITEM_CHECK,
        wxT( "Start Simulation" ) );
    AddTool(
        MAINTOOLBAR_STEP, wxT( "" ),
        mToolbarBitmaps[ "stepBitmap" ],
        mToolbarBitmaps[ "stepDisabledBitmap" ], wxITEM_NORMAL,
        wxT( "Step Simulation" ) );
#else
#ifdef CHARACTER_CONTROLLER
    AddTool(
        MAINTOOLBAR_CHARACTER, wxT( "" ),
        mToolbarBitmaps[ "characterBitmap" ],
        wxT( "Character Controller" ), wxITEM_CHECK );
#endif //CHARACTER_CONTROLLER
    AddTool(
        MAINTOOLBAR_RESET, wxT( "" ),
        mToolbarBitmaps[ "resetBitmap" ],
        wxT( "Reset Simulation" ), wxITEM_NORMAL );
    AddTool(
        MAINTOOLBAR_PAUSE, wxT( "" ),
        mToolbarBitmaps[ "pauseBitmap" ],
        wxT( "Pause Simulation" ), wxITEM_CHECK );
    AddTool(
        MAINTOOLBAR_PLAY, wxT( "" ),
        mToolbarBitmaps[ "playBitmap" ],
        wxT( "Start Simulation" ), wxITEM_CHECK );
    AddTool(
        MAINTOOLBAR_STEP, wxT( "" ),
        mToolbarBitmaps[ "stepBitmap" ],
        wxT( "Step Simulation" ), wxITEM_NORMAL );
#endif //WIN32
    AddSeparator();

    AddTool(
        MAINTOOLBAR_SUMMIT_JOB, wxT( "" ),
        mToolbarBitmaps[ "sendJobBitmap" ],
        wxT( "Submit Job" ), wxITEM_NORMAL );

    Realize();

    ToggleTool( MAINTOOLBAR_WORLD_NAVIGATION, true );
    ToggleTool( MAINTOOLBAR_SMALL_CENTER_POINT_JUMP, true );
#ifdef WIN32
    SetToolNormalBitmap(
        MAINTOOLBAR_PAUSE, mToolbarBitmaps[ "pauseDisabledBitmap" ] );
#endif //WIN32

    EnableTool( MAINTOOLBAR_CHARACTER, false );
    EnableTool( MAINTOOLBAR_RESET, false );
    EnableTool( MAINTOOLBAR_PAUSE, false );
    EnableTool( MAINTOOLBAR_PLAY, false );
    EnableTool( MAINTOOLBAR_STEP, false );
    */
}
////////////////////////////////////////////////////////////////////////////////
