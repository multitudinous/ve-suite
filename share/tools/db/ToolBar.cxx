
// --- VE-Suite Includes --- //
//Don't move AppFrame.h below ToolBar.h
#include "AppFrame.h"
#include "ToolBar.h"
#include "DBAppEnums.h"

#include <ves/conductor/xpm/ToolBar/NewDocumentButton.xpm>

//#include <ves/conductor/util/CORBAServiceList.h>

//#include <ves/open/xml/Command.h>
//#include <ves/open/xml/DataValuePair.h>

BEGIN_EVENT_TABLE( ToolBar, wxToolBar )
EVT_MENU( TOOLBAR_OPEN_CONNECTION_DIALOG, AppFrame::OpenConnectionDialog )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ToolBar::ToolBar( wxWindow* parent )
    :
    wxToolBar(
        parent,
        wxID_ANY,
        wxDefaultPosition,
        wxDefaultSize,
        wxNO_BORDER | wxTB_HORIZONTAL,
        wxT( "ToolBar" ) )
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
    mToolbarBitmaps[ std::string( "newBitmap" )] =
        wxBitmap( NewDocumentButton_xpm );
}
////////////////////////////////////////////////////////////////////////////////
void ToolBar::CreateGUI()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetToolBitmapSize( wxSize( 32, 32 ) );
    SetToolSeparation( 10 );

    AddTool(
        TOOLBAR_OPEN_CONNECTION_DIALOG, wxT( "" ),
        mToolbarBitmaps[ "newBitmap" ],
        wxT( "Add Connection" ), wxITEM_NORMAL );

	Realize();

    /*
    EnableTool( MAINTOOLBAR_CHARACTER, false );
    EnableTool( MAINTOOLBAR_RESET, false );
    EnableTool( MAINTOOLBAR_PAUSE, false );
    EnableTool( MAINTOOLBAR_PLAY, false );
    EnableTool( MAINTOOLBAR_STEP, false );
    */
}
////////////////////////////////////////////////////////////////////////////////
