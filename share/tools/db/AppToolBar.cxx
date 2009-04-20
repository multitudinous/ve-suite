
// --- VE-Suite Includes --- //
#include "AppToolBar.h"
#include "DBAppEnums.h"

#include "xpm/ToolBar/AddConnection.xpm"

BEGIN_EVENT_TABLE( AppToolBar, wxToolBar )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppToolBar::AppToolBar( wxWindow* parent )
    :
    wxToolBar(
        parent,
        wxID_ANY,
        wxDefaultPosition,
        wxDefaultSize,
        wxTB_FLAT | wxTB_NODIVIDER | wxTB_HORIZONTAL | wxNO_BORDER,
        wxT( "ToolBar" ) )
{
    LoadBitmaps();
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
AppToolBar::~AppToolBar()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::LoadBitmaps()
{
    mToolbarBitmaps[ "addConnectionBitmap" ] =
        wxBitmap( AddConnection_xpm );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::CreateGUI()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetToolBitmapSize( wxSize( 32, 32 ) );
    SetToolSeparation( 10 );

    AddTool(
        TOOLBAR_OPEN_CONNECTION_DIALOG, wxT( "" ),
        mToolbarBitmaps[ "addConnectionBitmap" ],
        wxT( "Add Connection" ), wxITEM_NORMAL );

	Realize();

    //EnableTool( MAINTOOLBAR_CHARACTER, false );
}
////////////////////////////////////////////////////////////////////////////////
