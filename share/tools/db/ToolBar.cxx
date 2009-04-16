
// --- VE-Suite Includes --- //
#include "ToolBar.h"
#include "DBAppEnums.h"

#include "xpm/ToolBar/AddConnection.xpm"

BEGIN_EVENT_TABLE( ToolBar, wxToolBar )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ToolBar::ToolBar( wxWindow* parent )
    :
    wxToolBar(
        parent,
        wxID_ANY,
        wxDefaultPosition,
        wxDefaultSize,
        wxNO_BORDER | wxTB_VERTICAL,
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
    mToolbarBitmaps[ "addConnectionBitmap" ] =
        wxBitmap( AddConnection_xpm );
}
////////////////////////////////////////////////////////////////////////////////
void ToolBar::CreateGUI()
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
