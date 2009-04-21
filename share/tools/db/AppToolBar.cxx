
// --- VE-Suite Includes --- //
#include "AppToolBar.h"
#include "AppFrame.h"
#include "DBConnectionDialog.h"
#include "VESConnectionDialog.h"
#include "DBAppEnums.h"

#include "xpm/ToolBar/DBConnection.xpm"
#include "xpm/ToolBar/VESConnection.xpm"

BEGIN_EVENT_TABLE( AppToolBar, wxToolBar )
EVT_MENU( SHOW_DB_CONNECTION_DIALOG_ATB, AppToolBar::ShowDBConnectionDialog )
EVT_MENU( SHOW_VES_CONNECTION_DIALOG_ATB, AppToolBar::ShowVESConnectionDialog )
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
        wxT( "ToolBar" ) ),
    m_appFrame( static_cast< AppFrame* >( parent ) )
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
    mToolbarBitmaps[ "dbConnectionBitmap" ] =
        wxBitmap( DBConnection_xpm );

    mToolbarBitmaps[ "vesConnectionBitmap" ] =
        wxBitmap( VESConnection_xpm );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::CreateGUI()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetToolBitmapSize( wxSize( 32, 32 ) );
    SetToolSeparation( 10 );

    AddTool(
        SHOW_DB_CONNECTION_DIALOG_ATB, wxT( "" ),
        mToolbarBitmaps[ "dbConnectionBitmap" ],
        wxT( "DB Connection" ), wxITEM_NORMAL );

    AddSeparator();

    AddTool(
        SHOW_VES_CONNECTION_DIALOG_ATB, wxT( "" ),
        mToolbarBitmaps[ "vesConnectionBitmap" ],
        wxT( "VES Connection" ), wxITEM_NORMAL );

	Realize();

    //EnableTool( DB_CONNECTION_TOOLBAR, false );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::ShowDBConnectionDialog( wxCommandEvent& WXUNUSED( event ) )
{
    m_appFrame->GetDBConnectionDialog()->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::ShowVESConnectionDialog( wxCommandEvent& WXUNUSED( event ) )
{
    m_appFrame->GetVESConnectionDialog()->Show();
}
////////////////////////////////////////////////////////////////////////////////
