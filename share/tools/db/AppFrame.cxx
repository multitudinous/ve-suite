
// --- VE-Suite Includes --- //
#include "AppFrame.h"
#include "AppMenuBar.h"
#include "AppToolBar.h"
#include "AppTreeCtrl.h"
#include "AppNotebook.h"
#include "DBConnectionDialog.h"
#include "VESConnectionDialog.h"
#include "CorbaUnitManager.h"
#include "DBAppEnums.h"

#include <ves/util/icons/ve_icon16x16.xpm>

// --- wxWidgets Includes --- //
#include <wx/icon.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/scrolwin.h>

BEGIN_EVENT_TABLE( AppFrame, wxFrame )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppFrame::AppFrame( wxWindow* parent, wxWindowID id )
    :
    wxFrame(
        parent,
        id,
        wxT( "VE-DB" ),
        wxDefaultPosition,
        wxSize( 800, 600 ),
        wxDEFAULT_FRAME_STYLE ),
    m_appMenuBar( NULL ),
    m_appToolBar( NULL ),
    m_appTreeCtrl( NULL ),
    m_appNotebook( NULL ),
    m_dbConnectionDialog( new DBConnectionDialog( this ) ),
    m_vesConnectionDialog( new VESConnectionDialog( this ) ),
    m_corbaUnitManager( new CorbaUnitManager() )
{
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
AppFrame::~AppFrame()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::CreateGUI()
{
    SetSizeHints( wxDefaultSize, wxDefaultSize );
	SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetIcon( ve_icon16x16_xpm );

    //Create the main sizer
    wxBoxSizer* mainSizer = new wxBoxSizer( wxHORIZONTAL );

    //Create the menu bar
    m_appMenuBar = new AppMenuBar( this );
    SetMenuBar( m_appMenuBar );

    //Create the tool bar
    m_appToolBar = new AppToolBar( this );
    SetToolBar( m_appToolBar );

    //Create a scrolled window
    wxScrolledWindow* scrolledWindow;
	scrolledWindow =
        new wxScrolledWindow(
            this, wxID_ANY,
            wxDefaultPosition, wxDefaultSize,
            wxALWAYS_SHOW_SB | wxHSCROLL | wxRAISED_BORDER | wxVSCROLL );
	scrolledWindow->SetScrollRate( 5, 5 );
    mainSizer->Add( scrolledWindow, 1, wxEXPAND | wxTOP, 5 );

    //Create the tree control sizer
    wxBoxSizer* treeCtrlSizer = new wxBoxSizer( wxVERTICAL );
    scrolledWindow->SetSizer( treeCtrlSizer );
	scrolledWindow->Layout();
	treeCtrlSizer->Fit( scrolledWindow );
	
    //Create the tree control
	m_appTreeCtrl = new AppTreeCtrl( scrolledWindow );
	treeCtrlSizer->Add( m_appTreeCtrl, 1, wxALL | wxEXPAND, 10 );
	
    //Create the notebook
	m_appNotebook = new AppNotebook( this );
	mainSizer->Add( m_appNotebook, 3, wxEXPAND | wxTOP, 5 );
	
	SetSizer( mainSizer );
	Layout();
}
////////////////////////////////////////////////////////////////////////////////
AppMenuBar* const AppFrame::GetAppMenuBar() const
{
    return m_appMenuBar;
}
////////////////////////////////////////////////////////////////////////////////
AppToolBar* const AppFrame::GetAppToolBar() const
{
    return m_appToolBar;
}
////////////////////////////////////////////////////////////////////////////////
AppTreeCtrl* const AppFrame::GetAppTreeCtrl() const
{
    return m_appTreeCtrl;
}
////////////////////////////////////////////////////////////////////////////////
AppNotebook const AppFrame::GetAppNotebook() const
{
    return m_appNotebook;
}
////////////////////////////////////////////////////////////////////////////////
DBConnectionDialog* const AppFrame::GetDBConnectionDialog() const
{
    return m_dbConnectionDialog;
}
////////////////////////////////////////////////////////////////////////////////
VESConnectionDialog* const AppFrame::GetVESConnectionDialog() const
{
    return m_vesConnectionDialog;
}
////////////////////////////////////////////////////////////////////////////////
CorbaUnitManager* const AppFrame::GetCorbaUnitManager() const
{
    return m_corbaUnitManager;
}
////////////////////////////////////////////////////////////////////////////////
