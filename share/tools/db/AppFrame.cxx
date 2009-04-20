
// --- VE-Suite Includes --- //
#include "AppFrame.h"
#include "AppMenuBar.h"
#include "AppToolBar.h"
#include "AppTreeCtrl.h"
#include "AppNotebook.h"
#include "ConnectionDialog.h"
#include "DBAppEnums.h"

#include <ves/util/icons/ve_icon16x16.xpm>

// --- wxWidgets Includes --- //
#include <wx/icon.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/scrolwin.h>

BEGIN_EVENT_TABLE( AppFrame, wxFrame )
EVT_MENU( TOOLBAR_OPEN_CONNECTION_DIALOG, AppFrame::OpenConnectionDialog )
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
    m_connectionDialog( NULL )
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
    m_appMenuBar = new AppMenuBar();
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
void AppFrame::OpenConnectionDialog( wxCommandEvent& WXUNUSED( event ) )
{
    if( m_connectionDialog == NULL )
    {
        m_connectionDialog = new ConnectionDialog( this );
    }

    m_connectionDialog->Show();
}
////////////////////////////////////////////////////////////////////////////////
