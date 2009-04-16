
// --- VE-Suite Includes --- //
#include "AppFrame.h"
#include "MenuBar.h"
#include "ToolBar.h"
#include "Notebook.h"
#include "ConnectionDialog.h"
#include "DBAppEnums.h"

#include <ves/util/icons/ve_icon16x16.xpm>

// --- wxWidgets Includes --- //
#include <wx/icon.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/treectrl.h>

// --- C/C++ Libraries --- //


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
    m_menuBar( NULL ),
    m_toolBar( NULL ),
    m_connectionDialog( NULL ),
    m_connectionsTreeCtrl( NULL )
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

    //Create the menu bar
    m_menuBar = new MenuBar();
    SetMenuBar( m_menuBar );

    //Create the tool bar
    m_toolBar = new ToolBar( this );
    SetToolBar( m_toolBar );

	wxBoxSizer* mainSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticBoxSizer* connectionsSizer =
        new wxStaticBoxSizer( new wxStaticBox(
            this, wxID_ANY, wxT( "Database Connections" ) ), wxHORIZONTAL );
    mainSizer->Add( connectionsSizer, 1, wxEXPAND | wxLEFT | wxRIGHT, 5 );
	
	m_connectionsTreeCtrl =
        new wxTreeCtrl(
            this, wxID_ANY,
            wxDefaultPosition, wxDefaultSize,
            wxTR_DEFAULT_STYLE | wxHSCROLL | wxRAISED_BORDER | wxVSCROLL );
	m_connectionsTreeCtrl->SetBackgroundColour( wxColour( 255, 255, 255 ) );
	connectionsSizer->Add( m_connectionsTreeCtrl, 1, wxEXPAND | wxTOP, 5 );
	
    //Create the notebook
	m_notebook = new Notebook( this );
	mainSizer->Add( m_notebook, 3, wxEXPAND | wxLEFT, 5 );
	
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
