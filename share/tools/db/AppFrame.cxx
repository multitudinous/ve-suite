
// --- VE-Suite Includes --- //
#include "AppFrame.h"
#include "MenuBar.h"
#include "ToolBar.h"
#include "ConnectionDialog.h"
#include "DBAppEnums.h"

#include <ves/util/icons/ve_icon32x32.xpm>

// --- wxWidgets Includes --- //
#include <wx/icon.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/panel.h>
#include <wx/notebook.h>
#include <wx/treectrl.h>

// --- C/C++ Libraries --- //


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
        wxSize( 693, 595 ),
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
    SetIcon( ve_icon32x32_xpm );

    //Create the menu bar
    m_menuBar = new MenuBar();
    SetMenuBar( m_menuBar );

    //Create the tool bar
    m_toolBar = new ToolBar( this );
    SetToolBar( m_toolBar );

	wxBoxSizer* mainSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticBoxSizer* connectionsSizer =
        new wxStaticBoxSizer(
            new wxStaticBox( 
                this, wxID_ANY, wxT( "Connections" ) ), wxHORIZONTAL );
	
	m_connectionsTreeCtrl =
        new wxTreeCtrl(
            this, wxID_ANY, wxDefaultPosition, wxDefaultSize,
            wxTR_DEFAULT_STYLE | wxHSCROLL | wxNO_BORDER | wxVSCROLL );
	m_connectionsTreeCtrl->SetBackgroundColour( wxColour( 255, 255, 255 ) );
	
	connectionsSizer->Add( m_connectionsTreeCtrl, 0, wxALL | wxEXPAND, 5 );
	
	mainSizer->Add( connectionsSizer, 0, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* notebookSizer = new wxBoxSizer( wxVERTICAL );
	
	wxNotebook* notebook =
        new wxNotebook( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, 0 );
	notebook->SetBackgroundColour( wxColour( 255, 255, 255 ) );
	
	wxPanel* tempPanel =
        new wxPanel(
            notebook, wxID_ANY, wxDefaultPosition, wxDefaultSize,
            wxTAB_TRAVERSAL );
	tempPanel->SetBackgroundColour( wxColour( 255, 255, 255 ) );
	
	notebook->AddPage( tempPanel, wxT( "Tab 1" ), true );
	
	notebookSizer->Add( notebook, 1, wxEXPAND  |  wxALL, 5 );
	
	mainSizer->Add( notebookSizer, 1, wxALL | wxEXPAND, 5 );
	
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
