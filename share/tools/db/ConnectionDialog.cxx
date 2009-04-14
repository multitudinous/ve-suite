
// --- VE-Suite Includes --- //
#include "ConnectionDialog.h"

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/combobox.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/button.h>

// --- C/C++ Libraries --- //

BEGIN_EVENT_TABLE( ConnectionDialog, wxDialog )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ConnectionDialog::ConnectionDialog()
    :
    wxDialog(),
    m_connectionNameTextCtrl( NULL ),
    m_connectionTypeComboBox( NULL ),
    m_hostTextCtrl( NULL ),
    m_portTextCtrl( NULL ),
    m_usernameTextCtrl( NULL ),
    m_passwordTextCtrl( NULL )
{
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
ConnectionDialog::ConnectionDialog(
    wxWindow* parent,
    wxWindowID id,
    const wxString& title,
    const wxPoint& pos,
    const wxSize& size,
    long style )
    :
    wxDialog( parent, id, title, pos, size, style ),
    m_connectionNameTextCtrl( NULL ),
    m_connectionTypeComboBox( NULL ),
    m_hostTextCtrl( NULL ),
    m_portTextCtrl( NULL ),
    m_usernameTextCtrl( NULL ),
    m_passwordTextCtrl( NULL )
{
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
ConnectionDialog::~ConnectionDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ConnectionDialog::CreateGUI()
{
	SetSizeHints( wxDefaultSize, wxDefaultSize );
	SetBackgroundColour( wxColour( 255, 255, 255 ) );
	
	wxBoxSizer* mainSizer;
	mainSizer = new wxBoxSizer( wxVERTICAL );
	
	wxStaticBoxSizer* addConnectionSizer;
	addConnectionSizer = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT( "Add Connection" ) ), wxVERTICAL );
	
	wxBoxSizer* connectionNameSizer;
	connectionNameSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* connectionNameStaticText;
	connectionNameStaticText = new wxStaticText( this, wxID_ANY, wxT( "Connection Name" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	connectionNameStaticText->Wrap( -1 );
	connectionNameSizer->Add( connectionNameStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_connectionNameTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	connectionNameSizer->Add( m_connectionNameTextCtrl, 1, wxALL | wxEXPAND, 5 );
	
	addConnectionSizer->Add( connectionNameSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* connectionTypeSizer;
	connectionTypeSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* connectionTypeStaticText;
	connectionTypeStaticText = new wxStaticText( this, wxID_ANY, wxT( "Connection Type" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	connectionTypeStaticText->Wrap( -1 );
	connectionTypeSizer->Add( connectionTypeStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_connectionTypeComboBox = new wxComboBox( this, wxID_ANY, wxT( "MySQL" ), wxDefaultPosition, wxDefaultSize, 0, NULL, 0 );
	m_connectionTypeComboBox->Append( wxT( "MySQL" ) );
	connectionTypeSizer->Add( m_connectionTypeComboBox, 1, wxALL | wxEXPAND, 5 );
	
	addConnectionSizer->Add( connectionTypeSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* hostSizer;
	hostSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* hostStaticText;
	hostStaticText = new wxStaticText( this, wxID_ANY, wxT( "Host" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	hostStaticText->Wrap( -1 );
	hostSizer->Add( hostStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_hostTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	hostSizer->Add( m_hostTextCtrl, 1, wxALL | wxEXPAND, 5 );
	
	addConnectionSizer->Add( hostSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* portSizer;
	portSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* portStaticText;
	portStaticText = new wxStaticText( this, wxID_ANY, wxT( "Port" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	portStaticText->Wrap( -1 );
	portSizer->Add( portStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_portTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	portSizer->Add( m_portTextCtrl, 1, wxALL | wxEXPAND, 5 );
	
	addConnectionSizer->Add( portSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* usernameSizer;
	usernameSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* usernameStaticText;
	usernameStaticText = new wxStaticText( this, wxID_ANY, wxT( "Username" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	usernameStaticText->Wrap( -1 );
	usernameSizer->Add( usernameStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_usernameTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	usernameSizer->Add( m_usernameTextCtrl, 1, wxALL | wxEXPAND, 5 );
	
	addConnectionSizer->Add( usernameSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* passwordSizer;
	passwordSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* passwordStaticText;
	passwordStaticText = new wxStaticText( this, wxID_ANY, wxT( "Password" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	passwordStaticText->Wrap( -1 );
	passwordSizer->Add( passwordStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_passwordTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	passwordSizer->Add( m_passwordTextCtrl, 1, wxALL | wxEXPAND, 5 );
	
	addConnectionSizer->Add( passwordSizer, 1, wxALL | wxEXPAND, 5 );
	
	mainSizer->Add( addConnectionSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxStdDialogButtonSizer* stdDialogButtonSizer = new wxStdDialogButtonSizer();
	wxButton* OK = new wxButton( this, wxID_OK );
	wxButton* Cancel = new wxButton( this, wxID_CANCEL );
	wxButton* Help = new wxButton( this, wxID_HELP );
	stdDialogButtonSizer->AddButton( OK );
	stdDialogButtonSizer->AddButton( Cancel );
	stdDialogButtonSizer->AddButton( Help );
	stdDialogButtonSizer->Realize();
	mainSizer->Add( stdDialogButtonSizer, 0, wxALL | wxEXPAND, 5 );
	
	SetSizer( mainSizer );
	Layout();
	mainSizer->Fit( this );
}
////////////////////////////////////////////////////////////////////////////////
