
// --- VE-Suite Includes --- //
#include "ConnectionDialog.h"

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/combobox.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/treectrl.h>

// --- C/C++ Libraries --- //

BEGIN_EVENT_TABLE( ConnectionDialog, wxDialog )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ConnectionDialog::ConnectionDialog( wxWindow* parent )
    :
    wxDialog(
        parent,
        wxID_ANY,
        wxT( "Add a Connection" ),
        wxDefaultPosition,
        wxSize( -1, -1 ),
        wxCAPTION | wxCLOSE_BOX | wxMAXIMIZE_BOX |
        wxMINIMIZE_BOX | wxRESIZE_BORDER | wxSTAY_ON_TOP | wxSYSTEM_MENU ),
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
	
	wxBoxSizer* mainSizer = new wxBoxSizer( wxVERTICAL );
	
	wxStaticBoxSizer* addConnectionSizer =
        new wxStaticBoxSizer( new wxStaticBox(
            this, wxID_ANY, wxT( "Add Connection" ) ), wxVERTICAL );
	
	wxBoxSizer* connectionNameSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* connectionNameStaticText =
        new wxStaticText(
            this, wxID_ANY, wxT( "Connection Name" ),
            wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	connectionNameStaticText->Wrap( -1 );
	connectionNameSizer->Add(
        connectionNameStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_connectionNameTextCtrl = new wxTextCtrl(
        this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	connectionNameSizer->Add(
        m_connectionNameTextCtrl, 1, wxALL | wxEXPAND, 5 );
	
	addConnectionSizer->Add( connectionNameSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* connectionTypeSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* connectionTypeStaticText =
        new wxStaticText(
            this, wxID_ANY, wxT( "Connection Type" ),
            wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	connectionTypeStaticText->Wrap( -1 );
	connectionTypeSizer->Add(
        connectionTypeStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_connectionTypeComboBox =
        new wxComboBox(
            this, wxID_ANY, wxT( "MySQL" ),
            wxDefaultPosition, wxDefaultSize, 0, NULL, 0 );
	m_connectionTypeComboBox->Append( wxT( "MySQL" ) );
	connectionTypeSizer->Add(
        m_connectionTypeComboBox, 1, wxALL | wxEXPAND, 5 );
	
	addConnectionSizer->Add( connectionTypeSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* hostSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* hostStaticText =
        new wxStaticText( this, wxID_ANY, wxT( "Host" ),
        wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	hostStaticText->Wrap( -1 );
	hostSizer->Add( hostStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_hostTextCtrl = new wxTextCtrl(
        this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	hostSizer->Add( m_hostTextCtrl, 1, wxALL | wxEXPAND, 5 );
	
	addConnectionSizer->Add( hostSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* portSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* portStaticText =
        new wxStaticText(
            this, wxID_ANY, wxT( "Port" ),
            wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	portStaticText->Wrap( -1 );
	portSizer->Add( portStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_portTextCtrl = new wxTextCtrl(
        this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	portSizer->Add( m_portTextCtrl, 1, wxALL | wxEXPAND, 5 );
	
	addConnectionSizer->Add( portSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* usernameSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* usernameStaticText =
        new wxStaticText(
            this, wxID_ANY, wxT( "Username" ),
            wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	usernameStaticText->Wrap( -1 );
	usernameSizer->Add( usernameStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_usernameTextCtrl = new wxTextCtrl(
        this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	usernameSizer->Add( m_usernameTextCtrl, 1, wxALL | wxEXPAND, 5 );
	
	addConnectionSizer->Add( usernameSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* passwordSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* passwordStaticText =
        new wxStaticText(
            this, wxID_ANY, wxT( "Password" ),
            wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	passwordStaticText->Wrap( -1 );
	passwordSizer->Add( passwordStaticText, 1, wxALL | wxEXPAND, 5 );
	
	m_passwordTextCtrl = new wxTextCtrl(
        this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
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
