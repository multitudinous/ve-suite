
// --- VE-Suite Includes --- //
#include "DBConnectionDialog.h"
#include "AppFrame.h"
#include "AppTreeCtrl.h"
#include "DBAppEnums.h"
#include "MySQLConnection.h"

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/combobox.h>
#include <wx/choice.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/msgdlg.h>

// --- C/C++ Includes --- //
#include <string>
#include <iostream>
#include <iomanip>

BEGIN_EVENT_TABLE( DBConnectionDialog, wxDialog )
EVT_BUTTON( CONNECT_DBCD, DBConnectionDialog::Connect )
EVT_BUTTON( CLEAR_DBCD, DBConnectionDialog::Clear )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
DBConnectionDialog::DBConnectionDialog( wxWindow* parent )
    :
    wxDialog(
        parent,
        wxID_ANY,
        wxT( "DB Connection" ),
        wxDefaultPosition,
        wxSize( 500, 300 ),
        wxCAPTION | wxCLOSE_BOX |
        wxMINIMIZE_BOX | wxSTAY_ON_TOP | wxSYSTEM_MENU ),
    m_appFrame( static_cast< AppFrame* >( parent ) ),
    m_storedConnectionComboBox( NULL ),
    m_connectionTypeChoice( NULL ),
    m_serverHostTextCtrl( NULL ),
    m_portTextCtrl( NULL ),
    m_usernameTextCtrl( NULL ),
    m_passwordTextCtrl( NULL ),
    m_defaultSchemaTextCtrl( NULL )
{
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
DBConnectionDialog::~DBConnectionDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DBConnectionDialog::CreateGUI()
{
	SetSizeHints( wxDefaultSize, wxDefaultSize );
	SetBackgroundColour( wxColour( 255, 255, 255 ) );
	
	wxBoxSizer* mainSizer;
	mainSizer = new wxBoxSizer( wxVERTICAL );
	
	wxStaticBoxSizer* addConnectionSizer;
	addConnectionSizer = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT( "Connect to Server Instance" ) ), wxVERTICAL );
	
	wxBoxSizer* storedConnectionSizer;
	storedConnectionSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* storedConnectionStaticText;
	storedConnectionStaticText = new wxStaticText( this, wxID_ANY, wxT( "Stored Connection:" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	storedConnectionStaticText->Wrap( -1 );
	storedConnectionSizer->Add( storedConnectionStaticText, 2, wxALIGN_CENTER | wxALL, 5 );
	
	m_storedConnectionComboBox = new wxComboBox( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, NULL, 0 ); 
	storedConnectionSizer->Add( m_storedConnectionComboBox, 4, wxALIGN_CENTER | wxALL, 5 );
	
	
	storedConnectionSizer->Add( 0, 0, 1, wxEXPAND, 5 );
	
	addConnectionSizer->Add( storedConnectionSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* connectionTypeSizer;
	connectionTypeSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* connectionTypeStaticText;
	connectionTypeStaticText = new wxStaticText( this, wxID_ANY, wxT( "Connection Type:" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	connectionTypeStaticText->Wrap( -1 );
	connectionTypeSizer->Add( connectionTypeStaticText, 2, wxALIGN_CENTER | wxALL, 5 );
	
	wxString m_connectionTypeChoiceChoices[] = { wxT( "MySQL" ), wxT( "MS Access" ) };
	int m_connectionTypeChoiceNChoices = sizeof( m_connectionTypeChoiceChoices ) / sizeof( wxString );
	m_connectionTypeChoice = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_connectionTypeChoiceNChoices, m_connectionTypeChoiceChoices, 0 );
	m_connectionTypeChoice->SetSelection( 0 );
	connectionTypeSizer->Add( m_connectionTypeChoice, 2, wxALIGN_CENTER | wxALL, 5 );
	
	
	connectionTypeSizer->Add( 0, 0, 3, wxEXPAND, 5 );
	
	addConnectionSizer->Add( connectionTypeSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* serverHostSizer;
	serverHostSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* serverHostStaticText;
	serverHostStaticText = new wxStaticText( this, wxID_ANY, wxT( "Server Host:" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	serverHostStaticText->Wrap( -1 );
	serverHostSizer->Add( serverHostStaticText, 2, wxALIGN_CENTER | wxALL, 5 );
	
	m_serverHostTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	serverHostSizer->Add( m_serverHostTextCtrl, 2, wxALIGN_CENTER | wxALL, 5 );
	
	wxStaticText* portStaticText;
	portStaticText = new wxStaticText( this, wxID_ANY, wxT( "Port:" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	portStaticText->Wrap( -1 );
	serverHostSizer->Add( portStaticText, 1, wxALIGN_CENTER | wxALL, 5 );
	
	m_portTextCtrl = new wxTextCtrl( this, wxID_ANY, wxT( "3306" ), wxDefaultPosition, wxDefaultSize, 0 );
	serverHostSizer->Add( m_portTextCtrl, 1, wxALIGN_CENTER | wxALL, 5 );
	
	
	serverHostSizer->Add( 0, 0, 1, wxEXPAND, 5 );
	
	addConnectionSizer->Add( serverHostSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* usernameSizer;
	usernameSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* usernameStaticText;
	usernameStaticText = new wxStaticText( this, wxID_ANY, wxT( "Username:" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	usernameStaticText->Wrap( -1 );
	usernameSizer->Add( usernameStaticText, 2, wxALIGN_CENTER | wxALL, 5 );
	
	m_usernameTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	usernameSizer->Add( m_usernameTextCtrl, 2, wxALIGN_CENTER | wxALL, 5 );
	
	
	usernameSizer->Add( 0, 0, 3, wxEXPAND, 5 );
	
	addConnectionSizer->Add( usernameSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* passwordSizer;
	passwordSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* passwordStaticText;
	passwordStaticText = new wxStaticText( this, wxID_ANY, wxT( "Password:" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	passwordStaticText->Wrap( -1 );
	passwordSizer->Add( passwordStaticText, 2, wxALIGN_CENTER | wxALL, 5 );
	
	m_passwordTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	passwordSizer->Add( m_passwordTextCtrl, 2, wxALIGN_CENTER | wxALL, 5 );
	
	
	passwordSizer->Add( 0, 0, 3, wxEXPAND, 5 );
	
	addConnectionSizer->Add( passwordSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* connectionNameSizer;
	connectionNameSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* defaultSchemaStaticText;
	defaultSchemaStaticText = new wxStaticText( this, wxID_ANY, wxT( "Default Schema:" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	defaultSchemaStaticText->Wrap( -1 );
	connectionNameSizer->Add( defaultSchemaStaticText, 2, wxALIGN_CENTER | wxALL, 5 );
	
	m_defaultSchemaTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	connectionNameSizer->Add( m_defaultSchemaTextCtrl, 2, wxALIGN_CENTER | wxALL, 5 );
	
	
	connectionNameSizer->Add( 0, 0, 3, wxEXPAND, 5 );
	
	addConnectionSizer->Add( connectionNameSizer, 1, wxALL | wxEXPAND, 5 );
	
	mainSizer->Add( addConnectionSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* buttonSizer;
	buttonSizer = new wxBoxSizer( wxHORIZONTAL );
	
	
	buttonSizer->Add( 0, 0, 1, wxEXPAND, 5 );
	
	wxButton* connectButton;
	connectButton = new wxButton( this, CONNECT_DBCD, wxT( "Connect" ), wxDefaultPosition, wxDefaultSize, 0 );
	buttonSizer->Add( connectButton, 0, wxALL, 5 );
	
	wxButton* clearButton;
	clearButton = new wxButton( this, CLEAR_DBCD, wxT( "Clear" ), wxDefaultPosition, wxDefaultSize, 0 );
	buttonSizer->Add( clearButton, 0, wxALL, 5 );
	
	wxButton* cancelButton;
	cancelButton = new wxButton( this, wxID_CANCEL );
	buttonSizer->Add( cancelButton, 0, wxALL, 5 );
	
	mainSizer->Add( buttonSizer, 0, wxEXPAND, 5 );
	
	SetSizer( mainSizer );
	Layout();
}
////////////////////////////////////////////////////////////////////////////////
void DBConnectionDialog::Connect( wxCommandEvent& WXUNUSED( event ) )
{
	//Get database access parameters from wx
    std::string db = m_defaultSchemaTextCtrl->GetValue().mb_str();
    std::string server = m_serverHostTextCtrl->GetValue().mb_str();
    std::string username = m_usernameTextCtrl->GetValue().mb_str();
    std::string password = m_passwordTextCtrl->GetValue().mb_str();
    unsigned int port =
        static_cast< unsigned int >( wxAtoi( m_portTextCtrl->GetValue() ) );

    switch( m_connectionTypeChoice->GetSelection() )
    {
        //MySQL
        case 0:
        {
            if( db != "" && server != "" && username != "" &&
                password != "" && port != atoi( "" ) )
            {
	            //Connect to the database
                MySQLConnection* mysqlConnection =
                    new MySQLConnection( db, server, username, password, port );
	            if( mysqlConnection->Connected() )
                {
                    Hide();

                    m_appFrame->GetAppTreeCtrl()->AddDBConnection(
                        mysqlConnection );

                    wxMessageDialog msgDlg(
                        this,
                        wxT( "MySQL connection successful!" ),
                        wxT( "Success" ) );
                    msgDlg.ShowModal();
	            }
	            else
                {
                    wxMessageDialog wrnDlg(
                        this,
                        wxT( "Unable to connect to MySQL server!" ),
                        wxT( "Warning" ),
                        wxOK | wxCENTRE | wxICON_EXCLAMATION );
                    wrnDlg.ShowModal();

                    delete mysqlConnection;
                    mysqlConnection = NULL;
	            }
            }
            else
            {
                wxMessageDialog errorDlg(
                    this,
                    wxT( "Left required field empty!" ),
                    wxT( "Error" ),
                    wxOK | wxCENTRE | wxICON_ERROR );
                errorDlg.ShowModal();
            }

            break;
        }
        //MS Access
        case 1:
        {
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void DBConnectionDialog::Clear( wxCommandEvent& WXUNUSED( event ) )
{
    m_serverHostTextCtrl->SetValue( wxT( "" ) );
    m_portTextCtrl->SetValue( wxT( "" ) );
    m_usernameTextCtrl->SetValue( wxT( "" ) );
    m_passwordTextCtrl->SetValue( wxT( "" ) );
    m_defaultSchemaTextCtrl->SetValue( wxT( "" ) );
}
////////////////////////////////////////////////////////////////////////////////
