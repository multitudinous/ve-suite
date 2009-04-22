
// --- VE-Suite Includes --- //
#include "VESConnectionDialog.h"
#include "AppFrame.h"
#include "AppToolBar.h"
#include "DBAppEnums.h"
#include "CorbaUnitManager.h"

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/combobox.h>
#include <wx/choice.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/filepicker.h>
#include <wx/msgdlg.h>

// --- C/C++ Libraries --- //
#include <iostream>
#include <iomanip>

BEGIN_EVENT_TABLE( VESConnectionDialog, wxDialog )
EVT_BUTTON( CONNECT_VESCD, VESConnectionDialog::Connect )
EVT_BUTTON( CLEAR_VESCD, VESConnectionDialog::Clear )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
VESConnectionDialog::VESConnectionDialog( wxWindow* parent )
    :
    wxDialog(
        parent,
        wxID_ANY,
        wxT( "VES Connection" ),
        wxDefaultPosition,
        wxSize( 500, 200 ),
        wxCAPTION | wxCLOSE_BOX |
        wxMINIMIZE_BOX | wxSTAY_ON_TOP | wxSYSTEM_MENU ),
    m_appFrame( static_cast< AppFrame* >( parent ) ),
    m_ceServerHostTextCtrl( NULL ),
    m_cePortTextCtrl( NULL )
{
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
VESConnectionDialog::~VESConnectionDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void VESConnectionDialog::CreateGUI()
{
	SetSizeHints( wxDefaultSize, wxDefaultSize );
	SetBackgroundColour( wxColour( 255, 255, 255 ) );
	
	wxBoxSizer* mainSizer;
	mainSizer = new wxBoxSizer( wxVERTICAL );
	
	wxStaticBoxSizer* connectionSizer;
	connectionSizer = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT( "Connect to Server Instance" ) ), wxVERTICAL );
	
	wxBoxSizer* workingDirectorySizer;
	workingDirectorySizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* workingDirectoryStaticText;
	workingDirectoryStaticText = new wxStaticText( this, wxID_ANY, wxT( "Working Directory:" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	workingDirectoryStaticText->Wrap( -1 );
	workingDirectorySizer->Add( workingDirectoryStaticText, 1, wxALIGN_CENTER | wxALL, 5 );
	
	m_workingDirPickerCtrl = new wxDirPickerCtrl( this, wxID_ANY, wxEmptyString, wxT( "Select a folder" ), wxDefaultPosition, wxDefaultSize, wxDIRP_DEFAULT_STYLE | wxDIRP_USE_TEXTCTRL );
	workingDirectorySizer->Add( m_workingDirPickerCtrl, 2, wxALIGN_CENTER | wxALL, 5 );
	
	connectionSizer->Add( workingDirectorySizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* ceServerHostSizer;
	ceServerHostSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* ceServerHostStaticText;
	ceServerHostStaticText = new wxStaticText( this, wxID_ANY, wxT( "CE Server Host:" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	ceServerHostStaticText->Wrap( -1 );
	ceServerHostSizer->Add( ceServerHostStaticText, 2, wxALIGN_CENTER | wxALL, 5 );
	
	m_ceServerHostTextCtrl = new wxTextCtrl( this, wxID_ANY, wxT( "localhost" ), wxDefaultPosition, wxDefaultSize, 0 );
	ceServerHostSizer->Add( m_ceServerHostTextCtrl, 2, wxALIGN_CENTER | wxALL, 5 );
	
	wxStaticText* cePortStaticText;
	cePortStaticText = new wxStaticText( this, wxID_ANY, wxT( "CE Port:" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	cePortStaticText->Wrap( -1 );
	ceServerHostSizer->Add( cePortStaticText, 1, wxALIGN_CENTER | wxALL, 5 );
	
	m_cePortTextCtrl = new wxTextCtrl( this, wxID_ANY, wxT( "1239" ), wxDefaultPosition, wxDefaultSize, 0 );
	ceServerHostSizer->Add( m_cePortTextCtrl, 1, wxALIGN_CENTER | wxALL, 5 );
	
	connectionSizer->Add( ceServerHostSizer, 1, wxALL | wxEXPAND, 5 );
	
	mainSizer->Add( connectionSizer, 1, wxALL | wxEXPAND, 5 );
	
	wxBoxSizer* buttonSizer;
	buttonSizer = new wxBoxSizer( wxHORIZONTAL );
	buttonSizer->Add( 0, 0, 1, wxEXPAND, 5 );
	
	wxButton* connectButton;
	connectButton = new wxButton( this, CONNECT_VESCD, wxT( "Connect" ), wxDefaultPosition, wxDefaultSize, 0 );
	buttonSizer->Add( connectButton, 0, wxALL, 5 );
	
	wxButton* clearButton;
	clearButton = new wxButton( this, CLEAR_VESCD, wxT( "Clear" ), wxDefaultPosition, wxDefaultSize, 0 );
	buttonSizer->Add( clearButton, 0, wxALL, 5 );
	
	wxButton* cancelButton;
	cancelButton = new wxButton( this, wxID_CANCEL );
	buttonSizer->Add( cancelButton, 0, wxALL, 5 );
	
	mainSizer->Add( buttonSizer, 0, wxEXPAND, 5 );
	
	SetSizer( mainSizer );
	Layout();
}
////////////////////////////////////////////////////////////////////////////////
void VESConnectionDialog::Connect( wxCommandEvent& WXUNUSED( event ) )
{
    std::string workingDirectory = m_workingDirPickerCtrl->GetPath().mb_str();
    std::string ceServerHost = m_ceServerHostTextCtrl->GetValue().mb_str();
    std::string cePort = m_cePortTextCtrl->GetValue().mb_str();

    if( workingDirectory != wxT( "" ) &&
        ceServerHost != wxT( "" ) &&
        cePort != wxT( "" ) )
    {
        CorbaUnitManager* corbaUnitManager = m_appFrame->GetCorbaUnitManager();
        if( corbaUnitManager->RunORB( workingDirectory, ceServerHost, cePort ) )
        {
            m_appFrame->GetAppToolBar()->DisableVESConnectionDialog();

            Hide();

            wxMessageDialog msgDlg(
                this, wxT( "Connection successful" ), wxT( "Success" ) );
            msgDlg.ShowModal();
        }
        else
        {
            wxMessageDialog wrnDlg(
                this, wxT( "Unable to connect to VE-CE" ), wxT( "Warning" ),
                wxOK | wxCENTRE | wxICON_EXCLAMATION );
            wrnDlg.ShowModal();
        }
    }
    else
    {
        wxMessageDialog errorDlg(
            this, wxT( "Left required field empty" ), wxT( "Error" ),
            wxOK | wxCENTRE | wxICON_ERROR );
        errorDlg.ShowModal();
    }
}
////////////////////////////////////////////////////////////////////////////////
void VESConnectionDialog::Clear( wxCommandEvent& WXUNUSED( event ) )
{
    m_ceServerHostTextCtrl->SetValue( wxT( "" ) );
    m_cePortTextCtrl->SetValue( wxT( "" ) );
}
////////////////////////////////////////////////////////////////////////////////
