// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "UserInterfaceDialog.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/dialog.h>

BEGIN_EVENT_TABLE( UserInterfaceDialog, wxDialog )
    EVT_BUTTON( OK_BUTTON, UserInterfaceDialog::OnOK )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
UserInterfaceDialog::UserInterfaceDialog( wxWindow* parent, int id, 
    ves::conductor::util::CORBAServiceList* service, std::string* portNumber )
:
UIDialog( ( wxWindow * )parent, id, _( "CameraPlacementTool" ) ), p_portNumber( portNumber )
{
    serviceList = service;

    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
UserInterfaceDialog::~UserInterfaceDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool UserInterfaceDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool UserInterfaceDialog::TransferDataToWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void UserInterfaceDialog::Lock( bool l )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UserInterfaceDialog::BuildGUI()
{
    SetForegroundColour( wxColour( 255, 255, 255 ) );
    SetBackgroundColour( wxColour( 0, 0, 0 ) );

    CenterOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void UserInterfaceDialog::UpdateGUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UserInterfaceDialog::OnOK( wxCommandEvent& event )
{
    Close( true );
}
////////////////////////////////////////////////////////////////////////////////
void UserInterfaceDialog::ClearInstructions()
{
    instructions.clear();
    command_name.clear();
}
////////////////////////////////////////////////////////////////////////////////
void UserInterfaceDialog::SendCommandsToXplorer()
{
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 

    for( size_t i = 0; i < instructions.size(); ++i )
    {
        command->AddDataValuePair( instructions.at( i ) );
    }

    command->SetCommandName( command_name );

    serviceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
