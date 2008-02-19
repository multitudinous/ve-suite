// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "ConstructionBotsUIDialog.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/dialog.h>

BEGIN_EVENT_TABLE( ConstructionBotsUIDialog, wxDialog )
    EVT_BUTTON( OK_BUTTON, ConstructionBotsUIDialog::OnOK )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ConstructionBotsUIDialog::ConstructionBotsUIDialog( wxWindow* parent, int id, ves::conductor::util::CORBAServiceList* service, std::string* portNumber )
:
UIDialog( (wxWindow *)parent, id, "ConstructionBots" ), p_portNumber( portNumber )
{
    serviceList = service;

    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
ConstructionBotsUIDialog::~ConstructionBotsUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool ConstructionBotsUIDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool ConstructionBotsUIDialog::TransferDataToWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::Lock( bool l )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::BuildGUI()
{
    SetForegroundColour( wxColour( 255, 255, 255 ) );
    SetBackgroundColour( wxColour( 0, 0, 0 ) );

    SetBestFittingSize();
    CenterOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::UpdateGUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::OnOK( wxCommandEvent& event )
{
    Close( true );
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::ClearInstructions()
{
    instructions.clear();
    command_name.clear();
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::SendCommandsToXplorer()
{
    ves::open::xml::Command* command = new ves::open::xml::Command();

    for( size_t i = 0; i < instructions.size(); i++ )
    {
        command->AddDataValuePair( instructions.at( i ) );
    }

    command->SetCommandName( command_name );

    serviceList->SendCommandStringToXplorer( command );

    //Clean up memory
    delete command;
}
////////////////////////////////////////////////////////////////////////////////
