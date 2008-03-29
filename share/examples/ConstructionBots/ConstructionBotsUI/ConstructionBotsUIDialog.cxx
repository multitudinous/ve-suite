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

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ConstructionBotsUIDialog::ConstructionBotsUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ConstructionBotsUIDialog::ConstructionBotsUIDialog(
    wxWindow* parent,
    int id, 
    ves::conductor::util::CORBAServiceList* service )
        :
        UIDialog( static_cast< wxWindow* >( parent ),
                  id,
                  wxT( "ConstructionBots" ) )
{
    mServiceList = service;

    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
ConstructionBotsUIDialog::~ConstructionBotsUIDialog()
{
    mServiceList->CleanUp();
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
    CenterOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::UpdateGUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::ClearInstructions()
{
    mInstructions.clear();
    mCommandName.clear();
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::SendCommandsToXplorer()
{
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 

    for( size_t i = 0; i < mInstructions.size(); i++ )
    {
        command->AddDataValuePair( mInstructions.at( i ) );
    }

    command->SetCommandName( mCommandName );

    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
