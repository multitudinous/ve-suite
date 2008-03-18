// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "CameraPlacementToolUIDialog.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/dialog.h>

using namespace cpt;

BEGIN_EVENT_TABLE( CameraPlacementToolUIDialog, wxDialog )
    EVT_BUTTON( OK_BUTTON, CameraPlacementToolUIDialog::OnOK )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::CameraPlacementToolUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::CameraPlacementToolUIDialog( 
    wxWindow* parent,
    int id, 
    ves::conductor::util::CORBAServiceList* service )
:
UIDialog( ( wxWindow* )parent, id, _( "CameraPlacementTool" ) )
{
    mServiceList = service;

    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::~CameraPlacementToolUIDialog()
{
    mServiceList->CleanUp();
}
////////////////////////////////////////////////////////////////////////////////
bool CameraPlacementToolUIDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraPlacementToolUIDialog::TransferDataToWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::Lock( bool l )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::BuildGUI()
{
    CenterOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateGUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnOK( wxCommandEvent& event )
{
    Close( true );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::ClearInstructions()
{
    mInstructions.clear();
    mCommandName.clear();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::SendCommandsToXplorer()
{
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 

    for( size_t i = 0; i < mInstructions.size(); ++i )
    {
        command->AddDataValuePair( mInstructions.at( i ) );
    }

    command->SetCommandName( mCommandName );

    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
