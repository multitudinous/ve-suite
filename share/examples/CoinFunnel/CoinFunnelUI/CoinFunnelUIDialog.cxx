// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "CoinFunnelUIDialog.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/dialog.h>

BEGIN_EVENT_TABLE( CoinFunnelUIDialog, wxDialog )
EVT_BUTTON( OK_BUTTON, CoinFunnelUIDialog::OnOK )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
CoinFunnelUIDialog::CoinFunnelUIDialog( wxWindow* parent,
                                        int id,
                                        ves::conductor::util::CORBAServiceList* service,
                                        std::string* portNumber )
:
UIDialog( ( wxWindow * )parent, id, _( "CoinFunnel" ) ), p_portNumber( portNumber )
{
    serviceList = service;

    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
CoinFunnelUIDialog::~CoinFunnelUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool CoinFunnelUIDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool CoinFunnelUIDialog::TransferDataToWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::Lock( bool l )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::BuildGUI()
{
    SetForegroundColour( wxColour( 255, 255, 255 ) );
    SetBackgroundColour( wxColour( 0, 0, 0 ) );

    SetBestFittingSize();
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::UpdateGUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::OnOK( wxCommandEvent& event )
{
    Close( true );
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::ClearInstructions()
{
    instructions.clear();
    command_name.clear();
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::SendCommandsToXplorer()
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
