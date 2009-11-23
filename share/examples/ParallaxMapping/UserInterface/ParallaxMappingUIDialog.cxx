
// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- ParallaxMapping Includes --- //
#include "ParallaxMappingUIDialog.h"

// --- VE-Suite Includes --- //
//#include <ves/conductor/util/spinctld.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/frame.h>

BEGIN_EVENT_TABLE( ParallaxMappingUIDialog, wxDialog )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ParallaxMappingUIDialog::ParallaxMappingUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ParallaxMappingUIDialog::ParallaxMappingUIDialog(
    wxWindow* parent,
    int id, 
    ves::conductor::util::CORBAServiceList* service )
    :
    UIDialog( ( wxWindow* )parent, id, wxT( "ParallaxMapping" ) )
{
    mServiceList = service;

    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
ParallaxMappingUIDialog::~ParallaxMappingUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool ParallaxMappingUIDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool ParallaxMappingUIDialog::TransferDataToWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void ParallaxMappingUIDialog::Lock( bool l )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ParallaxMappingUIDialog::BuildGUI()
{
    SetSizeHints( wxDefaultSize, wxDefaultSize );
    SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    wxBoxSizer* mainSizer;
    mainSizer = new wxBoxSizer( wxVERTICAL );

    //Add code here

    SetSizer( mainSizer );
    Layout();
    mainSizer->Fit( this );
    CenterOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void ParallaxMappingUIDialog::ClearInstructions()
{
    mInstructions.clear();
    mCommandName.clear();
}
////////////////////////////////////////////////////////////////////////////////
void ParallaxMappingUIDialog::SendCommandsToXplorer()
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
