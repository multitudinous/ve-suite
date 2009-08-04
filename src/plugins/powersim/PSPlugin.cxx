
// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>
#include <plugins/powersim/PSPlugin.h>
#include <plugins/powersim/PSOpenDialog.h>
#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/xpm/powersim/PSStudio.xpm>

#include <ves/conductor/ConductorLibEnums.h>
#include <ves/conductor/UserPreferencesDataBuffer.h>
#include <ves/conductor/XMLDataBufferEngine.h>
#include <ves/conductor/Network.h>
#include <ves/conductor/Module.h>

#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/XMLReaderWriter.h>

// --- wxWidgets Includes --- //
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/image.h>
//#include <wx/window.h>
//#include <wx/filedlg.h>
#include <wx/filename.h>

using namespace ves::conductor;
namespace vox = ves::open::xml;

BEGIN_EVENT_TABLE( PSPlugin, ves::conductor::UIPluginBase )
EVT_MENU( PS_PLUGIN_OPEN, PSPlugin::OnOpen )
/*
EVT_MENU( APPLUGIN_SHOW_ASPEN_SIMULATION, PSPlugin::ShowAspenSimulation )
EVT_MENU( APPLUGIN_HIDE_ASPEN_SIMULATION, PSPlugin::HideAspenSimulation )
EVT_MENU( APPLUGIN_CLOSE_ASPEN_SIMULATION, PSPlugin::OnCloseAspenSimulation )
EVT_MENU( APPLUGIN_DISCONNECT_ASPEN_SIMULATION, PSPlugin::OnDisconnectAspenSimulation )
EVT_MENU( APPLUGIN_RUN_ASPEN_NETWORK, PSPlugin::RunAspenNetwork )
EVT_MENU( APPLUGIN_REINITIALIZE_ASPEN_SIMULATION, PSPlugin::ReinitializeAspenSimulation )
EVT_MENU( APPLUGIN_STEP_ASPEN_NETWORK, PSPlugin::StepAspenNetwork )
EVT_MENU( APPLUGIN_SAVE_SIMULATION, PSPlugin::SaveSimulation )
EVT_MENU( APPLUGIN_SAVEAS_SIMULATION, PSPlugin::SaveAsSimulation )
*/
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( PSPlugin, ves::conductor::UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
PSPlugin::PSPlugin()
    :
    UIPluginBase(),
    m_powersimMenu( NULL )
{
    mPluginName = wxString( "Powersim", wxConvUTF8 );
    mDescription = wxString( "Powersim Plugin", wxConvUTF8 );
    GetVEModel()->SetPluginType( "PSPlugin" );

    iconFilename = "PSStudio_xpm";
    wxImage image( PSStudio_xpm );
    SetImage( image );
}
////////////////////////////////////////////////////////////////////////////////
PSPlugin::~PSPlugin()
{
    ;
    /*
    if( IsBKPOpen() )
    {
        DisconnectAspenSimulation();
        CloseAspenSimulation();
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
wxString PSPlugin::GetConductorName()
{
    return wxString( "Powersim_PS", wxConvUTF8 );
}
////////////////////////////////////////////////////////////////////////////////
wxMenu* PSPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
{
    if( m_powersimMenu )
    {
        return baseMenu;
    }

    baseMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, true );
    wxMenu* menu = baseMenu->FindItemByPosition( 0 )->GetSubMenu();
    menu->Enable( UIPLUGINBASE_MODEL_INPUTS, false );
    menu->Enable( UIPLUGINBASE_MODEL_RESULTS, false );
    menu->Enable( UIPLUGINBASE_SHOW_ICON_CHOOSER, false );

    m_powersimMenu = new wxMenu();
    m_powersimMenu->Append( PS_PLUGIN_OPEN, wxT( "Open" ) );
    m_powersimMenu->Enable( PS_PLUGIN_OPEN, true );
    //m_powersimMenu->Append( APPLUGIN_CLOSE_ASPEN_SIMULATION, wxT( "Close" ) );
    //m_powersimMenu->Append( APPLUGIN_DISCONNECT_ASPEN_SIMULATION, wxT( "Disconnect" ) );
    //m_powersimMenu->Append( APPLUGIN_SHOW_ASPEN_SIMULATION, wxT( "Show" ) );
    //m_powersimMenu->Append( APPLUGIN_HIDE_ASPEN_SIMULATION, wxT( "Hide" ) );
    //m_powersimMenu->Append( APPLUGIN_RUN_ASPEN_NETWORK, wxT( "Run" ) );
    //m_powersimMenu->Append( APPLUGIN_REINITIALIZE_ASPEN_SIMULATION, wxT( "Reinitialize" ) );
    //m_powersimMenu->Append( APPLUGIN_STEP_ASPEN_NETWORK, wxT( "Step" ) );
    //m_powersimMenu->Append( APPLUGIN_SAVE_SIMULATION, wxT( "Save" ) );
    //m_powersimMenu->Append( APPLUGIN_SAVEAS_SIMULATION, wxT( "SaveAs" ) );

    baseMenu->Insert(
        0, PS_PLUGIN_MENU, wxT( "Powersim" ),
        m_powersimMenu, wxT( "Used in conjunction with Powersim" ) );
    baseMenu->Enable( PS_PLUGIN_MENU, true );

    if( GetVEModel()->GetSubSystem() != NULL )
    {
        //m_powersimMenu->Enable( APPLUGIN_CLOSE_ASPEN_SIMULATION, true );
        //m_powersimMenu->Enable( APPLUGIN_DISCONNECT_ASPEN_SIMULATION, true );
        //m_powersimMenu->Enable( APPLUGIN_SHOW_ASPEN_SIMULATION, true );
        //m_powersimMenu->Enable( APPLUGIN_HIDE_ASPEN_SIMULATION, true );
        //m_powersimMenu->Enable( APPLUGIN_RUN_ASPEN_NETWORK, true );
        //m_powersimMenu->Enable( APPLUGIN_REINITIALIZE_ASPEN_SIMULATION, true );
        //m_powersimMenu->Enable( APPLUGIN_STEP_ASPEN_NETWORK, true );
        //m_powersimMenu->Enable( APPLUGIN_SAVE_SIMULATION, true );
        //m_powersimMenu->Enable( APPLUGIN_SAVEAS_SIMULATION, true );
    }
    else
    {
        //m_powersimMenu->Enable( APPLUGIN_CLOSE_ASPEN_SIMULATION, false );
        //m_powersimMenu->Enable( APPLUGIN_DISCONNECT_ASPEN_SIMULATION, false );
        //m_powersimMenu->Enable( APPLUGIN_SHOW_ASPEN_SIMULATION, false );
        //m_powersimMenu->Enable( APPLUGIN_HIDE_ASPEN_SIMULATION, false );
        //m_powersimMenu->Enable( APPLUGIN_RUN_ASPEN_NETWORK, false );
        //m_powersimMenu->Enable( APPLUGIN_REINITIALIZE_ASPEN_SIMULATION, false );
        //m_powersimMenu->Enable( APPLUGIN_STEP_ASPEN_NETWORK, false );
        //m_powersimMenu->Enable( APPLUGIN_SAVE_SIMULATION, false );
        //m_powersimMenu->Enable( APPLUGIN_SAVEAS_SIMULATION, false );
    }

    return baseMenu;
}
////////////////////////////////////////////////////////////////////////////////
bool PSPlugin::IsSIPOpen()
{
    /*
    if( mUserPrefBuffer )
    {
        CommandPtr aspenBKPFile = mUserPrefBuffer->GetCommand( "Aspen_Plus_Preferences" );

        if( aspenBKPFile->GetCommandName() != "NULL" )
        {
            DataValuePairPtr bkpPtr =
                aspenBKPFile->GetDataValuePair( "BKPFileName" );
            if( bkpPtr )
            {
                return true;
            }
        }
    }
    */

    return false;
}
////////////////////////////////////////////////////////////////////////////////
void PSPlugin::OnOpen( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )

    PSOpenDialog fd( m_canvas );
    fd.SetPopulateFilenames( );

    if( fd.ShowModal() != wxID_OK )
    {
        return;
    }

    wxFileName sipFileName;
    sipFileName.ClearExt();
    sipFileName.SetName( fd.GetFilename() + wxT( ".sip" ) );

    vox::CommandPtr returnState ( new vox::Command() );
    returnState->SetCommandName( "GetNetwork" );
    vox::DataValuePairPtr data( new vox::DataValuePair() );
    data->SetData( "NetworkQuery", "GetNetwork" );
    returnState->AddDataValuePair( data );

    data = vox::DataValuePairPtr( new vox::DataValuePair() );
    data->SetData(
        "sipFileName", ConvertUnicode( sipFileName.GetFullName().c_str() ) );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< vox::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< vox::XMLObjectPtr, std::string >(
        returnState, "vecommand" ) );
    vox::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    //Get results
    std::string nw_str = serviceList->Query( status );
    if( nw_str.empty() )
    {
        wxMessageDialog md(
            m_canvas,
            wxT( "No Powersim Unit connected.\nPlease launch Powersim Unit." ),
            wxT( "Error" ), wxOK );
        md.ShowModal();

        return;
    }

    //If there is nothing on the CE
    if( nw_str.compare( "SIPDNE" ) == 0 )
    {
        wxMessageDialog md(
            m_canvas,
            wxT( "Powersim Unit is unable to find the sip file.\nDid you select the correct directory in Powersim Unit?" ),
            wxT( "Error" ), wxOK );
        md.ShowModal();
        //Log( "SIP File Does NOT exist.\n" );

        return;
    }

    vox::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    std::vector< std::pair< std::string, std::string > > dataToObtain;
    std::vector< std::pair< std::string, std::string > >::iterator dataIter;
    dataToObtain.push_back( std::make_pair( "Model", "veSystem" ) );
    networkWriter.ReadXMLData( nw_str, dataToObtain );
    std::vector< vox::XMLObjectPtr >::iterator objectIter;
    std::vector< vox::XMLObjectPtr > objectVector =
        networkWriter.GetLoadedXMLObjects();

    vox::model::SystemPtr tempSystem;
    tempSystem = boost::dynamic_pointer_cast< vox::model::System >(
        objectVector.at( 0 ) );
    vox::model::ModelPtr powersimModel;
    //Set parent model on topmost level
    for( int i = 0; i < tempSystem->GetNumberOfModels(); ++i )
    {
        tempSystem->GetModel( i )->SetParentModel( powersimModel );
    }

    GetVEModel()->SetSubSystem( tempSystem );
    mDataBufferEngine->ParseSystem( tempSystem );

    m_canvas->AddSubNetworks( );
    event.SetId( UPDATE_HIER_TREE );
    wxPostEvent( m_canvas, event );

    vox::CommandPtr powersimSIPFile( new vox::Command() );
    powersimSIPFile->SetCommandName( "sipPreferences" );
    data = vox::DataValuePairPtr( new vox::DataValuePair() );
    data->SetData(
        "sipFileName", ConvertUnicode( sipFileName.GetFullName().c_str() ) );
    powersimSIPFile->AddDataValuePair( data );
    mUserPrefBuffer->SetCommand( "sipPreferences", powersimSIPFile );

    SetName( fd.GetFilename() );
    event.SetId( UIPLUGINBASE_SET_UI_PLUGIN_NAME );
    GlobalNameUpdate( event );

    //mAspenMenu->Enable( SDPLUGIN_CLOSE_ASPEN_SIMULATION, true );
    //mAspenMenu->Enable( SDPLUGIN_DISCONNECT_ASPEN_SIMULATION, true );
    //mAspenMenu->Enable( SDPLUGIN_SHOW_ASPEN_SIMULATION, true );
    //mAspenMenu->Enable( SDPLUGIN_HIDE_ASPEN_SIMULATION, true );
    //mAspenMenu->Enable( SDPLUGIN_RUN_ASPEN_NETWORK, true );
    //mAspenMenu->Enable( SDPLUGIN_REINITIALIZE_ASPEN_SIMULATION, true );
    //mAspenMenu->Enable( SDPLUGIN_STEP_ASPEN_NETWORK, true );
    //mAspenMenu->Enable( SDPLUGIN_SAVE_SIMULATION, true );
    //mAspenMenu->Enable( SDPLUGIN_SAVEAS_SIMULATION, true );
}
/////////////////////////////////////////////////////////////////////////////
