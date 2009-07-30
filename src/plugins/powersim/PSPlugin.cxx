
// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>
#include <plugins/powersim/PSPlugin.h>
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
//#include <wx/msgdlg.h>
#include <wx/image.h>
//#include <wx/window.h>
//#include <wx/filedlg.h>
//#include <wx/filename.h>

using namespace ves::conductor;

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
    //m_powersimMenu->Enable( APPLUGIN_OPEN_SIM, true );
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
void PSPlugin::OnOpen( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )

    /*
    if( IsBKPOpen() )
    {
        wxMessageDialog md( m_canvas, 
            wxT( "Simulation already open.\nClose it and open another?" ),
            wxT( "Confirm" ),
            wxYES_NO);
        if( md.ShowModal() == wxID_NO )
        {
            return;
        }
        else
        {
            DisconnectAspenSimulation();
            CloseAspenSimulation();
        }
    }

    APOpenDialog fd( m_canvas );
    fd.SetPopulateFilenames( );

    if( fd.ShowModal() != wxID_OK )
    {
        return;
    }

    wxFileName bkpFileName;
    bkpFileName.ClearExt();
    bkpFileName.SetName( fd.GetFilename() + wxT(".bkp") );

    CommandPtr returnState ( new Command() );
    returnState->SetCommandName( "getNetwork" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "getNetwork" );
    returnState->AddDataValuePair( data );

    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "BKPFileName",  ConvertUnicode( bkpFileName.GetFullName().c_str() ) );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );
    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    //Get results
    std::string nw_str = serviceList->Query( status );
    
    if( nw_str.empty() )
    {
        wxMessageDialog md( m_canvas, wxT("No Aspen Unit connected.\nPlease launch Aspen Unit."), wxT("Error"), wxOK);
        md.ShowModal();
        return;
    }

    // If there is nothing on the CE
    if( nw_str.compare("BKPDNE") == 0 )
    {
        wxMessageDialog md(m_canvas, wxT("Aspen Unit is unable to find the bkp file.\nDid you select the correct directory in Aspen Unit?" ), wxT("Error"), wxOK );
        md.ShowModal();
        //Log( "BKP File Does NOT exist.\n" );
        return;
    }    
    else if( nw_str.compare("APWDNE") == 0 )
    {
        wxMessageDialog md( m_canvas, wxT("Aspen Unit is unable to find the apw file.\nDid you select the correct directory in Aspen Unit?" ), wxT("Error"), wxOK);
        md.ShowModal();
        //Log( "APW File Does NOT exist.\n" );
        return;
    }
    
    //Parse the network string thst was returned from the VE-PSI Unit
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    std::vector< std::pair< std::string, std::string > > dataToObtain;
    std::vector< std::pair< std::string, std::string > >::iterator dataIter;
    dataToObtain.push_back( std::make_pair( "Model", "veSystem" ) );
    networkWriter.ReadXMLData( nw_str, dataToObtain );
    //Now get the veopen classes from the network string
    std::vector< ves::open::xml::XMLObjectPtr >::iterator objectIter;
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkWriter.GetLoadedXMLObjects();

    //Now we need to make this plugin the top level plugin becuase
    //the aspen flowsheet is actually a subnetwork of this 
    //main aspen plus plugin
    ves::open::xml::model::SystemPtr tempSystem;
    tempSystem = boost::dynamic_pointer_cast<ves::open::xml::model::System>( objectVector.at( 0 ) );
    ves::open::xml::model::ModelPtr aspenPlusModel;
    //set a null pointer as the top most parent model on topmost level
    for( size_t modelCount = 0; 
        modelCount < tempSystem->GetNumberOfModels(); 
        ++modelCount )
    {
        //Not sure why we set a null pointer here...
        tempSystem->GetModel( modelCount )->SetParentModel( aspenPlusModel );
    }
    //Now we get this plugins veopen model and set its subsystem as the
    //flowsheet we just queried from VE-PSI
    GetVEModel()->SetSubSystem( tempSystem );
    mDataBufferEngine->ParseSystem( tempSystem );

    //Now let the rest of VE-Conductor know about the new network
    m_canvas->AddSubNetworks();
#if 0
    std::ofstream netdump ("netdump.txt");
    netdump << nw_str;
    netdump.close();
#endif
    
    event.SetId( UPDATE_HIER_TREE );
    ::wxPostEvent( m_canvas, event );

    //create hierarchy page
    //hierarchyTree->PopulateTree( 
    //    mDataBufferEngine->GetTopSystemId() );

    //Log( "Simulation Opened.\n" );
    ///
    CommandPtr aspenBKPFile( new Command() );
    aspenBKPFile->SetCommandName( "Aspen_Plus_Preferences" );
    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "BKPFileName",
                   ConvertUnicode( bkpFileName.GetFullName().c_str() ) );
    aspenBKPFile->AddDataValuePair( data );
    mUserPrefBuffer->SetCommand( "Aspen_Plus_Preferences", aspenBKPFile );

    SetName( fd.GetFilename() );
    event.SetId( UIPLUGINBASE_SET_UI_PLUGIN_NAME );
    GlobalNameUpdate( event );

    //mAspenMenu->Enable( APPLUGIN_CLOSE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_DISCONNECT_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_SHOW_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_HIDE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_RUN_ASPEN_NETWORK, true );
    mAspenMenu->Enable( APPLUGIN_REINITIALIZE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_STEP_ASPEN_NETWORK, true );
    mAspenMenu->Enable( APPLUGIN_SAVE_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_SAVEAS_SIMULATION, true );
    */
}
/////////////////////////////////////////////////////////////////////////////
