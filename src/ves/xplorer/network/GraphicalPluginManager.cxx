/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/network/VE_i.h>
#include <ves/xplorer/network/cfdVEAvailModules.h>
#include <ves/xplorer/network/cfdVEPluginLoader.h>
#include <ves/xplorer/network/UpdateNetworkEventHandler.h>
#include <ves/xplorer/network/NetworkSystemView.h>
#include <ves/xplorer/network/GraphicalPluginManager.h>
#include <ves/xplorer/network/DeleteObjectFromNetworkEventHandler.h>
#include <ves/xplorer/network/DeleteNetworkViewEventHandler.h>
#include <ves/xplorer/network/SwitchXplorerViewEventHandler.h>
#include <ves/xplorer/network/ReloadPluginsEventHandler.h>
#ifdef QT_ON
#include <ves/xplorer/network/LoadVesFileEventHandler.h>
#endif // QT_ON

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>

#include <ves/xplorer/plugin/PluginBase.h>

#include <ves/xplorer/command/CommandManager.h>

#include <ves/xplorer/communication/CommunicationHandler.h>

#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/ResourceManager.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

#ifdef MINERVA_GIS_SUPPORT
#include <ves/xplorer/minerva/MinervaManager.h>
#endif

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/System.h>

#ifdef VE_SOUND
// --- osgAudio Includes --- //
#include <osgAudio/SoundManager.h>
#endif

// --- VR Juggler Includes --- //
#include <vpr/System.h>
#include <vpr/Util/GUID.h>

// --- ACE-TAO Includes --- //
#include <orbsvcs/CosNamingC.h>

// --- Xercesc Includes --- //
#include <xercesc/dom/DOM.hpp>
XERCES_CPP_NAMESPACE_USE

// --- STL Includes --- //
#include <iostream>
#include <fstream>
#include <sstream>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;
using namespace ves::open::xml;
using namespace ves::xplorer::plugin;
using namespace ves::xplorer::network;
using namespace ves::xplorer::command;

vprSingletonImpLifetime( ves::xplorer::network::GraphicalPluginManager, 0 );

////////////////////////////////////////////////////////////////////////////////
GraphicalPluginManager::GraphicalPluginManager()
    :
    mAvailableModules( 0 ),
    ui_i( 0 ),
    naming_context( 0 ),
    _exec( 0 ),
    netSystemView( 0 ),
    m_ChildPOA( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::Initialize( CosNaming::NamingContext* inputNameContext,
                               PortableServer::POA* child_poa )
{
    this->naming_context = inputNameContext;
    m_ChildPOA = child_poa;
    
    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch ( const XMLException &toCatch )
    {
        std::cerr << "Error during Xerces-c Initialization.\n"
            << "  Exception message:"
            << XMLString::transcode( toCatch.getMessage() ) << std::endl;
        return;
    }

    mAvailableModules = new cfdVEAvailModules();

    std::ostringstream dirStringStream;
    dirStringStream << "VEClient-" << vpr::System::getHostname()
        << "-" <<  vpr::GUID( vpr::GUID::generateTag ).toString();
    m_UINAME = dirStringStream.str();

    ConnectToCE();
    if( ui_i )
    {
        ui_i->GetNetworkFromCE();
        LoadDataFromCE();
    }
    
    _eventHandlers[std::string( "DELETE_OBJECT_FROM_NETWORK" )] = 
        new DeleteObjectFromNetworkEventHandler();
    _eventHandlers[std::string( "DELETE_NETWORK_SYSTEM_VIEW" )] = 
        new DeleteNetworkViewEventHandler();
    _eventHandlers[std::string( "CHANGE_XPLORER_VIEW" )] = 
        new SwitchXplorerViewEventHandler();
    _eventHandlers[std::string( "Plugin_Control" )] = 
        new ReloadPluginsEventHandler();
    _eventHandlers[std::string( "veNetwork Update" )] = 
        new UpdateNetworkEventHandler();
#ifdef QT_ON
    _eventHandlers[std::string( "LOAD_VES_FILE" )]=
        new LoadVesFileEventHandler();
#endif // QT_ON
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, ves::xplorer::plugin::PluginBase* >* 
    GraphicalPluginManager::GetTheCurrentPlugins()
{
    return &mPluginsMap;
}
////////////////////////////////////////////////////////////////////////////////
GraphicalPluginManager::~GraphicalPluginManager()
{
    
    mPluginsMap.clear();
    _id_map.clear();
    pluginEHMap.clear();
    for( std::map< std::string, ves::xplorer::event::EventHandler* >::iterator iter = _eventHandlers.begin(); iter != _eventHandlers.end(); ++iter )
    {
        delete iter->second;
    }
    _eventHandlers.clear();

    if( mAvailableModules )
    {
        delete mAvailableModules;
        mAvailableModules = 0;
    }

    if(netSystemView)
    {
        delete netSystemView;
    }

    delete ui_i;
    ui_i = 0;
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::UnloadPlugins()
{
    mPluginsMap.clear();
    _id_map.clear();
    //idToModel.clear();
    pluginEHMap.clear();
    _eventHandlers.clear();

    delete mAvailableModules;
    mAvailableModules = 0;
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::UnRegisterExecutive()
{
    try
    {
        if( ui_i )
        {
            _exec->UnRegisterUI( ui_i->UIName_.c_str() );
            std::cout << "|\tDisconnect from VE-CE succeeded!" << std::endl;
        }
    }
    catch ( CORBA::Exception& ex )
    {
        std::cerr << "|\tDisconnect from VE_CE failed!" << std::endl;
        std::cerr << ex._info().c_str() << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::UnbindORB()
{
    if( !ui_i )
    {
        return;
    }

    CosNaming::Name UIname( 1 );
    UIname.length( 1 );
    UIname[0].id = CORBA::string_dup(( ui_i->UIName_ ).c_str() );

    try
    {
        this->naming_context->unbind( UIname );
    }
    catch ( CosNaming::NamingContext::InvalidName& ex )
    {
        vprDEBUG( vesDBG, 1 ) << "|\t\tGraphicalPluginManager : Invalid Name! "
            << ex._info().c_str() << std::endl << vprDEBUG_FLUSH;
    }
    catch ( CosNaming::NamingContext::NotFound& ex )
    {
        vprDEBUG( vesDBG, 1 ) << "|\t\tGraphicalPluginManager : Not Found! "
            << ex._info().c_str() << std::endl << vprDEBUG_FLUSH;
    }
    catch ( CosNaming::NamingContext::CannotProceed& ex )
    {
        vprDEBUG( vesDBG, 1 ) << "|\t\tGraphicalPluginManager : Cannot Proceed! "
            << ex._info().c_str() << std::endl << vprDEBUG_FLUSH;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::GetNetwork()
{
    // Get buffer value from Body_UI implementation
    std::string temp( ui_i->GetNetworkString() );
    //std::ofstream output("xplorerNetwork.txt");
    //output<<temp<<std::endl;
    //output.close();
    const std::string network = temp;
    veNetwork = network;
    vprDEBUG( vesDBG, 2 ) << "|\t\tNetwork String : " << network
        << std::endl << vprDEBUG_FLUSH;

    // Load from the nt file loaded through wx
    // Get a list of all the command elements
    XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();

    // we can do this because the plugins will actually
    // manage the memory for these models. When a plugin is deleted the
    // plugin will be responsible for deleting the veModel pointer
    // This logic also works for the case where a custom plugin doesn't exist because
    // there will be a default plugin that will be created just like there
    // is currently for conductor
    _id_map.clear();
    mIDToSystem.clear();
    // do this for models
    networkWriter.ReadXMLData( network, "System", "veSystem" );
    std::vector< XMLObjectPtr > currentModels;
    currentModels = networkWriter.GetLoadedXMLObjects();
    model::SystemPtr tempSystem = boost::dynamic_pointer_cast<model::System>( currentModels.at( 0 ) );

    if( currentModels.size() > 1 )
    {
        std::cerr << "There is a problem in the Xplorer network parser" 
            << std::endl;
    }

    //Store top level systems
    mTopSystemID = tempSystem->GetID();
    //Construct map of systems
    //Loop over all systems and get all models on the map
    scenegraph::Group* graphicalPluginManager =
        &scenegraph::SceneManager::instance()->GetGraphicalPluginManager();
    ParseSystem( tempSystem, true, graphicalPluginManager );

    //create network system view
    netSystemView = new NetworkSystemView( veNetwork );
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::GetEverything()
{
    if( CORBA::is_nil( this->_exec ) )
    {
        vprDEBUG( vesDBG, 3 ) << "ERROR : The Executive has not been intialized!"
            << std::endl << vprDEBUG_FLUSH;
        return;
    }

    vprDEBUG( vesDBG, 0 ) << "|\t\tGetting Network From Executive" 
        << std::endl << vprDEBUG_FLUSH;
    GetNetwork();

    std::map< std::string, std::string >::iterator iter;
    // Remove any plugins that aren't present in the current network
    for( std::map< std::string, ves::xplorer::plugin::PluginBase* >::iterator 
        foundPlugin = mPluginsMap.begin(); foundPlugin != mPluginsMap.end(); )
    {
        // When we clear the _plugin map will
        // loop over all plugins
        iter = _id_map.find( foundPlugin->first );
        if( iter == _id_map.end() )
        {
            //Set active model to null so that if the previous active model 
            //is deleted that we don't get errors in our code other places.
            if(  ModelHandler::instance()->GetActiveModel() ==
                foundPlugin->second->GetCFDModel() )
            {
                std::string nullString;
                ModelHandler::instance()->SetActiveModel( nullString );    
            }
            
            // if a module is on the pugins map but not on the id map
            foundPlugin->second->RemoveSelfFromSG();
            ModelHandler::instance()->RemoveModel( foundPlugin->second->GetCFDModel() );
            // Remove a plugins event handler map
            // do this before the foundPlugin is deleted
            std::map< std::string, std::map< std::string, PluginBase* > >::iterator cmdIter;
            cmdIter = pluginEHMap.find( foundPlugin->first );
            pluginEHMap.erase( cmdIter );
            // Must delete current instance of vebaseclass object
            delete mPluginsMap[ foundPlugin->first ];
            mPluginsMap.erase( foundPlugin++ );
        }
        else
        {
            // plugin already present...
            vprDEBUG( vesDBG, 1 ) << "|\t\tPlugin [ " << iter->first
                << " ]-> " << iter->second
                << " is already on the plugin and id map."
                << std::endl << vprDEBUG_FLUSH;
            ++foundPlugin;
        }
        // The above code is from : The C++ Standard Library by:Josuttis pg. 205
    }
    ves::xplorer::communication::CommunicationHandler::instance()
        ->SendConductorMessage( "Finished loading data in VE-Xplorer." );
    vprDEBUG( vesDBG, 0 ) << "|\t\tDone Getting Network From Executive"
        << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::InitModules()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::PreFrameUpdate()
{
    vprDEBUG( vesDBG, 3 ) << "|\tGraphicalPluginManager::PreFrameUpdate"
        << std::endl << vprDEBUG_FLUSH;
    if( !ui_i )
    {
        ConnectToCE();
        return;
    }

    //process the current command form the gui
    const CommandPtr tempCommand = CommandManager::instance()->GetXMLCommand();
    if( tempCommand )
    {
        std::map< std::string, ves::xplorer::event::EventHandler* >::iterator
        currentEventHandler;
        currentEventHandler = _eventHandlers.find( tempCommand->GetCommandName() );
        if( currentEventHandler != _eventHandlers.end() )
        {
            vprDEBUG( vesDBG, 0 ) << "|\t\tExecuting: " << tempCommand->GetCommandName()
                << std::endl << vprDEBUG_FLUSH;
            currentEventHandler->second->SetGlobalBaseObject();
            currentEventHandler->second->Execute( tempCommand );
        }
    }

    ///Load the data from ce
    const std::string tempNetworkCommand = ui_i->GetStatusString();
    bool updatePluginResults = false;
    if( tempNetworkCommand.compare( 0, 35, "VE-Suite Network Execution Complete" ) == 0 )
    {
        std::cout << "|\tLoading data into plugins" << std::endl;
        //LoadDataFromCE();
        updatePluginResults = true;
    }

    ///process the standard plugin stuff
    for( std::map< std::string, PluginBase* >::const_iterator 
        foundPlugin = mPluginsMap.begin(); foundPlugin != mPluginsMap.end();
        ++foundPlugin )
    {
        //1. Set the current command on all plugins
        /*if ( ModelHandler::instance()->GetActiveModel() )
        {
           CommandPtr tempCommand = 
                    ModelHandler::instance()->GetActiveModel()->GetVECommand();
           foundPlugin->second->SetCurrentCommand( tempCommand );
        }*/
        //2. if active model is the plugin's model...
        if( ModelHandler::instance()->GetActiveModel() &&
                ( ModelHandler::instance()->GetActiveModel() ==
                  foundPlugin->second->GetCFDModel() )
           )
        {
            //Process a special plugin command
            if( tempCommand )
            {
                const std::string cmdName = tempCommand->GetCommandName();
                PluginBase* const tempBase = pluginEHMap[ foundPlugin->first ][ cmdName ];
                if( tempBase )
                {
                    tempBase->SetCurrentCommand( tempCommand );
                }
            }
            //Update the draw function
            //only if you are selected
            foundPlugin->second->SelectedPreFrameUpdate();
        }
        //3. Call this for all plugins every frame
        foundPlugin->second->PreFrameUpdate();
        //4. Run results function for all plugins
        if( updatePluginResults )
        {
            int dummyVar = 0;
            foundPlugin->second->CreateCustomVizFeature( dummyVar );
        }
    }
    vprDEBUG( vesDBG, 3 ) << "|\tGraphicalPluginManager::PreFrameUpdate End"
        << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::PostFrameUpdate()
{
    vprDEBUG( vesDBG, 3 ) << "|\tGraphicalPluginManager::PostFrameUpdate"
        << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::LoadDataFromCE()
{
    if( CORBA::is_nil( this->_exec ) )
    {
        std::cerr 
            << "|\tTried to load data from VE-CE but there is no connection." 
            << std::endl;
        return;
    }

    if( ui_i->GetNetworkFlag() )
    {
#ifdef MINERVA_GIS_SUPPORT
        ves::xplorer::minerva::MinervaManager::instance()->ClearModels();
        //If we have an earth we need to clear it when we load a new model
        ves::xplorer::minerva::MinervaManager::instance()->Clear();
#endif
        // Get Network and parse it
        GetEverything();
        ves::xplorer::scenegraph::SceneManager::instance()->ViewLogo( false );
    }
    return;
    // store the statusString in order to perform multiple operations on it...
    //std::string statusString = ui_i->GetStatusString();
    //vprDEBUG(vesDBG,3) << "|\tGraphicalPluginManager::PreFrameUpdate statusString = " << statusString
    //   << std::endl << vprDEBUG_FLUSH;

    // record position of some key phrases...
    //size_t pos1 = statusString.find( "VES Network Execution Complete" );

    //*******************************************************************//
    //For multiple model apps this implementation has to be changed      //
    //We need to be able to grab the id of of the currently completed    //
    //model and then in the loop below rather than iterating across      //
    //all plugins set the result and activate custom viz for the         //
    //current model id                                                   //
    //*******************************************************************//
    //unsigned int pos2 = statusString.find("Execution is done");

    //size_t pos3 = statusString.find("Time Step Complete");

    // If either of the positions are valid positions,
    // then make results available to the graphical plugins...
    //if ( pos1 != std::string::npos ||
    //     pos3 != std::string::npos )
    {
        //std::map< int, std::string >::iterator idMap;
        for( std::map< std::string, PluginBase* >::iterator foundPlugin =
                    mPluginsMap.begin();
                foundPlugin != mPluginsMap.end();
                foundPlugin++ )
        {
            //idMap = _id_map.find( foundPlugin->first );
            //No need to call this function when execution is complete because it
            //is called in the get network call
            /*if( pos3 != std::string::npos )
            {
                Command returnState;
                returnState.SetCommandName("Get XML Model Results");
                
                DataValuePairPtr data(  new DataValuePair() );      
                data->SetData("moduleName", idMap->second );
                returnState.AddDataValuePair( data );
                
                data = new DataValuePair();
                data->SetData("vendorUnit", 
                      idToModel[ foundPlugin->first ]->GetVendorName() );
                returnState.AddDataValuePair( data );
                
                data = new DataValuePair();
                data->SetData("moduleId", 
                      static_cast< unsigned int >( idMap->first ) );
                returnState.AddDataValuePair( data );
                
                std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
                nodes.push_back( std::pair< XMLObjectPtr, 
                      std::string >( &returnState, "vecommand" ) );
                XMLReaderWriter commandWriter;
                std::string status="returnString";
                commandWriter.UseStandaloneDOMDocumentManager();
                commandWriter.WriteXMLDocument( nodes, status, "Command" );
                const char* tempResult = this->_exec->Query( status.c_str() );
                std::string tempResultString = tempResult;
                _plugins[ foundPlugin->first ]->
                    SetModuleResults( tempResultString );
                delete tempResult;
            }*/

            int dummyVar = 0;
            mPluginsMap[ foundPlugin->first ]->CreateCustomVizFeature( dummyVar );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
/*bool GraphicalPluginManager::RegisterEHForGEPlugin( std::string commandName, PluginBase* baseClass )
{
   std::map< std::string, PluginBase* >::iterator iter;
   iter = pluginEHMap.find( commandName );
   if(iter == pluginEHMap.end() )
   {
      std::cerr << "RegisterEHForGEPlugin : Command already registered with another plugin" << std::endl;
      return false;
   }

   pluginEHMap[ commandName ] = baseClass;
   return true;
}*/
////////////////////////////////////////////////////////////////////////////////
cfdVEAvailModules* GraphicalPluginManager::GetAvailablePlugins()
{
    return mAvailableModules;
}
////////////////////////////////////////////////////////////////////////////////
VE_i* GraphicalPluginManager::GetCORBAInterface()
{
    return ui_i;
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::ConnectToCE()
{
    try
    {
        std::cout << "|\tTrying to register " << m_UINAME << std::endl;

        CosNaming::Name name( 1 );
        name.length( 1 );
        name[0].id = CORBA::string_dup( "Executive" );
        
        CORBA::Object_var exec_object = this->naming_context->resolve( name );
        _exec = Body::Executive::_narrow( exec_object.in() );
        
        //Create the Servant
        ui_i = new VE_i( _exec, m_UINAME );
        
        PortableServer::ObjectId_var id =
            PortableServer::string_to_ObjectId( "GraphicalPluginManager" );
        
        //activate it with this child POA
        m_ChildPOA->activate_object_with_id( id.in(), &( *ui_i ) );
        
        // obtain the object reference
        CORBA::Object_var objectRef = m_ChildPOA->id_to_reference( id.in() );
        Body::UI_var unit = Body::UI::_narrow( objectRef.in() );
        
        // Don't register it to the naming service anymore
        // the bind call will hang if you try to register
        // Instead, the new idl make the ref part of the register call
        // Naming Service now is only used for boot trap
        // to get the ref for Executive
        
        //Call the Executive CORBA call to register it to the Executive
        std::cout << "|\tRegistering " << m_UINAME << std::endl;
        _exec->RegisterUI( ui_i->UIName_.c_str(), unit.in() );
        std::cout << "|\tConnected to the Executive " << std::endl;
    }
    catch( CORBA::Exception& ex )
    {
        std::cerr << "|\tExecutive not present or VEClient registration error"
            << ex._info().c_str() << std::endl;
        ui_i = 0;
    }
}    
////////////////////////////////////////////////////////////////////////////////
std::string GraphicalPluginManager::GetCurrentNetwork()
{
    return veNetwork;
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView* GraphicalPluginManager::GetNetworkSystemView()
{
    return netSystemView;
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::DeleteNetworkSystemView()
{
    if(netSystemView)
    {
        delete netSystemView;
        netSystemView = NULL;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPluginManager::ParseSystem( ves::open::xml::model::SystemPtr system, 
    bool getResults, osg::Group* parentNode )
{
    //add the system to the map
    mIDToSystem[ system->GetID() ] = system;
    
    std::map< std::string, ves::xplorer::plugin::PluginBase* >::iterator
        foundPlugin;
    //Parse out the subsystems
    std::vector< model::ModelPtr > tempModels = system->GetModels();
    size_t modelCount = system->GetNumberOfModels();
    for( size_t i = 0; i < modelCount; ++i )
    {
        //Initialize model maps
        model::ModelPtr model = tempModels.at( i );
        std::string modelID = model->GetID();
        _id_map[ modelID ] = model->GetPluginType();

        //flag for parent models to not have submodel results loaded
        bool parentResultsFailed = !getResults;
        //Get lsit of models for this particular system
        //now do everything on a per model basis
        foundPlugin = mPluginsMap.find( modelID );
        if( foundPlugin == mPluginsMap.end() )
        {
            // if a new module is on the id map but not on the plugins map
            // create it...
            PluginBase* temp = 
                static_cast< ves::xplorer::plugin::PluginBase* >( 
                mAvailableModules->GetLoader()->CreateObject( 
                model->GetPluginType() ) );

            if( temp == 0 )
            {
                //load the default plugin
                temp = static_cast< ves::xplorer::plugin::PluginBase* >( 
                    mAvailableModules->GetLoader()->
                    CreateObject( "DefaultPlugin" ) );
            }

            mPluginsMap[ modelID ] = temp;
            // When we create the _plugin map here we will do the following
            temp->SetPhysicsSimulator( ves::xplorer::scenegraph::
                PhysicsSimulator::instance() );
#ifdef VE_SOUND
            temp->SetSoundManager( osgAudio::SoundManager::instance() );
#endif
            temp->SetEnvironmentHandler( EnvironmentHandler::instance() );
            temp->SetModelHandler( ves::xplorer::ModelHandler::instance() );
            temp->SetSceneManager( ves::xplorer::scenegraph::SceneManager::instance() );
            temp->SetResourceManager( ves::xplorer::scenegraph::ResourceManager::instance() );
            temp->SetCommunicationHandler( ves::xplorer::communication::CommunicationHandler::instance() );
            temp->SetInteractionDevice( DeviceHandler::instance()->GetDevice(
                device::Device::KEYBOARD_MOUSE ) );
            
            temp->InitializeNode( parentNode );
            temp->AddSelfToSG();
            Model* tempCFDModel = temp->GetCFDModel();
            tempCFDModel->SetID( modelID );
            ModelHandler::instance()->AddModel( tempCFDModel );
            //Need to pass an active device in here or something
            //This needs to be fixed
            //mPluginsMap[ iter->first ]->SetNavigate( 
                //EnvironmentHandler::instance()->GetNavigate() );
            pluginEHMap[ modelID ] = temp->GetCommandNameMap();
        }
        // this call always returns something because it is up to 
        // date with the id map
        ves::xplorer::plugin::PluginBase* newPlugin = mPluginsMap[ modelID ];
        newPlugin->SetXMLModel( model );
        //send command to get results
        if( getResults )
        {
            std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
            XMLReaderWriter commandWriter;
            CommandPtr returnState(  new Command() );
            returnState->SetCommandName( "Get XML Model Results" );
            
            DataValuePairPtr data(  new DataValuePair() );
            data->SetData( "vendorUnit", model->GetVendorName() );
            returnState->AddDataValuePair( data );
            
            data = DataValuePairPtr( new DataValuePair() );
            data->SetData( "moduleName", model->GetPluginType() );
            returnState->AddDataValuePair( data );
            
            //This needs to pass in the uuid not the fake id
            data = DataValuePairPtr( new DataValuePair() );
            data->SetData( "moduleId", static_cast< unsigned int >(
                model->GetModelID() ) );
            returnState->AddDataValuePair( data );
            
            std::string status = "returnString";
            nodes.push_back( std::pair< XMLObjectPtr, std::string >(
                            returnState, "vecommand" ) );
            commandWriter.UseStandaloneDOMDocumentManager();
            commandWriter.WriteXMLDocument( nodes, status, "Command" );
            nodes.clear();
            //Get results
            try
            {
                const char* tempResult = _exec->Query( status.c_str() );
                std::string resultData = tempResult;
                newPlugin->SetModuleResults( resultData );
                if( resultData.empty() || resultData == "NULL" )
                {
                    parentResultsFailed = true;
                }
                if( tempResult )
                {
                    delete tempResult;
                }
            }
            catch( CORBA::Exception& ex )
            {
                std::cerr << "|\tExecutive Query error"
                    << ex._info().c_str() << std::endl;
            }
        }

        newPlugin->ProcessOnSubmitJob();
        newPlugin->PreFrameUpdate();
        newPlugin->CreateCustomVizFeature( 0 );
        vprDEBUG( vesDBG, 1 ) << "|\t\tPlugin [ " << modelID
            << " ]-> " << newPlugin << " is updated."
            << std::endl << vprDEBUG_FLUSH;

        //Now lets find systems
        if( system->GetModel( i )->GetSubSystem() )
        {
            ParseSystem( system->GetModel( i )->GetSubSystem(),
                !parentResultsFailed, newPlugin->GetPluginDCS() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
