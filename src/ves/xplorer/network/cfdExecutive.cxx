/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/xplorer/network/VE_i.h>
//#include <ves/xplorer/XplorerHandlers/cfdEnum.h>
#include <ves/xplorer/cfdCommandArray.h>
#include <ves/xplorer/network/cfdVEAvailModules.h>
#include <ves/xplorer/network/UpdateNetworkEventHandler.h>
#include <ves/xplorer/plugin/cfdVEBaseClass.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/cfdEnvironmentHandler.h>
#include <ves/xplorer/cfdThread.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/DefaultGraphicalPlugin/DefaultGraphicalPlugin.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/ModelWeakPtr.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/SystemWeakPtr.h>
#include <ves/open/xml/model/SystemStrongPtr.h>
#include <ves/open/xml/model/System.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/network/DeleteObjectFromNetworkEventHandler.h>
#include <ves/xplorer/network/SwitchXplorerViewEventHandler.h>
#include <ves/xplorer/network/ReloadPluginsEventHandler.h>

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

#include <vpr/System.h>
#include <vpr/Util/GUID.h>
#include <orbsvcs/CosNamingC.h>

#include <xercesc/dom/DOM.hpp>
XERCES_CPP_NAMESPACE_USE

#include <ves/xplorer/network/cfdExecutive.h>
#include <ves/xplorer/cfdDebug.h>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

vprSingletonImpLifetime( VE_Xplorer::cfdExecutive, 15 );

void cfdExecutive::Initialize( CosNaming::NamingContext* inputNameContext,
                            PortableServer::POA* child_poa )
{
   this->naming_context = inputNameContext;
   try
   {
      XMLPlatformUtils::Initialize();
   }
   catch(const XMLException &toCatch)
   {
      std::cerr << "Error during Xerces-c Initialization.\n"
            << "  Exception message:"
            << XMLString::transcode(toCatch.getMessage()) << std::endl;
      return;
   }

   this->_doneWithCalculations = true;
   this->runGetEverythingThread = true;
   //this->updateNetworkString = false;
   //this->thread = 0;

   //this->naming_context = CosNaming::NamingContext::_duplicate( 
   //   corbaManager->_vjObs->GetCosNaming()->naming_context );
   this->_masterNode = new VE_SceneGraph::Group();
   this->_masterNode->SetName( "cfdExecutive_Node" );
   VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->AddChild( this->_masterNode.get() );

   m_avModules = new cfdVEAvailModules();

   std::ostringstream dirStringStream;
   dirStringStream << "VEClient-" << vpr::System::getHostname() 
                     << "-" <<  vpr::GUID( vpr::GUID::generateTag ).toString();
   std::string UINAME = dirStringStream.str();

   _exec = NULL;
   ui_i = 0;

   try
   {
      CosNaming::Name name(1);
      name.length(1);
      name[0].id = CORBA::string_dup ("Executive");
    
      CORBA::Object_var exec_object = this->naming_context->resolve(name);
      _exec = Body::Executive::_narrow(exec_object.in());

      //Create the Servant
      ui_i = new Body_UI_i(_exec, UINAME);
      //Body_UI_i ui_i( UINAME);

      PortableServer::ObjectId_var id = 
         PortableServer::string_to_ObjectId( "cfdExecutive" ); 
    
      //activate it with this child POA 
      child_poa->activate_object_with_id( id.in(), &(*ui_i) );

      // obtain the object reference
      CORBA::Object_var objectRef = child_poa->id_to_reference( id.in() );
      Body::UI_var unit = Body::UI::_narrow( objectRef.in() );

      // Don't register it to the naming service anymore
      // the bind call will hang if you try to register
      // Instead, the new idl make the ref part of the register call 
      // Naming Service now is only used for boot trap 
      // to get the ref for Executive

      //Call the Executive CORBA call to register it to the Executive
      std::cout << "|\tRegistering " << UINAME << std::endl;
      _exec->RegisterUI( ui_i->UIName_.c_str(), unit.in() );
      std::cout << "|\tConnected to the Executive " << std::endl;
      ui_i->GetNetworkFromCE();
   } 
   catch ( CORBA::Exception& ) 
   {      
      std::cerr << "|\tExecutive not present or VEClient registration error"
                << std::endl;
   }

   _eventHandlers[std::string("DELETE_OBJECT_FROM_NETWORK")] = new VE_EVENTS::DeleteObjectFromNetworkEventHandler();
   _eventHandlers[std::string("CHANGE_XPLORER_VIEW")] = new VE_EVENTS::SwitchXplorerViewEventHandler();
   _eventHandlers[std::string("Plugin_Control")] = new VE_EVENTS::ReloadPluginsEventHandler();
   _eventHandlers[std::string("veNetwork Update")] = new VE_EVENTS::UpdateNetworkEventHandler();
}
////////////////////////////////////////////////////////////////////////////////
std::map<int, cfdVEBaseClass* >* cfdExecutive::GetTheCurrentPlugins( void )
{
   return &_plugins;
}
////////////////////////////////////////////////////////////////////////////////
cfdExecutive::~cfdExecutive( void )
{
   // vprDEBUG(vesDBG,2) << "|\tExecutive Destructor " 
   //                     << std::endl << vprDEBUG_FLUSH;
   this->runGetEverythingThread = false;
   delete m_avModules;

   _plugins.clear();
   _id_map.clear();
   idToModel.clear();
   pluginEHMap.clear();
   _eventHandlers.clear();
   
   try
   {
      if(ui_i)
      {
         _exec->UnRegisterUI( ui_i->UIName_.c_str() );
      }
   }
   catch( CORBA::Exception& )
   {
      std::cerr << "|\tDisconnect from VE_CE failed!" << std::endl;
   }

   delete ui_i;
   ui_i = 0;
   //vprDEBUG(vesDBG,2) << "|\tEnd Executive Destructor " 
   //    << std::endl << vprDEBUG_FLUSH;
}
///////////////////////////////////////////////////////////////////
void cfdExecutive::UnbindORB()
{
    if( !ui_i )
    {
        return;
    }

    CosNaming::Name UIname(1);
    UIname.length(1);
    UIname[0].id = CORBA::string_dup((ui_i->UIName_).c_str());
    vprDEBUG(vesDBG,2) << "|\tExecutive Destructor " 
        << UIname[0].id << std::endl << vprDEBUG_FLUSH;

    try
    {
        this->naming_context->unbind(UIname);
    }
    catch ( CosNaming::NamingContext::InvalidName& ex )
    {
        vprDEBUG(vesDBG,1) << "|\t\tcfdExecutive : Invalid Name! " 
            << ex._info().c_str() 
            << std::endl << vprDEBUG_FLUSH;
    }
    catch ( CosNaming::NamingContext::NotFound& ex )
    {
        vprDEBUG(vesDBG,1) << "|\t\tcfdExecutive : Not Found! " 
            << ex._info().c_str() 
            << std::endl << vprDEBUG_FLUSH;
    }
    catch ( CosNaming::NamingContext::CannotProceed& ex )
    {
        vprDEBUG(vesDBG,1) << "|\t\tcfdExecutive : Cannot Proceed! " 
            << ex._info().c_str() 
            << std::endl << vprDEBUG_FLUSH;
    }
}
///////////////////////////////////////////////////////////////////
void cfdExecutive::GetNetwork( void )
{
   // Get buffer value from Body_UI implementation
   std::string temp( ui_i->GetNetworkString() );
   //std::ofstream output("xplorerNetwork.txt");
   //output<<temp<<std::endl;
   //output.close();
   const std::string network = temp;
   vprDEBUG(vesDBG,0) << "|\t\tNetwork String : " << network 
        << std::endl << vprDEBUG_FLUSH;

   // Load from the nt file loaded through wx
   // Get a list of all the command elements   
   VE_XML::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();

   // we can do this because the plugins will actually 
   // manage the memory for these models. When a plugin is deleted the 
   // plugin will be responsible for deleting the veModel pointer
   // This logic also works for the case where a custom plugin doesn't exist because
   // there will be a default plugin that will be created just like there
   // is currently for conductor
   std::vector< VE_XML::XMLObject* > currentModels;
   currentModels.clear();
   // do this for models
   networkWriter.ReadXMLData( network, "System", "veSystem" );
   currentModels = networkWriter.GetLoadedXMLObjects();
   VE_XML::VE_Model::SystemWeakPtr tempSystem = 
       dynamic_cast< VE_XML::VE_Model::System* >( currentModels.at( 0 ) );

	veNetwork = network;

   // now lets create a list of them
   _id_map.clear();
   idToModel.clear();
   std::vector< VE_XML::VE_Model::ModelWeakPtr > tempModels;
   tempModels = tempSystem->GetModels();
   for ( size_t i = 0; i < tempModels.size(); ++i )
   {
      VE_XML::VE_Model::ModelWeakPtr model = tempModels.at( i );
      _id_map[  model->GetModelID() ] = model->GetModelName();
      idToModel[ model->GetModelID() ] = model;
   }
}
///////////////////////////////////////////////////////////////////
void cfdExecutive::GetEverything( void )
{
    if( CORBA::is_nil( this->_exec ) )
    {
        vprDEBUG(vesDBG,3) << "ERROR : The Executive has not been intialized!"
            << std::endl << vprDEBUG_FLUSH;
        return;
    }
   
   vprDEBUG(vesDBG,0) << "|\t\tGetting Network From Executive" << std::endl << vprDEBUG_FLUSH;      
   GetNetwork();

   std::map< int, std::string >::iterator iter;
   std::map< int, cfdVEBaseClass* >::iterator foundPlugin;
   // Add any plugins that are present in the current network
   for ( iter=_id_map.begin(); iter!=_id_map.end(); iter++ )
   {
      foundPlugin = _plugins.find( iter->first );
      if ( (foundPlugin == _plugins.end()) || _plugins.empty() )
      {
         // if a new module is on the id map but not on the plugins map
         // create it...
         cfdVEBaseClass* temp = dynamic_cast< cfdVEBaseClass* >( m_avModules->GetLoader()->CreateObject( iter->second ) );
         if ( temp == 0 )
         {
            //load the default plugin
            temp = new DefaultGraphicalPlugin();
            //dynamic_cast< cfdVEBaseClass* >( av_modules->GetLoader()->CreateObject( "DefaultGraphicalPlugin" ) );
         }

         _plugins[ iter->first ] = temp;
         // When we create the _plugin map here we will do the following
         _plugins[ iter->first ]->InitializeNode( VE_SceneGraph::SceneManager::instance()->GetWorldDCS() );
         _plugins[ iter->first ]->AddSelfToSG();
         cfdModel* tempCFDModel = _plugins[ iter->first ]->GetCFDModel();
         tempCFDModel->SetID( iter->first );
         cfdModelHandler::instance()->AddModel( tempCFDModel );
         // Give graphical plugins access to wand position, wand buttons, and gui variables
         _plugins[ iter->first ]->SetCursor( cfdEnvironmentHandler::instance()->GetCursor() );
         //Need to pass an active device in here or something
         //This needs to be fixed
         //_plugins[ iter->first ]->SetNavigate( cfdEnvironmentHandler::instance()->GetNavigate() );
         //This is now handled by the active model and eventhandlers rather than cfdSoundHandler
         //_plugins[ iter->first ]->SetSoundHandler( cfdEnvironmentHandler::instance()->GetSoundHandler() );
         pluginEHMap[ iter->first ] = _plugins[ iter->first ]->GetCommandNameMap();
      }
      std::map< int, VE_XML::VE_Model::ModelStrongPtr >::iterator modelIter;
      // this call always returns something because it is up to date with the id map
      modelIter = idToModel.find( iter->first );
      _plugins[ iter->first ]->SetXMLModel( modelIter->second );
      //send command to get results
      VE_XML::Command returnState;
      returnState.SetCommandName("Get XML Model Results");
      
      VE_XML::DataValuePairWeakPtr data = new VE_XML::DataValuePair();
      data->SetData("vendorUnit", modelIter->second->GetVendorName() );
      returnState.AddDataValuePair( data );      
      
      data= new VE_XML::DataValuePair();
      data->SetData("moduleName", iter->second );
      returnState.AddDataValuePair( data );   
        
      data= new VE_XML::DataValuePair();
      data->SetData("moduleId", static_cast< unsigned int >( iter->first ) );
      returnState.AddDataValuePair( data );      
      
      std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
      nodes.push_back( 
                       std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ) 
                       );
      VE_XML::XMLReaderWriter commandWriter;
      std::string status="returnString";
      commandWriter.UseStandaloneDOMDocumentManager();
      commandWriter.WriteXMLDocument( nodes, status, "Command" );
      //Get results 
      const char* tempResult = this->_exec->Query( status.c_str() );
      std::string resultData = tempResult;
      _plugins[ iter->first ]->SetModuleResults( resultData );
      delete tempResult;
      _plugins[ iter->first ]->ProcessOnSubmitJob();
      _plugins[ iter->first ]->PreFrameUpdate();
      vprDEBUG(vesDBG,1) << "|\t\tPlugin [ " << iter->first 
                           << " ]-> " << iter->second 
                           << " is updated."
                           << std::endl << vprDEBUG_FLUSH;
   }
           
   // Remove any plugins that aren't present in the current network
   for ( foundPlugin=_plugins.begin(); foundPlugin!=_plugins.end(); )
   {  
      // When we clear the _plugin map will
      // loop over all plugins
      iter = _id_map.find( foundPlugin->first );
      if ( iter == _id_map.end() )
      {
         // if a module is on the pugins map but not on the id map
         foundPlugin->second->RemoveSelfFromSG();
         cfdModelHandler::instance()->RemoveModel( foundPlugin->second->GetCFDModel() );
         // Remove a plugins event handler map
         // do this before the foundPlugin is deleted
         std::map< int, std::map< std::string, cfdVEBaseClass* > >::iterator cmdIter;
         cmdIter = pluginEHMap.find( foundPlugin->first );
         pluginEHMap.erase( cmdIter );
         // Must delete current instance of vebaseclass object
         delete _plugins[ foundPlugin->first ];
         _plugins.erase( foundPlugin++ );
      }
      else
      {
         // plugin already present...
         vprDEBUG(vesDBG,1) << "|\t\tPlugin [ " << iter->first 
                                 << " ]-> " << iter->second 
                                 << " is already on the plugin and id map."
                                 << std::endl << vprDEBUG_FLUSH;
         ++foundPlugin;
      }
      // The above code is from : The C++ Standard Library by:Josuttis pg. 205
   }
   //Set active model to null so that if the previous active model is deleted
   //that we don't get errors in our code other places.
   cfdModelHandler::instance()->SetActiveModel( 0 );
   vprDEBUG(vesDBG,0) << "|\t\tDone Getting Network From Executive"
                          << std::endl << vprDEBUG_FLUSH;    
}
///////////////////////////////////////////////////////////////////
void cfdExecutive::InitModules( void )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void cfdExecutive::PreFrameUpdate( void )
{
   vprDEBUG(vesDBG,3) << "|\tcfdExecutive::PreFrameUpdate"
                          << std::endl << vprDEBUG_FLUSH;

   //process the current command form the gui
   //if ( cfdModelHandler::instance()->GetActiveModel() )
   {
      if( cfdModelHandler::instance()->GetXMLCommand()->GetCommandName().compare("wait") )
      {
         std::map<std::string,VE_EVENTS::EventHandler*>::iterator currentEventHandler;
         VE_XML::Command* tempCommand = cfdModelHandler::instance()->GetXMLCommand();
         currentEventHandler = _eventHandlers.find( tempCommand->GetCommandName() );
         if ( currentEventHandler != _eventHandlers.end() )
         {
            vprDEBUG(vesDBG,0) << "|\t\tExecuting: "<< tempCommand->GetCommandName() 
                                 << std::endl << vprDEBUG_FLUSH;
            currentEventHandler->second->SetGlobalBaseObject();
            currentEventHandler->second->Execute( tempCommand );
         }
      }
   }

   ///Load the data from ce
   //LoadDataFromCE();

   ///process the standard plugin stuff
   std::map< int, cfdVEBaseClass* >::iterator foundPlugin;
   for ( foundPlugin = _plugins.begin(); 
         foundPlugin != _plugins.end(); 
         ++foundPlugin )
   {
      //1. Set the current command on all plugins
      /*if ( cfdModelHandler::instance()->GetActiveModel() )
      {
         VE_XML::Command* tempCommand = 
                  cfdModelHandler::instance()->GetActiveModel()->GetVECommand();
         foundPlugin->second->SetCurrentCommand( tempCommand );
      }*/
      //2. if active model is the plugin's model...
      if ( cfdModelHandler::instance()->GetActiveModel() &&
           ( cfdModelHandler::instance()->GetActiveModel() == 
            foundPlugin->second->GetCFDModel() )
         )
      {  
         //Process a special plugin command
         VE_XML::Command* tempCommand = 
         cfdModelHandler::instance()->GetXMLCommand();
         //if( tempCommand )
         {
            std::string cmdName = tempCommand->GetCommandName();
            cfdVEBaseClass* tempBase = pluginEHMap[ foundPlugin->first ][ cmdName ];
            if ( tempBase )
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
   }
}
////////////////////////////////////////////////////////////////////////////////
void cfdExecutive::PostFrameUpdate( void )
{
   vprDEBUG(vesDBG,3) << "|\tcfdExecutive::PostFrameUpdate"
                        << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void cfdExecutive::LoadDataFromCE( void )
{
   if ( CORBA::is_nil( this->_exec ) )
   {
      return;
   }
   
   if ( ui_i->GetNetworkFlag() )
   {
      // Get Network and parse it
      GetEverything();
      VE_SceneGraph::SceneManager::instance()->ViewLogo(false);
   }
   
   // store the statusString in order to perform multiple operations on it...
   std::string statusString = ui_i->GetStatusString();
   vprDEBUG(vesDBG,3) << "|\tcfdExecutive::PreFrameUpdate statusString = " << statusString 
      << std::endl << vprDEBUG_FLUSH;
   
   // record position of some key phrases...
   size_t pos1 = statusString.find( "VES Network Execution Complete" );
   
   //*******************************************************************//
   //For multiple model apps this implementation has to be changed      //
   //We need to be able to grab the id of of the currently completed    //
   //model and then in the loop below rather than iterating across      //
   //all plugins set the result and activate custom viz for the         //
   //current model id                                                   //
   //*******************************************************************//
   //unsigned int pos2 = statusString.find("Execution is done");
   
   size_t pos3 = statusString.find("Time Step Complete");
   
   // If either of the positions are valid positions, 
   // then make results available to the graphical plugins...
   if ( pos1 != std::string::npos || 
        pos3 != std::string::npos )
   {
      std::map< int, std::string >::iterator idMap;
      for( std::map< int, cfdVEBaseClass* >::iterator foundPlugin = 
           _plugins.begin(); 
           foundPlugin!=_plugins.end(); 
           foundPlugin++ )
      {  
          idMap = _id_map.find( foundPlugin->first );
          //No need to call this function when execution is complete because it
          //is called in the get network call
          /*if( pos3 != std::string::npos )
          {
              VE_XML::Command returnState;
              returnState.SetCommandName("Get XML Model Results");
              
              VE_XML::DataValuePairWeakPtr data = new VE_XML::DataValuePair();      
              data->SetData("moduleName", idMap->second );
              returnState.AddDataValuePair( data );
              
              data = new VE_XML::DataValuePair();
              data->SetData("vendorUnit", 
                    idToModel[ foundPlugin->first ]->GetVendorName() );
              returnState.AddDataValuePair( data );
              
              data = new VE_XML::DataValuePair();
              data->SetData("moduleId", 
                    static_cast< unsigned int >( idMap->first ) );
              returnState.AddDataValuePair( data );
              
              std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
              nodes.push_back( std::pair< VE_XML::XMLObject*, 
                    std::string >( &returnState, "vecommand" ) );
              VE_XML::XMLReaderWriter commandWriter;
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
         _plugins[ foundPlugin->first ]->CreateCustomVizFeature( dummyVar );
      }
   } 
}
////////////////////////////////////////////////////////////////////////////////
/*bool cfdExecutive::RegisterEHForGEPlugin( std::string commandName, cfdVEBaseClass* baseClass )
{
   std::map< std::string, cfdVEBaseClass* >::iterator iter;
   iter = pluginEHMap.find( commandName );
   if ( iter == pluginEHMap.end() )
   {
      std::cerr << "RegisterEHForGEPlugin : Command already registered with another plugin" << std::endl;
      return false;
   }
   
   pluginEHMap[ commandName ] = baseClass;
   return true;
}*/
////////////////////////////////////////////////////////////////////////////////
cfdVEAvailModules* cfdExecutive::GetAvailablePlugins()
{
    return m_avModules;
}
////////////////////////////////////////////////////////////////////////////////
Body_UI_i* cfdExecutive::GetCORBAInterface()
{
    return ui_i;
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdExecutive::GetCurrentNetwork()
{
    return veNetwork;
}
