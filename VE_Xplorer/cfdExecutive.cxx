/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: cfdExecutive.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdExecutive.h"
#include "VE_SceneGraph/cfdDCS.h"

#include "VE_Xplorer/VE_i.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Xplorer/cfdCommandArray.h"
#include "VE_Xplorer/cfdVEAvailModules.h"
#include "VE_Xplorer/cfdVEBaseClass.h"
#include "VE_Xplorer/cfdModelHandler.h"
#include "VE_Xplorer/cfdEnvironmentHandler.h"
#include "VE_Xplorer/cfdThread.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Model/Model.h"

#include "VE_SceneGraph/cfdPfSceneManagement.h"

#include <iostream>
#include <string>
#include <sstream>

#include "VE_Xplorer/cfdDebug.h"

#include <vpr/System.h>

#include <orbsvcs/CosNamingC.h>

#include <xercesc/dom/DOM.hpp>
XERCES_CPP_NAMESPACE_USE

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
vprSingletonImp( VE_Xplorer::cfdExecutive );

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
   this->_masterNode = new VE_SceneGraph::cfdGroup();
   this->_masterNode->SetName( "cfdExecutive_Node" );
   VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->AddChild( this->_masterNode );

   av_modules = new cfdVEAvail_Modules();

   //time_t* timeVal;
   long timeID = (long)time( NULL );
   std::ostringstream dirStringStream;
   dirStringStream << "VEClient" << timeID;
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
         PortableServer::string_to_ObjectId( CORBA::string_dup( "cfdExecutive" ) ); 
    
      //activate it with this child POA 
      child_poa->activate_object_with_id( id.in(), &(*ui_i) );

      // obtain the object reference
      Body::UI_var unit =  
      Body::UI::_narrow( child_poa->id_to_reference( id.in() ) );

      // Don't register it to the naming service anymore
      // the bind call will hang if you try to register
      // Instead, the new idl make the ref part of the register call 
      // Naming Service now is only used for boot trap 
      // to get the ref for Executive

      //Call the Executive CORBA call to register it to the Executive
      _exec->RegisterUI( ui_i->UIName_.c_str(), unit.in() );
      std::cout << "|\tConnected to the Executive " << std::endl;   
      //this->thread = new cfdThread();
      //thread->new_thread = new vpr::Thread( new vpr::ThreadMemberFunctor< cfdExecutive > ( this, &cfdExecutive::GetEverything ) );
   } 
   catch (CORBA::Exception &) 
   {      
      std::cerr << "|\tExecutive not present or VEClient registration error"
                << std::endl;
   }
}
///////////////////////////////////////////////////////////////////
void cfdExecutive::CleanUp( void )
{
   this->runGetEverythingThread = false;
   delete av_modules;

   try
   {
      _exec->UnRegisterUI( ui_i->UIName_.c_str() );
      delete ui_i;
      ui_i = 0;
   }
   catch( CORBA::Exception& )
   {
      std::cerr << "|\tDisconnect from VE_CE failed!" << std::endl;
   }
}
///////////////////////////////////////////////////////////////////
void cfdExecutive::UnbindORB()
{
   if ( ui_i )
   {
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
         vprDEBUG(vesDBG,1) << "|\tcfdExecutive : Invalid Name! " 
                                 << std::endl << vprDEBUG_FLUSH;
      }
      catch ( CosNaming::NamingContext::NotFound& ex )
      {
         vprDEBUG(vesDBG,1) << "|\tcfdExecutive : Not Found! " 
                                 << std::endl << vprDEBUG_FLUSH;
      }
      catch ( CosNaming::NamingContext::CannotProceed& ex )
      {
         vprDEBUG(vesDBG,1) << "|\tcfdExecutive : Cannot Proceed! " 
                                 << std::endl << vprDEBUG_FLUSH;
      }
   }
}
///////////////////////////////////////////////////////////////////
void cfdExecutive::GetNetwork( void )
{
   // Get buffer value from Body_UI implementation
   std::string temp( ui_i->GetNetworkString() );
   const std::string network = temp;
   vprDEBUG(vesDBG,2) << "|\tNetwork String : " << network 
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
   currentModels.clear();
   // do this for models
   networkWriter.ReadXMLData( network, "Model", "veModel" );
   currentModels = networkWriter.GetLoadedXMLObjects();

   // now lets create a list of them
   _id_map.clear();
   idToModel.clear();
   for ( size_t i = 0; i < currentModels.size(); ++i )
   {
      VE_Model::Model* model = dynamic_cast< VE_Model::Model* >( currentModels.at( i ) );
      _id_map[  model->GetModelID() ] = model->GetModelName();
      idToModel[ model->GetModelID() ] = model;
   }
}
///////////////////////////////////////////////////////////////////
void cfdExecutive::GetEverything( void )
{
   if ( !CORBA::is_nil( this->_exec) )
   {
      vprDEBUG(vesDBG,0) << "|\tGetting Network From Executive" << std::endl << vprDEBUG_FLUSH;      
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
            cfdVEBaseClass* temp = dynamic_cast< cfdVEBaseClass* >( av_modules->GetLoader()->CreateObject( iter->second ) );
            if ( temp == 0 )
            {
               //load the default plugin
               temp = dynamic_cast< cfdVEBaseClass* >( av_modules->GetLoader()->CreateObject( "DefaultGraphicalPlugin" ) );
            }

            _plugins[ iter->first ] = temp;
            // When we create the _plugin map here we will do the following
            _plugins[ iter->first ]->InitializeNode( VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS() );
            _plugins[ iter->first ]->AddSelfToSG();
            cfdModelHandler::instance()->AddModel( _plugins[ iter->first ]->GetCFDModel() );
            // Give graphical plugins access to wand position, wand buttons, and gui variables
            _plugins[ iter->first ]->SetCursor( cfdEnvironmentHandler::instance()->GetCursor() );
            _plugins[ iter->first ]->SetNavigate( cfdEnvironmentHandler::instance()->GetNavigate() );
            _plugins[ iter->first ]->SetSoundHandler( cfdEnvironmentHandler::instance()->GetSoundHandler() );
         }

         _plugins[ iter->first ]->SetModuleResults( this->_exec->GetModuleResult( iter->first ) );
         vprDEBUG(vesDBG,2) << "|\tModule results: " 
                              << this->_exec->GetModuleResult( iter->first )
                              << std::endl << vprDEBUG_FLUSH;
         std::map< int, VE_Model::Model* >::iterator modelIter;
         // this call always returns something because it is up to date with the id map
         modelIter = idToModel.find( iter->first );
         _plugins[ iter->first ]->SetXMLModel( modelIter->second );
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
      vprDEBUG(vesDBG,0) << "|\tDone Getting Network From Executive"
                             << std::endl << vprDEBUG_FLUSH;    
   }
   else
   {
      vprDEBUG(vesDBG,3) << "ERROR : The Executive has not been intialized!"
                             << std::endl << vprDEBUG_FLUSH;     
   }
}
///////////////////////////////////////////////////////////////////
void cfdExecutive::InitModules( void )
{
   // Initiallize the dashboard
   std::cout << "|  4. Initializing.............................. Dashboard Display |" << std::endl;
   //this->_dashBoard = new cfdDashboard( this->_param->GetParameterFilename(), this->_masterNode );

   // Initiallize all the Digital Text gauges
   std::cout << "|  4. Initializing.............................. Gauges Display |" << std::endl;
   //this->_gauges = new cfdGauges( this->_param->GetParameterFilename(), this->_masterNode );
   
   // Initiallize each piece of geometry   
   std::cout << "|  4. Initializing.............................. Interactive Geometry |" << std::endl;
   //this->_geometry = new cfdInteractiveGeometry( this->_param->GetParameterFilename(), this->_masterNode );
}

void cfdExecutive::PreFrameUpdate( void )
{
   vprDEBUG(vesDBG,3) << " cfdExecutive::PreFrameUpdate"
                          << std::endl << vprDEBUG_FLUSH;

   if ( !CORBA::is_nil( this->_exec ) )
   {
      if ( ui_i->GetNetworkFlag() )
      {
         // Get Network and parse it
         GetEverything();
      }

      // store the statusString in order to perform multiple operations on it...
      std::string statusString = ui_i->GetStatusString();
      vprDEBUG(vesDBG,3) << "cfdExecutive::PreFrameUpdate statusString = " << statusString 
                             << std::endl << vprDEBUG_FLUSH;

      // record position of some key phrases...
      size_t pos1 = statusString.find("Network execution complete");
      
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

      /*if ( pos1 != std::string::npos || 
            pos2 != std::string::npos || 
            pos3 != std::string::npos )*/
	    
      if ( pos1 != std::string::npos || 
            pos3 != std::string::npos )
      {
         std::map< int, cfdVEBaseClass* >::iterator foundPlugin;
         for ( foundPlugin=_plugins.begin(); foundPlugin!=_plugins.end(); foundPlugin++)
         {  
            int dummyVar = 0;
            _plugins[ foundPlugin->first ]->SetModuleResults( this->_exec->GetModuleResult( foundPlugin->first ) );
            _plugins[ foundPlugin->first ]->CreateCustomVizFeature( dummyVar );
         }
      }
      std::map< int, cfdVEBaseClass* >::iterator foundPlugin;
      for ( foundPlugin = _plugins.begin(); foundPlugin != _plugins.end(); ++foundPlugin )
      {
         //if active model is the plugin's model...
         if ( cfdModelHandler::instance()->GetActiveModel() == foundPlugin->second->GetCFDModel() )
         {
            foundPlugin->second->PreFrameUpdate();
         }
      }
   }
}
bool cfdExecutive::CheckCommandId( cfdCommandArray* commandArray )
{
#ifdef _TAO
      if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR )
      {
         //this->SetCalculationsFlag( true );
         return true;
      }
/*      else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == ACT_CUSTOM_VIZ )
      {
         vprDEBUG(vesDBG,1) << " Custom Viz" << std::endl << vprDEBUG_FLUSH;
         std::map< int, cfdVEBaseClass* >::iterator foundPlugin;
         for ( foundPlugin=_plugins.begin(); foundPlugin!=_plugins.end(); foundPlugin++)
         {  
            int dummyVar = 0;
            _plugins[ foundPlugin->first ]->SetModuleResults( this->_exec->GetModuleResult( foundPlugin->first ) );
            _plugins[ foundPlugin->first ]->CreateCustomVizFeature( dummyVar );
         }
         return true;
      }      
*/         
#endif // _TAO
   return false;
}

void cfdExecutive::UpdateCommand()
{
   vprDEBUG(vesDBG,0) << "doing nothing in cfdExecutive::UpdateCommand()"
                          << std::endl << vprDEBUG_FLUSH;
}
