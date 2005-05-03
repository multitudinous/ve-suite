/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
#include "cfdExecutive.h"
#include "VE_i.h"
#include "cfdDCS.h"
#include "cfdEnum.h"
#include "cfdCommandArray.h"
#include "cfdVEAvailModules.h"
#include "cfdVEBaseClass.h"
#include "cfdModelHandler.h"
#include "cfdEnvironmentHandler.h"
#include "cfdThread.h"
#include "cfdPfSceneManagement.h"
#include "package.h"
#include "Network_Exec.h"

#include <iostream>
#include <string>
#include <sstream>

#include <vrj/Util/Debug.h>
#include <vpr/System.h>

#include <orbsvcs/CosNamingC.h>

cfdExecutive::cfdExecutive( CosNaming::NamingContext* inputNameContext, PortableServer::POA* child_poa )
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
   this->_masterNode = new cfdGroup();
   this->_masterNode->SetName( "cfdExecutive_Node" );
   cfdPfSceneManagement::instance()->GetWorldDCS()->AddChild( this->_masterNode );

   av_modules = new cfdVEAvail_Modules();
   _network = new Network();

   //time_t* timeVal;
   long timeID = (long)time( NULL );
   std::ostringstream dirStringStream;
   dirStringStream << "VEClient" << timeID;
   std::string UINAME = dirStringStream.str();
   bool is_orb_init = false;

   _exec = NULL;
   ui_i = 0;
   if (!is_orb_init)
   {
      //init_orb_naming();
      is_orb_init = true;
   }

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
      std::cerr << "|\tExecutive not present or VEClient registration error" << std::endl;
   }
   
   //_param = new cfdExecutiveConfiguration();

   //InitModules();
}

cfdExecutive::~cfdExecutive( void )
{
   this->runGetEverythingThread = false;
   //vpr::System::msleep( 500 );  // half-second delay
   /*if ( thread )
      delete thread;*/
   delete av_modules;
}

void cfdExecutive::UnbindORB()
{
   if ( ui_i )
   {
      CosNaming::Name UIname(1);
      UIname.length(1);
      UIname[0].id = CORBA::string_dup((ui_i->UIName_).c_str());
      std::cout<< " Executive Destructor " << UIname[0].id << std::endl;

      try
      {
         this->naming_context->unbind(UIname);
      }
      catch ( CosNaming::NamingContext::InvalidName& ex )
      {
         std::cout << " cfdExecutive : Invalid Name! " << std::endl;
      }
      catch ( CosNaming::NamingContext::NotFound& ex )
      {
         std::cout << " cfdExecutive : Not Found! " << std::endl;
      }
      catch ( CosNaming::NamingContext::CannotProceed& ex )
      {
         std::cout << " cfdExecutive : Cannot Proceed! " << std::endl;
      }
   }
}

void cfdExecutive::init_orb_naming()
{
   //char *argv[2]={"-ORBInitRef", "NameService=file://ns.ior"};
//   char **argv;
//   argv = new char*[ 0 ];
//   int argc = 0;
/*   try {
      // First initialize the ORB, 
      //orb =
         //CORBA::ORB_init (argc, argv,
         //              ""); // the ORB name, it can be anything! 
        
      //Here is the code to set up the ROOT POA
      CORBA::Object_var poa_object =
         orb->resolve_initial_references ("RootPOA"); // get the root poa
    
      poa = PortableServer::POA::_narrow(poa_object.in());
      PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
      poa_manager->activate ();
   
      //Here is the part to contact the naming service and get the reference for the executive
      CORBA::Object_var naming_context_object =
         orb->resolve_initial_references ("NameService");
      naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in ());
   }  catch (CORBA::Exception &) {
      poa->destroy (1, 1);
      // Finally destroy the ORB
      //orb->destroy();
      std::cerr << "CORBA exception raised!" << std::endl;
   }*/
}

/*void cfdExecutive::GetNetwork ( void )
{
   char *nw_str;
  
   try 
   { 
      nw_str = _exec->GetNetwork();
      //std::cout << "| Network String : " << nw_str << std::endl;
   } 
   catch (CORBA::Exception &) 
   {
      std::cout << "ERROR: cfdExecutive : no exec found! " << std::endl;
   }
  
   char buf[25];
   char *buf2;
  
   unsigned int size; 
   unsigned int netlength = strlen(nw_str);
   unsigned int ipos = 0;
  
   _it_map.clear();
   _name_map.clear();

   // Unpack incoming network string into individual interfaces,
   // and place them into the _it_map and _name_map structures.
  
   while(1) 
   {    
      Interface intf;
      //intf.unpack_ids(&nw_str[ipos]);
    
      ipos+=72;
    
      strncpy(buf, &nw_str[ipos], 24);
      ipos+=24;
      buf[24]='\0';
      size = atoi(buf);
    
      buf2 = new char[size+1];
      strncpy(buf2, &nw_str[ipos], size);
      ipos+=size;
      //intf.unpack(buf2);
      delete [] buf2;
    
      _it_map[intf._id] = intf;
      _name_map[intf.getString("NAME_")] = intf._id;
      //std::cout << " Name : " << intf.getString("NAME_") << std::endl;
      if(ipos>=netlength) break;
   }
  
   delete nw_str;
}
*/
void cfdExecutive::GetNetwork ( void )
{
/*  
   char* network = 0;
   try 
   { 
      network = _exec->GetNetwork();
   } 
   catch (CORBA::Exception &) 
   {
      std::cerr << "ERROR: cfdExecutive : no exec found! " << std::endl;
   }
*/
   // Get buffer value from Body_UI implementation
   std::string temp( ui_i->GetNetworkString() );
   const char* network = temp.c_str();
   vprDEBUG(vprDBG_ALL,2)  << "|\tNetwork String : " << network 
                              << std::endl << vprDEBUG_FLUSH;

/////////////////////////////
// This code taken from Executive_i.cpp
   Package p;
   p.SetSysId("temp.xml");
   p.Load(network, strlen(network));
  
   _network->clear();
   _id_map.clear();

   std::vector<Interface>::iterator iter;
   // Find network layout chunk in network structure
   for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
   {
      if ( iter->_id == -1 ) 
      {
         break;
      }
   }

   if(iter!=p.intfs.end() && _network->parse(&(*iter))) 
   {
      for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
      {
         if(_network->setInput(iter->_id, &(*iter))) 
         {
            _network->module(_network->moduleIdx(iter->_id))->_is_feedback  = iter->getInt("FEEDBACK");
            _network->module(_network->moduleIdx(iter->_id))->_need_execute = 1;
            _network->module(_network->moduleIdx(iter->_id))->_return_state = 0;
         }  
         else
         {
            std::cerr << "|\tUnable to set id# " << iter->_id << "'s inputs" << std::endl;
         }

         if ( iter->_id != -1 ) 
         {
            //std::cout <<  _network->module( _network->moduleIdx(iter->_id) )->get_id() << " : " << _network->module( _network->moduleIdx(iter->_id) )->_name <<std::endl;
            _id_map[ _network->module( _network->moduleIdx(iter->_id) )->get_id() ] = _network->module( _network->moduleIdx(iter->_id) )->_name;
            //std::cout <<  _network->module( _network->moduleIdx(iter->_id) )->get_id() << " : " << _network->module( _network->moduleIdx(iter->_id) )->_name <<std::endl;
            _it_map[ _network->module( _network->moduleIdx(iter->_id) )->get_id() ] = (*iter);
         }
      }
   } 
   else 
   {
      std::cerr << "Either no network present or error in GetNetwork in VE_Xplorer" << std::endl;
   }
///////////////////////////
   /*if ( network )
      delete [] network;*/
}
///////////////////////////////////////////////////////////////////
/*
void cfdExecutive::GetOutput( std::string name )
{
  // NOTHING YET
}
*/
///////////////////////////////////////////////////////////////////

void cfdExecutive::GetPort (std::string name)
{
   char* pt_str = 0;
      
   CORBA::Long mod_id  = (CORBA::Long)_name_map[name];
   CORBA::Long port_id = 0;
  
   try 
   { 
      pt_str = _exec->GetExportData(mod_id, port_id);
   } 
   catch (CORBA::Exception &) 
   {
      std::cerr << "no exec found!" << std::endl;
   }
  
   Interface intf;
  
   //intf.unpack_ids(&pt_str[0]);
   //intf.unpack(&pt_str[96]);
  
   _pt_map[mod_id] = intf;
  
   delete pt_str;
}

///////////////////////////////////////////////////////////////////

void cfdExecutive::GetEverything( void )
{
   //while ( runGetEverythingThread )
   {
      //vpr::System::msleep( 500 );  // half-second delay
   if ( !CORBA::is_nil( this->_exec) )//&& updateNetworkString )
   {
      vprDEBUG(vprDBG_ALL,0) << "|\tGetting Network From Executive" << std::endl << vprDEBUG_FLUSH;      
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
            cfdVEBaseClass* temp = (cfdVEBaseClass*)(av_modules->GetLoader()->CreateObject( (char*)iter->second.c_str() ) );
            if ( temp != NULL )
            {
               _plugins[ iter->first ] = (cfdVEBaseClass*)(av_modules->GetLoader()->CreateObject( (char*)iter->second.c_str() ) );
               // When we create the _plugin map here we will do the following
               _plugins[ iter->first ]->InitializeNode( cfdPfSceneManagement::instance()->GetWorldDCS() );
               _plugins[ iter->first ]->AddSelfToSG();
               cfdModelHandler::instance()->AddModel( _plugins[ iter->first ]->GetCFDModel() );
               _plugins[ iter->first ]->SetCursor( cfdEnvironmentHandler::instance()->GetCursor() );
               _plugins[ iter->first ]->SetNavigate( cfdEnvironmentHandler::instance()->GetNavigate() );
               _plugins[ iter->first ]->SetModuleResults( this->_exec->GetModuleResult( iter->first ) );
               vprDEBUG(vprDBG_ALL,1) << "|\t\tPlugin [ " << iter->first 
                                      << " ]-> " << iter->second 
                                      << " is being created." << std::endl << vprDEBUG_FLUSH;
            }
         }
         else
         {
            // plugin already present...
            vprDEBUG(vprDBG_ALL,1) << "|\t\tPlugin [ " << iter->first 
                                    << " ]-> " << iter->second 
                                    << " is already on the plugin map." << std::endl << vprDEBUG_FLUSH;
         }
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
            vprDEBUG(vprDBG_ALL,1) << "|\t\tPlugin [ " << iter->first 
                                    << " ]-> " << iter->second 
                                    << " is already on the plugin and id map." << std::endl << vprDEBUG_FLUSH;
            ++foundPlugin;
         }
         // The above code is from : The C++ Standard Library by:Josuttis pg. 205
      }
      vprDEBUG(vprDBG_ALL,0) << "|\tDone Getting Network From Executive" << std::endl << vprDEBUG_FLUSH;      
   }
   else
   {
      vprDEBUG(vprDBG_ALL,3) << "ERROR : The Executive has not been intialized or not time to update! " <<  std::endl << vprDEBUG_FLUSH;     
   }
      //updateNetworkString = false;
   }
}

///////////////////////////////////////////////////////////////////
/*
void cfdExecutive::HowToUse( std::string name )
{
   // get a module id from a name
  
   CORBA::Long mod_id = (CORBA::Long)_name_map[name];
  
   // get some input data for module
  
   int x = _it_map[mod_id].getInt("x");
   int y = _it_map[mod_id].getInt("y");
   int z = _it_map[mod_id].getInt("z");  
   // get some port data for module
  
   double temperature = _pt_map[mod_id].getDouble("TEMPERATURE");
   double co          = _pt_map[mod_id].getDouble("CO");
  
   // get output data for module
  
   double efficiency = _ot_map[mod_id].getDouble("EFFICIENCY");
   double cash_flow  = _ot_map[mod_id].getDouble("CASH_FLOW");
}
*/
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

void cfdExecutive::UpdateModules( void )
{
   if ( !CORBA::is_nil( this->_exec ) && ui_i->GetCalcFlag() )
   {
      // Get Network and parse it
      this->GetEverything();
   }
}

void cfdExecutive::PreFrameUpdate( void )
{
   vprDEBUG(vprDBG_ALL,2) << " cfdExecutive::PreFrameUpdate"
                          << std::endl << vprDEBUG_FLUSH;
   std::map< int, cfdVEBaseClass* >::iterator foundPlugin;
   for ( foundPlugin = _plugins.begin(); foundPlugin != _plugins.end(); foundPlugin++)
   {
      //if active model is the plugin's model...
      if ( cfdModelHandler::instance()->GetActiveModel() == foundPlugin->second->GetCFDModel() )
      {
         vprDEBUG(vprDBG_ALL,2) << " active model is the plugin: calling PreFrameUpdate"
                                << std::endl << vprDEBUG_FLUSH;
         foundPlugin->second->PreFrameUpdate();
      }
   }
}

void cfdExecutive::SetCalculationsFlag( bool x )
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   //vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   this->_doneWithCalculations = x;
}

bool cfdExecutive::GetCalculationsFlag( void )
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   //vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   return this->_doneWithCalculations;
}

bool cfdExecutive::CheckCommandId( cfdCommandArray* commandArray )
{
#ifdef _TAO
      if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR )
      {
         this->SetCalculationsFlag( true );
         return true;
      }
      else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == ACT_CUSTOM_VIZ )
      {
         vprDEBUG(vprDBG_ALL,1) << " Custom Viz" << std::endl << vprDEBUG_FLUSH;
         std::map< int, cfdVEBaseClass* >::iterator foundPlugin;
         for ( foundPlugin=_plugins.begin(); foundPlugin!=_plugins.end(); foundPlugin++)
         {  
            int dummyVar = 0;
            _plugins[ foundPlugin->first ]->SetModuleResults( this->_exec->GetModuleResult( foundPlugin->first ) );
            _plugins[ foundPlugin->first ]->CreateCustomVizFeature( dummyVar );
         }
         return true;
      }               
#endif // _TAO
   return false;
}

void cfdExecutive::UpdateCommand()
{
   vprDEBUG(vprDBG_ALL,0) << "doing nothing in cfdExecutive::UpdateCommand()"
                          << std::endl << vprDEBUG_FLUSH;
}
