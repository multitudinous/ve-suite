//***insert license here***\\



#include "cfdWebServices.h"
#include <iostream>
#include <string>
#include <sstream>

#include <vrj/Util/Debug.h>
#include <vpr/System.h>

#include <orbsvcs/CosNamingC.h>


cfdWebServices::cfdWebServices( CosNaming::NamingContext* inputNameContext, PortableServer::POA* childPOA )
{
  this->namingContext = inputNameContext;
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
   this->masterNode = new cfdGroup();
   this->masterNode->SetName( "cfdExecutive_Node" );
   cfdPfSceneManagement::instance()->GetWorldDCS()->AddChild( this->masterNode );

   av_modules = new cfdVEAvail_Modules();
   network = new Network();

   //time_t* timeVal;
   long id = (long)time( NULL );
   std::ostringstream dirStringStream;
   dirStringStream << "VEClient" << id;
   std::string UINAME = dirStringStream.str();
   bool isOrbInit = false;

   exec = NULL;
   this->uii = 0;
   if (!isOrbInit)
   {
      //init_orb_naming();
      isOrbInit = true;
   }

   try
   {
      CosNaming::Name name(1);
      name.length(1);
      name[0].id = CORBA::string_dup ("Executive");
    
      CORBA::Object_var exec_object = this->namingContext->resolve(name);
      this->exec = Body::Executive::_narrow(exec_object.in());

      //Create the Servant
      uii = new Body_UI_ithis->(exec, UINAME);
      //Body_UI_i ui_i( UINAME);

      PortableServer::ObjectId_var id = 
         PortableServer::string_to_ObjectId( CORBA::string_dup( "cfdExecutive" ) ); 
    
      //activate it with this child POA 
      child_poa->activate_object_with_id( id.in(), &(*ui_i) );

      // obtain the object reference
      Body::UI_var unit =  
      Body::UI::_narrow( childPOA->id_to_reference( id.in() ) );

      // Don't register it to the naming service anymore
      // the bind call will hang if you try to register
      // Instead, the new idl make the ref part of the register call 
      // Naming Service now is only used for boot trap 
      // to get the ref for Executive

      //Call the Executive CORBA call to register it to the Executive
      exec->RegisterUI( ui_i->UIName_.c_str(), unit.in() );
      std::cout << "|\tConnected to the Executive " << std::endl;   
	   //this->thread = new cfdThread();
      //thread->new_thread = new vpr::Thread( new vpr::ThreadMemberFunctor< cfdExecutive > ( this, &cfdExecutive::GetEverything ) );
   } 
   catch (CORBA::Exception &) 
   {      
      std::cerr << "|\tExecutive not present or VEClient registration error" << std::endl;
   }

}

///////////////////////////////////////////////////////////////////

void cfdExecutive::UpdateModules( void )
{
   if ( !CORBA::is_nil( this->_exec ) && ui_i->GetCalcFlag() )
   {
      // Get Network and parse it
      this->GetEverything();
   }
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


cfdWebServices::~cfdWebServices()
{
   delete uii;
   delete network;
   delete masterNode;
}



