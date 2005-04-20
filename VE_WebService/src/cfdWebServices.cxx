//***insert license here***\\



#include "cfdWebServices.h"
#include <iostream>
#include <string>
#include <sstream>

#include <vrj/Util/Debug.h>
#include <vpr/System.h>

#include <orbsvcs/CosNamingC.h>

#include "cfdExecutive.h"
#include "VE_i.h"
#include "cfdDCS.h"
#include "cfdEnum.h"
#include "cfdCommandArray.h"
//#include "cfdVEAvailModules.h"
//#include "cfdVEBaseClass.h"
#include "cfdModelHandler.h"
#include "cfdEnvironmentHandler.h"
#include "cfdThread.h"
#include "cfdPfSceneManagement.h"
#include "package.h"
#include "Network_Exec.h"



#define AINTWORKIN
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


   this->runGetEverythingThread = true;
   //this->updateNetworkString = false;
   //this->thread = 0;

   //this->naming_context = CosNaming::NamingContext::_duplicate( 
   //   corbaManager->_vjObs->GetCosNaming()->naming_context );
   this->masterNode = new cfdGroup();
   this->masterNode->SetName( "cfdWebServices_Node" );
   cfdPfSceneManagement::instance()->GetWorldDCS()->AddChild( this->masterNode );
#ifndef AINTWORKIN

   av_modules = new cfdVEAvail_Modules();
   network = new Network();
#endif
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
     // uii = new Body_UI_ithis->(exec, UINAME);
      //Body_UI_i ui_i( UINAME);

      PortableServer::ObjectId_var id = 
         PortableServer::string_to_ObjectId( CORBA::string_dup( "cfdWebServices" ) ); 
#ifndef AINTWORKIN
      //activate it with this child POA 
      childPOA->activate_object_with_id( id.in(), &(*uii) );
#endif      
      // obtain the object reference
      Body::UI_var unit =  
      Body::UI::_narrow( childPOA->id_to_reference( id.in() ) );

      // Don't register it to the naming service anymore
      // the bind call will hang if you try to register
      // Instead, the new idl make the ref part of the register call 
      // Naming Service now is only used for boot trap 
      // to get the ref for Executive

      //Call the Executive CORBA call to register it to the Executive
      exec->RegisterUI( uii->UIName_.c_str(), unit.in() );
      std::cout << "|\tConnected to the Executive " << std::endl;   
	   //this->thread = new cfdThread();
      //thread->new_thread = new vpr::Thread( new vpr::ThreadMemberFunctor< cfdWebServices > ( this, &cfdWebServices::GetEverything ) );
   } 
   catch (CORBA::Exception &) 
   {      
      std::cerr << "|\tExecutive not present or VEClient registration error" << std::endl;
   }

}

///////////////////////////////////////////////////////////////////

void cfdWebServices::UpdateModules( void )
{
   if ( !CORBA::is_nil( this->exec ) && uii->GetCalcFlag() )
   {
      // Get Network and parse it
      this->GetEverything();
   }
}

///////////////////////////////////////////////////////////////////

void cfdWebServices::GetEverything( void )
{

#ifndef AINTWORKIN

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
      for ( iter=IDMap.begin(); iter!=IDMap.end(); iter++ )
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
         iter = IDMap.find( foundPlugin->first );
         if ( iter == IDMap.end() )
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
#endif
}

///////////////////////////////////////////////////////////////////


void cfdWebServices::GetNetwork ( void )
{
   // Get buffer value from Body_UI implementation
   std::string temp( uii->GetNetworkString() );
   const char* networkString = temp.c_str();
   vprDEBUG(vprDBG_ALL,2)  << "|\tNetwork String : " << networkString
                              << std::endl << vprDEBUG_FLUSH;

/////////////////////////////
// This code taken from Executive_i.cpp
   Package p;
   p.SetSysId("temp.xml");
   p.Load(networkString, strlen(networkString));
  
   network->clear();
   IDMap.clear();

   std::vector<Interface>::iterator thisInterface;
   // Find network layout chunk in network structure
   for(thisInterface=p.intfs.begin(); thisInterface!=p.intfs.end(); thisInterface++)
   {
      if ( thisInterface->_id == -1 )  //find the interface with ID = -1
      {
         break;
      }
   }
   //if we did find the layout interface and were able to parse it....
   if(thisInterface!=p.intfs.end() && network->parse(&(*thisInterface))) 
   {
      //loop through the interfaces again
      for(thisInterface=p.intfs.begin(); thisInterface!=p.intfs.end(); thisInterface++)
      {
      
         //try to set this interface's inputs
         if(network->setInput(thisInterface->_id, &(*thisInterface))) 
         {
            network->module(network->moduleIdx(thisInterface->_id))->_is_feedback  = thisInterface->getInt("FEEDBACK");
            network->module(network->moduleIdx(thisInterface->_id))->_need_execute = 1;
            network->module(network->moduleIdx(thisInterface->_id))->_return_state = 0;
         }  
         else
         {
            std::cerr << "|\tUnable to set id# " << thisInterface->_id << "'s inputs" << std::endl;
         }
         //if this is not the layout interface...
         if ( thisInterface->_id != -1 ) 
         {
            int thisInterfaceID = 
               this->network->module( network->moduleIdx(thisInterface->_id) )->get_id();
            //std::cout <<  network->module( network->moduleIdx(thisInterface->_id) )->get_id() << " : " << network->module( network->moduleIdx(thisInterface->_id) )->_name <<std::endl;
            this->IDMap[ thisInterfaceID ] = 
              this->network->module( network->moduleIdx(thisInterface->_id) )->_name;
            //std::cout <<  network->module( network->moduleIdx(thisInterface->_id) )->get_id() << " : " << network->module( network->moduleIdx(thisInterface->_id) )->_name <<std::endl;
            this->interfaceMap[ thisInterfaceID ] = (*thisInterface);
         }
      }
   } 
   else     //this is probably bad
   {
      std::cerr << "Either no network present or error in GetNetwork in VE_Xplorer" << std::endl;
   }
}

void cfdWebServices::insertItemIntoSQL(Interface &interface)
{
   std::vector<std::string> names;              //vector to hold all the names we get
   std::string SQLString;                       //the string we'll pass to the database
   std::vector<std::string>::iterator stringIter;   //an iterator to go through the strings
   
   names = interface.getInts();           //grab the names of the ints from this interface
   SQLString = "";                        //clear the SQL string
   for(stringIter = names.begin(); stringIter != names.end(); stringIter++)   //iterate through the vector
   {
      char numStr[16];
      sprintf(numStr, "%l", interface.getInt(*stringIter));
      SQLString += *stringIter + "|" + numStr + "||";  //tack the name and number into our string, seperated by pipes
   
   }

   names = interface.getDoubles();        //repeat with other data types
   SQLString = "";                        //clear the SQL string
   for(stringIter = names.begin(); stringIter != names.end(); stringIter++)   //iterate through the vector
   {
      char numStr[16];
      sprintf(numStr, "%l", interface.getDouble(*stringIter));
      SQLString += *stringIter + "|" + numStr + "||";  //tack the name and number into our string, seperated by pipes
   
   }
   
   
   names = interface.getStrings();        //repeat with other data types
   SQLString = "";                        //clear the SQL string
   for(stringIter = names.begin(); stringIter != names.end(); stringIter++)   //iterate through the vector
   {
      SQLString += *stringIter + "|" + interface.getString(*stringIter) + "||";  //tack the name and value into our string, seperated by pipes
   
   }

   names = interface.getInts1D();        //repeat with other data types
   SQLString = "";                        //clear the SQL string
   for(stringIter = names.begin(); stringIter != names.end(); stringIter++)   //iterate through the vector
   {
      std::vector<long> vals = interface.getInt1D(*stringIter);                      //this time we have to get vectors and iterate through those
      SQLString += *stringIter;
      std::vector<long>::iterator valIter;
      for(valIter = vals.begin(); valIter != vals.end(); valIter++)
      {
         SQLString += "|" + *valIter;
      }
      SQLString += "||";
   
   }

   names = interface.getDoubles1D();        //repeat with other data types
   SQLString = "";                        //clear the SQL string
   for(stringIter = names.begin(); stringIter != names.end(); stringIter++)   //iterate through the vector
   {
      std::vector<double> vals = interface.getDouble1D(*stringIter);                      //this time we have to get vectors and iterate through those
      SQLString += *stringIter;
      std::vector<double>::iterator valIter;
      for(valIter = vals.begin(); valIter != vals.end(); valIter++)
      {
            char valString[32];
            double val = *valIter;
            sprintf(valString, "%d", val);
            std::string valStringString(valString);
            SQLString += "|" + valStringString;
      }
      SQLString += "||";
   
   }
   
   names = interface.getStrings1D();        //repeat with other data types
   SQLString = "";                        //clear the SQL string
   for(stringIter = names.begin(); stringIter != names.end(); stringIter++)   //iterate through the vector
   {
      std::vector<std::string> vals = interface.getString1D(*stringIter);                      //this time we have to get vectors and iterate through those
      SQLString += *stringIter;
      std::vector<std::string>::iterator valIter;
      for(valIter = vals.begin(); valIter != vals.end(); valIter++)
      {
         SQLString += "|" + *valIter;
      }
      SQLString += "||";
   
   }
   
//the old way of doing it
//   std::map<std::string, long> ints;      //grab a copy of the interface's int map
//   ints = interface.getIntMap();
//   std::map<std::string, long>::iterator intsIter;  //and make an iter
//   for(intsIter = ints.begin(); intsIter != ints.end(); intsIter++)  //iterate through the map
//   {  
//      intString += intsIter->first + "|" + intsIter->second + "||";         //and tack the name and number into our string, separated by pipes
//      
//   }
//   
//   //repeat with doubles
//   std::string doubleString;                
//   std::map<std::string, long> ints;     
//   ints = interface.getIntMap();
//   std::map<std::string, long>::iterator intsIter;  
//   for(doublesIter = ints.begin(); doublesIter != ints.end(); doublesIter++) 
//   {  
//      intString += doublesIter->first + "|" + doublesIter->second + "||";    
//      
//   }

}

cfdWebServices::~cfdWebServices()
{
   delete uii;
   delete network;
   delete masterNode;
}



