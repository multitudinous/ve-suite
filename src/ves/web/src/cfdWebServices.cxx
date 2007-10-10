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
#include "cfdWebServices.h"
#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <vrj/Util/Debug.h>
#include <vpr/System.h>



#include <orbsvcs/CosNamingC.h>

#include "cfdExecutive.h"
#include "veWebService_i.h"
#include "cfdDCS.h"
#include "cfdEnum.h"
#include "cfdCommandArray.h"
//#include "cfdVEAvailModules.h"
//#include "cfdVEBaseClass.h"
//#include "cfdModelHandler.h"
//#include "cfdEnvironmentHandler.h"
//#include "cfdThread.h"
//#include "cfdPfSceneManagement.h"
#include "package.h"
#include "Network_Exec.h"
#include <mysql++.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>


#define AINTWORKIN
cfdWebServices::cfdWebServices( CosNaming::NamingContext* inputNameContext, PortableServer::POA* childPOA )
{
   network = new Network();
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

#ifndef USE_TEST_FILE
  this->namingContext = inputNameContext;
   

   this->runGetEverythingThread = true;
   //this->updateNetworkString = false;
   //this->thread = 0;

   //this->naming_context = CosNaming::NamingContext::_duplicate( 
   //   corbaManager->_vjObs->GetCosNaming()->naming_context );

#ifndef AINTWORKIN

   av_modules = new cfdVEAvail_Modules();
#endif


   //time_t* timeVal;
   long timeID = (long)time( NULL );
   std::ostringstream dirStringStream;
   dirStringStream << "VEClient" << timeID;
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
      uii = new veWebService_i( exec, UINAME);
      uii->setWebServices(this);
      PortableServer::ObjectId_var id = 
         PortableServer::string_to_ObjectId( CORBA::string_dup( "cfdWebServices" ) ); 
      //activate it with this child POA 
      childPOA->activate_object_with_id( id.in(), &(*uii) );  
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
#else
   GetNetwork();
#endif
   printf("Well, we made it through the constructor\n");

}

///////////////////////////////////////////////////////////////////

void cfdWebServices::UpdateModules( void )
{
      printf("we just got told to update\n");
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


char* cfdWebServices::GetNetwork ( void )
{

#ifndef USE_TEST_FILE
   vprDEBUG(vprDBG_ALL,2)  << "getting network..." << std::endl << vprDEBUG_FLUSH;

   // Get buffer value from Body_UI implementation
   std::string temp( uii->GetNetworkString() );
   const char* networkString = temp.c_str();        //***temporary testing purposes.  comment this back in!
   vprDEBUG(vprDBG_ALL,2)  << "|\tNetwork String : " << networkString
                              << std::endl << vprDEBUG_FLUSH;
#else   

   std::string testFile = "/home/users/kennyk/public_html/PowerPlant/data/larry_ruth.nt";
   std::string testNetwork;
   std::ifstream infile(testFile.c_str());						//try open the file
   if (!infile)
   {
      printf("couldn't load the file\n");
   }

   std::string line;
   int lineNum = 0;

   while(infile.peek() != EOF)									//go through every line of the file
   {
      lineNum++;
      std::getline(infile, line);
      testNetwork += line;
   }
   infile.close();
   const char* networkString = testNetwork.c_str();
#endif
   outputFile = fopen("output.dat", "w");
   fullSQLString = "";        //reset the SQL string
   //first we insert a clear command and timestamp into the database so we can tell if things have been updated
   fprintf(outputFile, "delete from VEConfigurations where ID = '0';\n\n");
   /////////////////////////////
// This code taken from Executive_i.cpp
   Package p;
   p.SetSysId("temp.xml");
//   printf("string:  %s\n is %i long\n", networkString, strlen(networkString));
   vprDEBUG(vprDBG_ALL,2)  << "loading package..." << std::endl << vprDEBUG_FLUSH;

   p.Load(networkString, strlen(networkString));
   vprDEBUG(vprDBG_ALL,2)  << "|\ clearing network : " << std::endl << vprDEBUG_FLUSH;

   network->clear();
   vprDEBUG(vprDBG_ALL,2)  << "clearing IDMap..." << std::endl << vprDEBUG_FLUSH;
   IDMap.clear();
   vprDEBUG(vprDBG_ALL,2)  << "getting layout chunk..." << std::endl << vprDEBUG_FLUSH;

   std::vector<Interface>::iterator thisInterface;
   // Find network layout chunk in network structure
   for(thisInterface=p.intfs.begin(); thisInterface!=p.intfs.end(); thisInterface++)
   {
      if ( thisInterface->_id == -1 )  //find the interface with ID = -1
      {
         vprDEBUG(vprDBG_ALL,2)  << "found layout chunk..." << std::endl << vprDEBUG_FLUSH;
         vprDEBUG(vprDBG_ALL,2)  << "Gonna parse now" << std::endl << vprDEBUG_FLUSH;
         break;
      }
   }

   //if we did find the layout interface and were able to parse it....
   if(thisInterface!=p.intfs.end() && network->parse(&(*thisInterface))) 
   {
      vprDEBUG(vprDBG_ALL,2)  << "That parsing thing worked" << std::endl << vprDEBUG_FLUSH;
      //loop through the interfaces again
      for(thisInterface=p.intfs.begin(); thisInterface!=p.intfs.end(); thisInterface++)
      {
      
         //try to set this interface's inputs
         bool inputSet;
         if(network->setInput(thisInterface->_id, &(*thisInterface))) 
         {
            inputSet = true;
            network->module(network->moduleIdx(thisInterface->_id))->_is_feedback  = thisInterface->getInt("FEEDBACK");
            network->module(network->moduleIdx(thisInterface->_id))->_need_execute = 1;
            network->module(network->moduleIdx(thisInterface->_id))->_return_state = 0;
         }  
         else
         {
            inputSet = false;
            std::cerr << "|\tUnable to set id# " << thisInterface->_id << "'s inputs" << std::endl;
         }
         //if this is not the layout interface...
         //and we were able to set the inputs
         if ( thisInterface->_id != -1 && inputSet ) 
         {
            int thisInterfaceID = 
               this->network->module( network->moduleIdx(thisInterface->_id) )->get_id();
            //std::cout <<  network->module( network->moduleIdx(thisInterface->_id) )->get_id() << " : " << network->module( network->moduleIdx(thisInterface->_id) )->_name <<std::endl;
            this->IDMap[ thisInterfaceID ] = 
              this->network->module( network->moduleIdx(thisInterface->_id) )->_name;
            //std::cout <<  network->module( network->moduleIdx(thisInterface->_id) )->get_id() << " : " << network->module( network->moduleIdx(thisInterface->_id) )->_name <<std::endl;
            this->interfaceMap[ thisInterfaceID ] = (*thisInterface);
         }
         insertItemIntoSQL(*thisInterface);
      }
      
      fprintf(outputFile, "insert into VEConfigurations(ID, name, timestamp, data) values ('0', 'Active_Configuration', '%i', '%s');\n\n", time(NULL), fullSQLString.c_str());
      fclose(outputFile);
      mysqlQuery("insert into VEConfigurations(ID,timestamp, data) values ('0', '0', '" + fullSQLString + "');");
     // printf("SQL string = %s\n", fullSQLString.c_str());
      return "what's up?  it worked.  Does Ken actually need to send something back?";
   } 
   else     //this is probably bad
   {
      std::cerr << "Either no network present or error in GetNetwork in VE_Xplorer" << std::endl;
      return "broken!";
   }
   

}

void cfdWebServices::insertItemIntoSQL(Interface &interface)
{
   StringHolder object;
   object.ID = interface._id;
   std::vector<std::string> names;              //vector to hold all the names we get
   std::string SQLString;                       //the string we'll pass to the database
   std::vector<std::string>::iterator stringIter;   //an iterator to go through the strings
   
   names = interface.getInts();           //grab the names of the ints from this interface
      std::string pipe("|");
   std::string dPipe("||");
   SQLString = "";                        //clear the SQL string
   for(stringIter = names.begin(); stringIter != names.end(); stringIter++)   //iterate through the vector
   {

      char numStr[16];
      int thisInteger = interface.getInt(*stringIter);
      sprintf(numStr, "%i", interface.getInt(*stringIter) );
//      printf("number:  %s\n", numStr);
      std::string s(numStr);
      SQLString += *stringIter + "|" + numStr + "||";  //tack the name and number into our string, seperated by pipes
    
     // printf("network string so far...:  %s\n", SQLString.c_str());

   }
   object.intString = SQLString;
   
   
   names = interface.getDoubles();        //repeat with other data types
   SQLString = "";                        //clear the SQL string

   for(stringIter = names.begin(); stringIter != names.end(); stringIter++)   //iterate through the vector
   {
      char numStr[16];
      double val = interface.getDouble(*stringIter);
      sprintf(numStr, "%f", val);
      std::string s(numStr);
      SQLString += *stringIter + "|" + numStr + "||";  //tack the name and number into our string, seperated by pipes

   }
   object.doubleString = SQLString;      
   
  
    names = interface.getStrings();        //repeat with other data types
   SQLString = "";                        //clear the SQL string
   for(stringIter = names.begin(); stringIter != names.end(); stringIter++)   //iterate through the vector
   {
      SQLString += *stringIter + "|" + interface.getString(*stringIter) + "||";  //tack the name and value into our string, seperated by pipes
      
   }
   object.stringString = SQLString;   
   
   
   names = interface.getInts1D();        //repeat with other data types
   SQLString = "";                        //clear the SQL string
   for(stringIter = names.begin(); stringIter != names.end(); stringIter++)   //iterate through the vector
   {
      std::vector<long> vals = interface.getInt1D(*stringIter);                      //this time we have to get vectors and iterate through those
      SQLString += *stringIter;
      std::vector<long>::iterator valIter;
      for(valIter = vals.begin(); valIter != vals.end(); valIter++)
      {
         char valString[32];
         int val = *valIter;
         sprintf(valString, "%i", val);
         std::string valStringString(valString);
         SQLString += "|" + valStringString;

      }
      SQLString += "||";
   
   }
   object.intArrayString = SQLString;   
   
   
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
            sprintf(valString, "%f", val);
            std::string valStringString(valString);
            SQLString += "|" + valStringString;
      }
      SQLString += "||";
   
   }
   object.doubleArrayString = SQLString;   
   
  
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
   object.stringArrayString = SQLString;   
   

   //grab the ID.
   char IDChars[4];
   sprintf(IDChars, "%i", object.ID );
   std::string IDString(IDChars);
   //SQLString = "insert into ppModules(ID, 
//      intString, doubleString, stringString, 
//      intArrayString, doubleArrayString, stringArrayString) 
//      values ('" 
//      + IDString + "','" 
//      + object.intString + "', ' "
//      + object.doubleString + "', '" 
//      + object.stringString + "','"
//      + object.intArrayString + "', '" 
//      + object.doubleArrayString + "', '"
//      + object.stringArrayString + "')";
//   //printf("%s;\n", SQLString.c_str());
   std::string moduleString = "%%%%"
      + IDString + "&&&" 
      + object.intString + "&&&"
      + object.doubleString + "&&&" 
      + object.stringString + "&&&"
      + object.intArrayString + "&&&" 
      + object.doubleArrayString + "&&&"
      + object.stringArrayString;
   fullSQLString += moduleString;
   
} 

cfdWebServices::~cfdWebServices()
{
   delete network;
#ifndef USE_TEST_FILE
   delete masterNode;
   delete uii;
#endif
}

void cfdWebServices::mysqlQuery(std::string qel)
{
   printf("I'm gonna stick \n\n%s\n\n into the database\n", qel.c_str());
   mysqlpp::Connection con(mysqlpp::use_exceptions);
	try 
   {
      
      con.real_connect("test", "vracs001.vrac.iastate.edu", "kennyk", "ILovePHP", 3306,
						 0, 60, NULL);
		mysqlpp::Query query = con.query();
      query.exec(qel.c_str());

	}
	catch (mysqlpp::BadQuery& er) 
   {
		// handle any connection or query errors that may come up
		std::cerr << "Error: " << er.what() << " " << con.errnum() << std::endl;
	}
	catch (mysqlpp::BadConversion& er) 
   {
		// handle bad conversions
		std::cerr << "Error: " << er.what() << "\"." << std::endl
			<< "retrieved data size: " << er.retrieved
			<< " actual data size: " << er.actual_size << std::endl;
	}
	catch (std::exception& er) 
   {
		std::cerr << "Error: " << er.what() << std::endl;
	}


}


