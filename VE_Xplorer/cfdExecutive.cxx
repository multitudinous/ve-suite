/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
#include "cfdGauges.h"
#include "cfdDashboard.h"
#include "cfdDataSet.h"
#include "cfdExecutiveConfiguration.h"
#include "cfdDCS.h"
#include "cfdGroup.h"
#include "cfdInteractiveGeometry.h"
#include "VE_i.h"
#include "cfdEnum.h"
#include "cfdCommandArray.h"
#include "cfdVEAvailModules.h"
#include "cfdVEBaseClass.h"
#include "cfdModelHandler.h"
#include "cfdEnvironmentHandler.h"

#include "package.h"
#include "interface.h"
#include "Network_Exec.h"
//#include "Executive_i.h"

#include <iostream>
#include <sstream>
#include <string>
#include <time.h>

#include <vtkDataSet.h>
#include <vtkPointData.h>

#include <vrj/Util/Debug.h>

using namespace std;

cfdExecutive::cfdExecutive( CosNaming::NamingContext_ptr naming, cfdDCS* worldDCS )
{
  try
    {
      XMLPlatformUtils::Initialize();
    }
  
  catch(const XMLException &toCatch)
    {
      XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n"
				<< "  Exception message:"
				<< XMLString::transcode(toCatch.getMessage()) << XERCES_STD_QUALIFIER endl;
      //return 1;
    }


   this->_doneWithCalculations = true;
   //this->orb->_duplicate( orb );
   this->naming_context = CosNaming::NamingContext::_duplicate( naming );
   this->worldDCS = worldDCS;
   this->_masterNode = new cfdGroup();
   this->_masterNode->SetName( "cfdExecutive_Node" );
   this->worldDCS->AddChild( (cfdSceneNode*)this->_masterNode );

   av_modules = new cfdVEAvail_Modules();
   _network = new Network();

   //time_t* timeVal;
   long id = (long)time( NULL );
   char uiname[256];
   sprintf(uiname, "VEClient%ld", id);
   std::string UINAME = uiname;
   bool is_orb_init = false;

   _exec = NULL;

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
		ui_i = new Body_UI_i(_exec.in(), UINAME);
		//Body_UI_i ui_i( UINAME);
    
		//pass the network's pointer to the UI corba implementation
		//ui_i.SetUINetwork(network);
      ui_i->SetcfdExecutive( this );
		//Activate it to obtain the object reference
		Body::UI_var ui = (*ui_i)._this();
     
		CosNaming::Name UIname(1);
		UIname.length(1);
		UIname[0].id = CORBA::string_dup (UINAME.c_str());
		//Bind the object
		try	{
			this->naming_context->bind(UIname, ui.in());
		}catch(CosNaming::NamingContext::AlreadyBound& ex){
			this->naming_context->rebind(UIname, ui.in());
		}

		//register it to the server
		_exec->RegisterUI(ui_i->UIName_.c_str());
      std::cout << " Connected to the Executive " << std::endl;   
	} 
   catch (CORBA::Exception &) 
   {		
		cerr << "Can't find executive or UI registration error" << endl;
	}
   
   //_param = new cfdExecutiveConfiguration();

   InitModules();
}

cfdExecutive::~cfdExecutive( void )
{
   delete av_modules;
}

void cfdExecutive::UnbindORB()
{
	CosNaming::Name UIname(1);
	UIname.length(1);
	UIname[0].id = CORBA::string_dup( (ui_i->UIName_).c_str() );

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

void cfdExecutive::init_orb_naming()
{
	//char *argv[2]={"-ORBInitRef", "NameService=file://ns.ior"};
//	char **argv;
//   argv = new char*[ 0 ];
//	int argc = 0;
/*	try {
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
		cerr << "CORBA exception raised!" << endl;
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
   char *network;
  
   try 
   { 
      network = _exec->GetNetwork();
      vprDEBUG(vprDBG_ALL,2)  << "| Network String : " << network 
                              << std::endl << vprDEBUG_FLUSH;
   } 
   catch (CORBA::Exception &) 
   {
      std::cerr << "ERROR: cfdExecutive : no exec found! " << std::endl;
   }

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
      if(iter->_id==-1) break;
  
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
	         cerr << "Unable to set id# " << iter->_id << "'s inputs\n";

         if ( iter->_id != -1 ) 
         {
            //cout << iter->_id <<endl; 
            //cout <<  _network->module( _network->moduleIdx(iter->_id) )->get_id() << " : " << _network->module( _network->moduleIdx(iter->_id) )->_name <<endl;
            _id_map[ _network->module( _network->moduleIdx(iter->_id) )->get_id() ] = _network->module( _network->moduleIdx(iter->_id) )->_name;
            //cout <<  _network->module( _network->moduleIdx(iter->_id) )->get_id() << " : " << _network->module( _network->moduleIdx(iter->_id) )->_name <<endl;
            _it_map[ _network->module( _network->moduleIdx(iter->_id) )->get_id() ] = (*iter);
         }
      }
   } 
   else 
   {
      cerr << "Either no network present or error in GetNetwork in VE_Xplorer" << endl;
   }
///////////////////////////
   delete network;

}
///////////////////////////////////////////////////////////////////

void cfdExecutive::GetOutput (std::string name)
{
  // NOTHING YET
}

///////////////////////////////////////////////////////////////////

void cfdExecutive::GetPort (std::string name)
{
   char *pt_str;
      
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
   if ( !CORBA::is_nil( this->_exec.in() ) )
   {
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
               _plugins[ iter->first ]->InitializeNode( worldDCS );
               _plugins[ iter->first ]->AddSelfToSG();
               _modelHandler->AddModel( _plugins[ iter->first ]->GetCFDModel() );
               _plugins[ iter->first ]->SetCursor( _envHandler->GetCursor() );
               //_plugins[ iter->first ]->SetModuleResults( this->_exec->GetModuleResult( iter->first ) );
               int dummyVar = 0;
               _plugins[ iter->first ]->CreateCustomVizFeature( dummyVar );
               vprDEBUG(vprDBG_ALL,1) << " Plugin [ " << iter->first 
                                      << " ]-> " << iter->second 
                                      << " is being created." << endl << vprDEBUG_FLUSH;
            }
         }
         else
         {
            // plugin already present...
            vprDEBUG(vprDBG_ALL,1) << " Plugin [ " << iter->first 
                                    << " ]-> " << iter->second 
                                    << " is already on the plugin map." << endl << vprDEBUG_FLUSH;
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
            _modelHandler->RemoveModel( foundPlugin->second->GetCFDModel() );
            delete foundPlugin->second;
            _plugins.erase( foundPlugin++ );
         }
         else
         {
            // plugin already present...
            vprDEBUG(vprDBG_ALL,1) << " Plugin [ " << iter->first 
                                    << " ]-> " << iter->second 
                                    << " is already on the plugin and id map." << endl << vprDEBUG_FLUSH;
            ++foundPlugin;
         }
         // The above code is from : The C++ Standard Library by:Josuttis
      }
   }
   else
   {
      std::cerr << "ERROR : The Executive has not been intialized! " <<std::endl;     
   }
}

///////////////////////////////////////////////////////////////////

void cfdExecutive::HowToUse( std::string name )
{
   // get a module id from a name
/*  
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
   double cash_flow  = _ot_map[mod_id].getDouble("CASH_FLOW");*/
}

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
   if ( !CORBA::is_nil( this->_exec.in() ) )
   {
      if ( this->GetCalculationsFlag() )
      {
         this->GetEverything();
         std::cout << " Get Everything " << std::endl;      
         /*std::map<std::string, int>::iterator iter;
         for ( iter=_name_map.begin(); iter!=_name_map.end(); iter++ )
         {
            if ( iter->first=="ASU"    || iter->first=="Power"    ||
                  iter->first=="SELX"  || iter->first=="SRS"      ||
                  iter->first=="STACK" || iter->first=="GASI"     ||
                  iter->first=="WGSR"  || iter->first=="REI_Gasi" ||
                  iter->first=="NETWORK") 
            {
               //std::string temp = "ASU";
               this->_gauges->Update( iter->first, this );
               //this->_gauges->Update( temp, this );
               // Find each modules scalar info
               // Pass info all the way to each gauge
            }
         }*/
         //std::cout << " End Gauge Update " << std::endl;
         //this->_geometry->Update( this->_activeScalarName, this );
         //std::cout << " End Geometry Update " << std::endl;
         this->SetCalculationsFlag( false );
      }
   }
}

void cfdExecutive::SetActiveDataSet( cfdDataSet* dataSet )
{
   this->_3dMesh = dataSet;
   const char* scalarName = this->_3dMesh->GetDataSet()->GetPointData()->GetScalars()->GetName();
   
   std::ostringstream dataStream;
   std::string dataString;
   dataStream << scalarName;
   this->_activeScalarName = dataStream.str();
   //std::cout << this->_activeScalarName<< std::endl;
}

void cfdExecutive::SetCalculationsFlag( bool x )
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   this->_doneWithCalculations = x;
}

bool cfdExecutive::GetCalculationsFlag( void )
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   return this->_doneWithCalculations;
}

void cfdExecutive::SetModelHandler( cfdModelHandler* input, cfdEnvironmentHandler* env )
{
   _modelHandler = input;
   _envHandler = env;
}
bool cfdExecutive::CheckCommandId( cfdCommandArray* commandArray )
{
// Add in cfdCommandArray stuff
#ifdef _TAO
      if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR )
      {
         this->SetCalculationsFlag( true );
         return true;
      }
      return false;
#endif // _TAO
   return false;
}

void cfdExecutive::UpdateCommand()
{
   cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << endl;
}
