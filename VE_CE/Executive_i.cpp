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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Network/string_ops.h"
#include "VE_CE/Executive_i.h"

#include "VE_Open/XML/Model/ModelCreator.h"
#include "VE_Open/XML/Shader/ShaderCreator.h"
#include "VE_Open/XML/CAD/CADCreator.h"
#include "VE_Open/XML/XMLCreator.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/XMLObjectFactory.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include <iostream>

// Implementation skeleton constructor
Body_Executive_i::Body_Executive_i (CosNaming::NamingContext_ptr nc)
  : naming_context_(CosNaming::NamingContext::_duplicate(nc))
{
   _network = new Network();
   _scheduler = new Scheduler( _network );

   //Initialize all the XML objects
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator("XML",new VE_XML::XMLCreator());
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator("Shader",new VE_Shader::ShaderCreator());
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator("Model",new VE_Model::ModelCreator());
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator("CAD",new VE_CAD::CADCreator());
}

// Implementation skeleton destructor
Body_Executive_i::~Body_Executive_i (void)
{
  delete _network;
  delete _scheduler;
}

char * Body_Executive_i::GetImportData (
    CORBA::Long module_id,
    CORBA::Long port_id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
   _mutex.acquire();
 
   std::cout << "GetImportData " << module_id << " " << port_id << std::endl;

   Module *mod = _network->GetModule( _network->moduleIdx(module_id) );
   if(!mod) 
   {
      std::cerr << "Cannot find module, id# " << module_id << std::endl;
      return CORBA::string_dup("");  
   }
   
   IPort *iport = mod->getIPort(mod->iportIdx(port_id));
  
   //bool        rv = false;
   std::string str;
  
   if ( iport && iport->nconnections()) 
   {
      Connection* conn=iport->connection(0); // should only have one connection
      OPort* oport=conn->get_oport();
    
      if(oport->have_data()) 
      {
         /*Package p;
         p.SetSysId("temp.xml");
         p.intfs.push_back(oport->_data);
      
         str = p.Save(rv);*/
         std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
         nodes.push_back( std::pair< VE_XML::Command*, std::string  >( oport->GetPortData(), std::string( "vecommand" ) ) );
         std::string fileName( "returnString" );
         VE_XML::XMLReaderWriter netowrkWriter;
         netowrkWriter.UseStandaloneDOMDocumentManager();
         netowrkWriter.WriteXMLDocument( nodes, fileName, "Command" );
         str = fileName;
      } 
      else 
      {
         std::string msg = "Mod #" + to_string(module_id) + " IPort, id #" + to_string(port_id)+" has no data\n" ;
         ClientMessage(msg.c_str());
      }
   } 
   else 
   {
      std::string msg = "Unable to get mod #" + to_string(module_id) + " IPort, id #" + to_string(port_id)+"\n" ;
      ClientMessage(msg.c_str());
   }
  
   _mutex.release();
  
   return CORBA::string_dup(str.c_str());
}

void Body_Executive_i::execute (std::string mn)
{
   std::string msg;

   if ( _exec_thread.find(mn)==_exec_thread.end() ) 
   {
      std::cerr << "Cannot find execution thread for " << mn << std::endl;
   } 
   else 
   {
      if ( !_exec_thread[mn]->needexecute() ) 
      {
         msg = "Failed to execute " + mn +"\n";
         std::cerr << msg;
         ClientMessage(msg.c_str());
      }
      else 
      {
         msg = "Executing " + mn +"\n";
         ClientMessage(msg.c_str());
      }
   }
}

void Body_Executive_i::SetExportData (
    CORBA::Long module_id,
    CORBA::Long port_id,
    const char * data
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  //std::cout << "SetExportData "<< module_id << " " << port_id << std::endl;

  //Package p;
  //p.SetSysId("temp.xml");
  //p.Load(data, strlen(data));
   VE_XML::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();
   networkWriter.ReadXMLData( std::string( data ), "Command", "vecommand" );
   std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();
  
   // Should only be one item. But, maybe later...
   //std::vector<Interface>::iterator iter;
   //for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
   {
      if ( !_network->GetModule( _network->moduleIdx( module_id ) )->setPortData( 
            port_id, dynamic_cast< VE_XML::Command* >( objectVector.at(0) ) ) 
         )
      {
         std::string msg = "Unable to set mod id# " + to_string(module_id) + ", port id# " + to_string(port_id)+ "'s port data\n";
         ClientMessage(msg.c_str());
      }
   }
   _mutex.release();
}
  
char * Body_Executive_i::GetExportData (
    CORBA::Long module_id,
    CORBA::Long port_id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
   _mutex.acquire();
   //std::cout << "GetExportData "<< module_id << " " << port_id << std::endl;
  
   //Interface intf;
   VE_XML::Command portData;
   if ( !_network->GetModule( _network->moduleIdx( module_id ) )->getPortData( port_id, portData ) ) 
   {
      std::string msg = "Unable to get mod id# " + to_string(module_id) + ", port id# " + to_string(port_id)+ "'s port data\n";
      ClientMessage(msg.c_str());
      _mutex.release();
      return CORBA::string_dup("");
   }

   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
   nodes.push_back( std::pair< VE_XML::Command*, std::string  >( &portData, std::string( "vecommand" ) ) );
   std::string fileName( "returnString" );
   VE_XML::XMLReaderWriter netowrkWriter;
   netowrkWriter.UseStandaloneDOMDocumentManager();
   netowrkWriter.WriteXMLDocument( nodes, fileName, "Command" );

   //bool        rv;
   //std::string str;
  
   //Package p;
   //p.SetSysId("temp.xml");
   //p.intfs.push_back(intf);
  
   //str = p.Save(rv);
  
   _mutex.release();
  
   //if(rv) 
   return CORBA::string_dup( fileName.c_str() );
}

void Body_Executive_i::SetProfileData (
    CORBA::Long module_id,
    CORBA::Long port_id,
    const Types::Profile & data
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();

  std::cout << " SetProfileData "<< module_id << " " << port_id << std::endl;
 
  
   if( !_network->GetModule( _network->moduleIdx( module_id ) )->setPortProfile( port_id, &data) ) 
   {
      std::string msg = "Unable to set mod id# " + to_string(module_id) + ", port id# " + to_string(port_id)+ "'s port profile\n";
      ClientMessage(msg.c_str());
   } 
   else 
   {
      ; //std::cout << "SetPortProfile success\n";
   }
 
  _mutex.release();
}

void Body_Executive_i::GetProfileData (
    CORBA::Long module_id,
    CORBA::Long port_id,
    Types::Profile_out data
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
 
  std::cout << "GetProfileData " << module_id << " " << port_id << std::endl;

  Module *mod = _network->GetModule(_network->moduleIdx(module_id));
  if(!mod) {
    std::cerr << "Cannot find module, id# " << module_id << std::endl;
    return;
  }
  IPort *iport = mod->getIPort(mod->iportIdx(port_id));
  
  if(iport && iport->nconnections()) {
    Connection* conn=iport->connection(0); // should only have one connection
    OPort* oport=conn->get_oport();
    
    if(oport->have_profile()) {
      data = new Types::Profile(*(oport->_profile));
    } else {
      std::string msg = "Mod #" + to_string(module_id) + " IPort, id #" + to_string(port_id)+" has no Profile\n" ;
      std::cerr << msg;
      ClientMessage(msg.c_str());
    }
  } else {
    std::string msg = "Unable to get Profile from mod #" + to_string(module_id) + " IPort, id #" + to_string(port_id)+"\n" ;
    std::cerr << msg;
    ClientMessage(msg.c_str());	
  }
  
  _mutex.release();
}
/////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::execute_next_mod( long module_id )
{
   //id is module jsut executed
   std::string msg( "" );

   std::string mod_type = _network->GetModule(_network->moduleIdx(module_id))->GetModuleName();
   if ( _mod_units.find( mod_type ) != _mod_units.end() ) 
   {
      try 
      {
         _mod_units[ mod_type ]->_non_existent();
         msg = _mod_units[ mod_type ]->GetStatusMessage();
      }
      catch(CORBA::Exception &) 
      {
         std::cerr << "Cannot contact module id " << module_id 
                     << " name " << mod_type << std::endl;
         return;
      }
   }
   else 
   {
      std::cerr << "Module name " << mod_type << " is not yet connected." << std::endl;
      return;
   }

	if (msg!="")
	{
		VE_XML::XMLReaderWriter networkWriter;
		networkWriter.UseStandaloneDOMDocumentManager();
		networkWriter.ReadFromString();
		networkWriter.ReadXMLData( msg, "Command", "vecommand" );
		std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

		if ( !objectVector.empty() ) 
		{
			VE_XML::Command* returnState = dynamic_cast< VE_XML::Command* >( objectVector.at( 0 ) );
  
			long rs;
			// 0:O.K, 1:ERROR, 2:?, 3:FB COMLETE
			returnState->GetDataValuePair( "RETURN_STATE" )->GetData( rs );

			if ( rs == -1 ) 
			{
            returnState->GetDataValuePair( "return_state" )->GetData( rs );
         }
    
			_network->GetModule(_network->moduleIdx(module_id))->_return_state = rs;
    
			if(rs!=1) 
			{
				int rt = _scheduler->execute(_network->GetModule(_network->moduleIdx(module_id)))-1;
				if(rt<0) 
				{
               ClientMessage("VES Network Execution Complete\n");
            }
            else if(_mod_units.find(_network->GetModule(rt)->GetModuleName())==_mod_units.end()) 
            {
               std::cerr <<  "Cannot find running unit " << _network->GetModule(rt)->GetModuleName() << std::endl;
            }
            else 
            {
               std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
               std::vector< VE_XML::Command* > inputList = _network->GetModule( rt )->GetInputData();
               for ( size_t k = 0; k < inputList.size(); ++k )
               {
                  nodes.push_back( std::pair< VE_XML::Command*, std::string  >( 
                                    inputList.at( k ), std::string( "vecommand" ) ) 
                                 );
               }
               std::string fileName( "returnString" );
               networkWriter.UseStandaloneDOMDocumentManager();
               networkWriter.WriteXMLDocument( nodes, fileName, "Command" );

               try 
               {
                  long int tempID = static_cast< long int >(  _network->GetModule(rt)->get_id() );
                  _mod_units[ _network->GetModule(rt)->GetModuleName() ]->SetParams( tempID, fileName.c_str() );
                  _mod_units[ _network->GetModule(rt)->GetModuleName() ]->SetCurID( tempID );
                  execute( _network->GetModule(rt)->GetModuleName() );
               }
               catch(CORBA::Exception &)
               {
                  std::cerr << "Cannot contact Module " << module_id << std::endl;
               }
            }
         }
      }
   }
}
////////////////////////////////////////////////////////////////////////////  
void Body_Executive_i::SetModuleMessage (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{  
   // send a unit message to all uis
   std::string message = std::string( "SetModuleMessage ") + std::string( msg );
   _mutex.acquire();
   ClientMessage( message.c_str() );
   _mutex.release();
}
////////////////////////////////////////////////////////////////////////////  
void Body_Executive_i::SetModuleResult (
    CORBA::Long module_id,
    const char * result
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
   _mutex.acquire();

   //Package p;
   //p.SetSysId("temp.xml");
   //p.Load(result, strlen(result));

   // Should only be one item. But, maybe later...
   //std::vector<Interface>::iterator iter;
   //for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
   {
      VE_XML::XMLReaderWriter networkWriter;
      networkWriter.UseStandaloneDOMDocumentManager();
      networkWriter.ReadFromString();
      networkWriter.ReadXMLData( std::string( result ), "Command", "vecommand" );
      std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

      _network->GetModule( _network->moduleIdx( module_id ) )->SetResultsData( objectVector );
      /*if ( _network->setOutput( module_id, &(*iter) ) ) 
      {
         // Keep track of power requirements
         bool f = false;
         std::string p = iter->getString(std::string("Power (MW)"), &f);
         if(f) 
            _module_powers[module_id] = atof(p.c_str());

         std::string ti = iter->getString(std::string("Thermal Input (MW)"), &f);

         if(f) 
            _thermal_input[module_id] = atof(ti.c_str()); //changed by yang
         //original code is  //if(f) _thermal_input = atof(ti.c_str());
      } 
      else 
      {
         std::string msg = "Unable to set mod id# " + to_string(module_id) + "'s Output data\n";
         ClientMessage(msg.c_str());
      }*/
   }

   std::string msg = "Mod id# "+ to_string(module_id) + "'s Execution is done\n";
   ClientMessage(msg.c_str());
  
  _mutex.release();
}
////////////////////////////////////////////////////////////////////////////  
char * Body_Executive_i::GetModuleResult (
    CORBA::Long module_id
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
   std::cout << "Body_Executive_i::GetModuleResult has been replaced with the query function." << std::endl;
   return CORBA::string_dup("");
}
////////////////////////////////////////////////////////////////////////////  
void Body_Executive_i::SetNetwork (
    const char * network
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
   _mutex.acquire();
   //Clear old network and schedule
   _network->clear();
   _scheduler->clear();
  
   // Keep track of power requirements
   //_module_powers.clear();
   //_thermal_input.clear();
   if ( _network->parse( std::string( network ) ) )
   {
      _mutex.release();
      // Make the new schedule
      if ( !_scheduler->schedule(0) )
      {
         ClientMessage( "Error in VES Schedule\n" );
         return;
      }
      else 
      {
         ClientMessage( "Successfully Scheduled VES Network\n" );
         _scheduler->print_schedule();
      }
   }
   else 
   {
      _mutex.release();
      ClientMessage( "Error in SetNetwork\n" );
      return;
   }
}
////////////////////////////////////////////////////////////////////////////  
char * Body_Executive_i::GetNetwork ( 
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
   _mutex.acquire();
   std::string xmlNetwork = _network->GetNetworkString();
   _mutex.release();

   if ( xmlNetwork.empty() )
      ClientMessage( "No Current VES Network Present In VE-CE.\n" );
  
   return CORBA::string_dup( xmlNetwork.c_str() );
}
////////////////////////////////////////////////////////////////////////////  
void Body_Executive_i::SetModuleUI (
    CORBA::Long module_id,
    const char * ui
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
   _mutex.acquire();
  
   ///I don't think this function is used. We may be able to remove it.
   VE_XML::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();
   networkWriter.ReadXMLData( std::string( ui ), "Command", "vecommand" );
   std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

   _network->GetModule( _network->moduleIdx( module_id ) )->SetInputData( objectVector );
   _network->GetModule( _network->moduleIdx( module_id ) )->_need_execute = 1;
   _network->GetModule( _network->moduleIdx( module_id ) )->_return_state = 0;
  
  /*Package p;
  p.SetSysId("temp.xml");
  p.Load(ui, strlen(ui));
  
   // Should only be one item. But, maybe later...
   std::vector<Interface>::iterator iter;
   for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
   {
      //if ( iter->_type == 1 ) // this block is for inputs not geom
      {
         if ( _network->setInput( module_id, &(*iter) ) ) 
         {
            _network->GetModule( _network->moduleIdx(iter->_id) )->_need_execute = 1;
            _network->GetModule( _network->moduleIdx(iter->_id) )->_return_state = 0;
         }
         else
            std::cerr << "Unable to set mod id# " << module_id << "'s Input data" << std::endl;
      }
   }*/
   _mutex.release();
}
  
void Body_Executive_i::SetWatchList (
    const Types::ArrayLong & id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  watch_list_ = id;
  
  _mutex.release();
}

::Types::ArrayLong * Body_Executive_i::GetWatchList (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  // Stuff here

  _mutex.release();
  
  return new ::Types::ArrayLong(watch_list_);
}

char * Body_Executive_i::GetStatus (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  // Stuff here
  
  _mutex.release();
  
  return CORBA::string_dup("");//str.c_str());//"yang";//0;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::StartCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
   //_scheduler->reset();

   if(_scheduler->snodes_size()==0)
   {
      return;
   }

   _mutex.acquire();
  
   int rt = _scheduler->execute(0)-1;
  
   if(rt<0) 
   {
      std::cerr << "VES Network Execution Complete" << std::endl;
   }
   else 
   {
      std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
      std::vector< VE_XML::Command* > inputList = _network->GetModule( rt )->GetInputData();
      for ( size_t k = 0; k < inputList.size(); ++k )
      {
         nodes.push_back( std::pair< VE_XML::Command*, std::string  >( 
                           inputList.at( k ), std::string( "vecommand" ) ) 
                        );
      }

      std::string fileName( "returnString" );
      VE_XML::XMLReaderWriter netowrkWriter;
      netowrkWriter.UseStandaloneDOMDocumentManager();
      netowrkWriter.WriteXMLDocument( nodes, fileName, "Command" );

      try 
      {
         std::cout << "Initial Execute" << std::endl;
         if ( !_mod_units.empty() )
         {
            std::string moduleName = _network->GetModule( rt )->GetModuleName();
            if ( _mod_units.find( moduleName ) != _mod_units.end() )
            {
               long int tempID = static_cast< long >( _network->GetModule( rt )->get_id() );
               _mod_units[ moduleName ]->SetParams( tempID, fileName.c_str() );
               _mod_units[ moduleName ]->SetCurID( tempID );
               // This starts a chain reaction which eventually leads to Execute_Thread
               // which calls executenextmod in this class
               // by having the thread do that all subsequent modules get executed
               execute( moduleName );
            }
            else
            {
               std::cerr << "Initial Execute, module " << moduleName 
                           << " is not registered yet" << std::endl;
            }
         }
         else
         {
            std::cerr << " No Module Units connected to the VE-CE, skipping execution " << std::endl;
         }
      }
      catch(CORBA::Exception &) 
      {
         std::cerr << "Initial Execute, cannot contact Module " 
                     << _network->GetModule(rt)->GetModuleName() << std::endl;
      }
   }
   
   _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::StopCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
   // Stop all units
   std::map<std::string, Body::Unit_var>::iterator iter;
   for ( iter = _mod_units.begin(); iter != _mod_units.end(); )
   {
	   try 
      {
   	   //queryString.append( iter->second->Query() );
   	   iter->second->StopCalc();
         ++iter;
	   }
      catch (CORBA::Exception &) 
      {
         // std::cout << iter->first <<" is obsolete." << std::endl;
         // it seems this call should be blocked as we are messing with 
         // a map that is used everywhere
         UnRegisterUnit( iter->first.c_str() );
         // Not sure if increment here or not
         _mod_units.erase( iter++ );
	   }
   }
  _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::PauseCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
   // Pause all units
   std::map<std::string, Body::Unit_var>::iterator iter;
   for ( iter = _mod_units.begin(); iter != _mod_units.end(); )
   {
	   try 
      {
   	   //queryString.append( iter->second->Query() );
   	   iter->second->PauseCalc();
         ++iter;
	   }
      catch (CORBA::Exception &) 
      {
         // std::cout << iter->first <<" is obsolete." << std::endl;
         // it seems this call should be blocked as we are messing with 
         // a map that is used everywhere
         UnRegisterUnit( iter->first.c_str() );
         // Not sure if increment here or not
         _mod_units.erase( iter++ );
	   }
   }
  _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::Resume (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
   _mutex.acquire();
   // Resume all the modules
   std::map<std::string, Body::Unit_var>::iterator iter;
   for ( iter = _mod_units.begin(); iter != _mod_units.end(); )
   {
	   try 
      {
   	   //queryString.append( iter->second->Query() );
   	   iter->second->Resume();
         ++iter;
	   }
      catch (CORBA::Exception &) 
      {
         // std::cout << iter->first <<" is obsolete." << std::endl;
         // it seems this call should be blocked as we are messing with 
         // a map that is used everywhere
         UnRegisterUnit( iter->first.c_str() );
         // Not sure if increment here or not
         _mod_units.erase( iter++ );
	   }
   }
   _mutex.release();
}
  
char *  Body_Executive_i::Query (  const char * command
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC (( CORBA::SystemException, Error::EUnknown ))
{
   // read the command to get the module name and module id
   VE_XML::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();
   networkWriter.ReadXMLData( command, "Command", "vecommand" );
   std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

   std::string moduleName;
   unsigned int moduleId;
   VE_XML::Command* tempCommand = dynamic_cast< VE_XML::Command* >( objectVector.at( 0 ) );
   VE_XML::Command passCommand;
   passCommand.SetCommandName( tempCommand->GetCommandName() );
   size_t numDVP = tempCommand->GetNumberOfDataValuePairs();
   for ( size_t i = 0; i < numDVP; ++i )
   {
      VE_XML::DataValuePair* tempPair = tempCommand->GetDataValuePair( i );
      if ( tempPair->GetDataName() == "moduleName" )
      {
         tempPair->GetData( moduleName );
      }
      else if ( tempPair->GetDataName() == "moduleId" )
      {
         tempPair->GetData( moduleId );
      }
      else
      {
         passCommand.GetDataValuePair(-1)->SetData( tempPair->GetDataName(), tempPair );
      }
   }
        
   ///string used to hold query data
   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
   nodes.push_back( 
                   std::pair< VE_XML::XMLObject*, std::string >( &passCommand, "vecommand" ) 
                   );
                   
   VE_XML::XMLReaderWriter commandWriter;
   std::string status="returnString";
   commandWriter.UseStandaloneDOMDocumentManager();
   commandWriter.WriteXMLDocument( nodes, status, "Command" );

   std::string queryString;

   _mutex.acquire();
   // Resume all the modules
   std::map<std::string, Body::Unit_var>::iterator iter;
   iter = _mod_units.find( moduleName );
   if ( iter == _mod_units.end() )
   {
      _mutex.release();
      return 0;
   }
   
   try 
   {
      iter->second->_non_existent();
      iter->second->SetCurID( moduleId );
      queryString.assign( iter->second->Query( CORBA::string_dup( status.c_str() ) ) );
   }
   catch (CORBA::Exception &) 
   {
      UnRegisterUnit( moduleName.c_str() );
      // Not sure if increment here or not
      _mod_units.erase( iter );
   }

   return CORBA::string_dup( queryString.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::RegisterUI (
    const char * UIName,
      Body::UI_ptr ui_ptr
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{ 
   _mutex.acquire();
  
   //CosNaming::Name name(1);
   //name.length(1);
   //name[0].id = CORBA::string_dup(UIName);
  
   try 
   {
      //  CORBA::Object_var ui_object = naming_context_->resolve(name); 
      //  if (CORBA::is_nil(ui_object))
      //   std::cerr << "NULL UI_OBJ" << std::endl;
    
      //Body::UI_var ui = Body::UI::_narrow(ui_object.in());
    
      Body::UI_var ui = Body::UI::_duplicate(ui_ptr);
      if ( CORBA::is_nil(ui) )
         std::cerr << "NULL UI" << std::endl;

      //uis_.insert(std::pair<std::string, Body::UI_var>(std::string(UIName), ui));
      uis_[ std::string(UIName) ] = ui;

      _mutex.release();
      ui->Raise("Connected to Executive\n");
      std::cerr << UIName << " : registered a UI" << std::endl;
   }
   catch (CORBA::Exception &ex) 
   {
      //std::cerr << "CORBA exception raised! : " <<ex._name<< std::endl;
      //std::cerr << ex._info<<std::endl;
      std::cerr<<"Can't call UI name "<<UIName<< std::endl;
      _mutex.release();
   }
   return;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::RegisterUnit (
    const char * UnitName,
   Body::Unit_ptr unit,
    CORBA::Long flag
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
   // When this is called, a unit is already binded to the name service, 
   // so this call can get it's reference from the name service
   std::string message =  std::string("Going to RegisterUnit ") + std::string( UnitName ) + std::string("\n" );
   ClientMessage( message.c_str() );

   _mod_units[std::string(UnitName)] = Body::Unit::_duplicate(unit);

   std::map<std::string, Execute_Thread*>::iterator iter;
   iter = _exec_thread.find( std::string(UnitName) );

   if( iter == _exec_thread.end() ) 
   {
      // CLEAN THIS UP IN UNREGISTER UNIT !
      Execute_Thread *ex = new Execute_Thread( _mod_units[std::string(UnitName)], (Body_Executive_i*)this);
      ex->activate();
      _exec_thread[ std::string(UnitName) ] = ex;
   }
   else //replace it with new reference
   {
      ACE_Task_Base::cleanup(iter->second, NULL);
      if (iter->second)
      {
         delete iter->second;
      }
      Execute_Thread *ex = new Execute_Thread( _mod_units[std::string(UnitName)], (Body_Executive_i*)this);
      ex->activate();
   }
   message = std::string( "Successfully registered " ) + std::string( UnitName ) + std::string("\n" );
   ClientMessage( message.c_str() );
}
////////////////////////////////////////////////////////////////////////////////  
void Body_Executive_i::UnRegisterUI (
    const char * UIName
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
	std::map<std::string, Body::UI_var>::iterator iter;
	_mutex.acquire();

  
	// Add your implementation here
	iter = uis_.find(std::string(UIName));
	if (iter!= uis_.end())
	{
		uis_.erase(iter);
      std::cout<<UIName<<" Unregistered!\n";
	}
  _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::UnRegisterUnit (
    const char * UnitName
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
   _mutex.acquire();

   std::map<std::string, Execute_Thread*>::iterator iter;
   iter=_exec_thread.find(std::string(UnitName));
   if (iter!=_exec_thread.end())
   {
	   ACE_Task_Base::cleanup(iter->second, NULL);
	   if (iter->second)
		   delete iter->second;

	   _exec_thread.erase(iter);
   }   
  
   _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
CORBA::Long Body_Executive_i::GetGlobalMod (
    Types::ArrayLong_out ids
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  // Add your implementation here
  
  _mutex.release();
  
  return CORBA::Long(0);
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::SetID (
                              const char * moduleName,
                              ::CORBA::Long id
                              )
ACE_THROW_SPEC ((
                 ::CORBA::SystemException,
                 ::Error::EUnknown
                 ))
{
   std::map< std::string, Body::Unit_var >::iterator iter;
   _mutex.acquire();

   iter = _mod_units.find( std::string( moduleName ) );
   if ( iter == _mod_units.end() )
   {
      _mutex.release();
      return;
   }

   try 
   {
      iter->second->_non_existent();
      iter->second->SetID( id );
      _mutex.release();
   }
   catch (CORBA::Exception &) 
   {
      std::cout << iter->first <<" is obsolete." << std::endl;
      _mod_units.erase( iter );
      _mutex.release();
      UnRegisterUnit( moduleName );
   }
   catch (...)
   {
      std::cout << "another kind of exception " << std::endl;
   }
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::DeleteModuleInstance (
                                             const char * moduleName,
                                             ::CORBA::Long module_id
                                             )
ACE_THROW_SPEC ((
                 ::CORBA::SystemException,
                 ::Error::EUnknown
                 ))
{
   // Add your implementation here
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::SetParams (
                                  const char * moduleName,
                                  ::CORBA::Long module_id,
                                  const char * param
                                  )
ACE_THROW_SPEC ((
                 ::CORBA::SystemException,
                 ::Error::EUnknown
                 ))
{
   // Add your implementation here
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::ClientMessage(const char *msg)
{
   std::cout << "CE Output = " << msg;
	std::map<std::string, Body::UI_var>::iterator iter;
	for(iter=uis_.begin(); iter!=uis_.end(); ) 
	{
      std::cout << msg << " to -> " << iter->first << std::endl;
	   try 
      {
         iter->second->_non_existent();
   	   iter->second->Raise(msg);
         ++iter;
	   }
      catch (CORBA::Exception &) 
      {
         std::cout << iter->first <<" is obsolete." << std::endl;
         // it seems this call should be blocked as we are messing with 
         // a map that is used everywhere
		   uis_.erase( iter++ );
	   }
      catch (...)
      {
         std::cout << "another kind of exception " << std::endl;
      }
   }
}
