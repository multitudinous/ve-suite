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
#include "VE_Open/skel/VjObsS.h"
#include <orbsvcs/CosNamingC.h>
#include "VE_Xplorer/cfdDebug.h"

#include "VEMsgInterpreter.h"

////////////////////////////////////////////////////////////////////
//Navigation command interpreter constructor                     //
////////////////////////////////////////////////////////////////////
VEMsgInterpreter::VEMsgInterpreter(int argc, char** argv)
{
   _initCORBA(argc,argv);

   _quit = false;
   _id = -1;
   _iso_value = 0;
   _timesteps = 0;
   _sc = 0;
   _min = 0;
   _max = 1;
   _geo_state = 0;
   _pre_state = 0;
   _teacher_state = 0;
   _numOfClientInfo = 9;
   _clientInfoArray = new VjObs::obj_pd(50);
   _clientInfoArray->length(_numOfClientInfo);
}
////////////////////////////////////////////////////////////////////
//Navigation command interpreter constructor                     //
////////////////////////////////////////////////////////////////////
VEMsgInterpreter::VEMsgInterpreter(int argc, char** argv, VjObs_ptr server)
{
   _initCORBA(argc,argv);

   _quit = false;
   _id = -1;
   _iso_value = 0;
   _timesteps = 0;
   _sc = 0;
   _min = 0;
   _max = 1;
   _geo_state = 0;
   _pre_state = 0;
   _teacher_state = 0;
   _numOfClientInfo = 9;
   _clientInfoArray = new VjObs::obj_pd(50);
   _clientInfoArray->length(_numOfClientInfo);
   SetServer(server);
}

VEMsgInterpreter::~VEMsgInterpreter()
{
   _shutdownCORBA();
}

//////////////////////////////////////////
void VEMsgInterpreter::_shutdownCORBA()
{
   //do we need to shutdown corba?
}
///////////////////////////////////////////////////////////
void VEMsgInterpreter::_initCORBA(int argc, char** argv)
{
   PortableServer::POA_var poa;
   CORBA::ORB_var orb;
   CosNaming::NamingContext_var naming_context;

   try 
   {
      // First initialize the ORB, 

      orb = CORBA::ORB_init( argc, argv, ""); 

      //Here is the code to set up the ROOT POA
      CORBA::Object_var poa_object =
         orb->resolve_initial_references ("RootPOA"); // get the root poa

      poa = PortableServer::POA::_narrow(poa_object.in());
      PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
      poa_manager->activate();

      //Here is the part to contact the naming service and get the reference for the executive
      CORBA::Object_var naming_context_object =
         orb->resolve_initial_references ("NameService");

      naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in ());
   }  
   catch (CORBA::Exception &) 
   {
      // Finally destroy the ORB
      orb->destroy();
      std::cerr << "CORBA exception raised!" << std::endl;
   }
 
   CosNaming::Name name;
   name.length(1);
   //Now get the reference of the VE server
   name[0].id   = (const char*)"Master";
   name[0].kind = (const char*)"VE_Xplorer";
   try 
   {
      CORBA::Object_var ve_object;
      try
      {
         if ( !CORBA::is_nil( naming_context.in() ) )
            ve_object = naming_context->resolve(name);
      }
      catch ( CORBA::Exception & )
      {
         vprDEBUG(vesDBG,0) << " Can't resolve name " << std::endl << vprDEBUG_FLUSH;
      }

      if ( !CORBA::is_nil( ve_object.in() ) ){
         _server = VjObs::_narrow(ve_object.in());
//         navInterpreter = new VEMsgInterpreter(_server);
      }

      if (CORBA::is_nil(_server))
         std::cerr<<"VjObs is Nill"<< std::endl;
   } 
   catch (CORBA::Exception &) 
   {
      std::cerr << "Can't find VE server" << std::endl;
   }
}
/////////////////////////////////////////////
bool VEMsgInterpreter::IsRunning()
{
   return !_quit;
}
///////////////////////////////////////////////////////////
void VEMsgInterpreter::SetServer(VjObs_ptr server)
{
   _server = VjObs::_duplicate(server);
}
/////////////////////////////////////////////////////////
void VEMsgInterpreter::_sendVoiceCommandToVE()
{
   _clientInfoArray[ 0 ] = (double)_id;
   _clientInfoArray[ 1 ] = (double)_iso_value;
   _clientInfoArray[ 2 ] = (double)_timesteps;
   _clientInfoArray[ 3 ] = (double)_sc;
   _clientInfoArray[ 4 ] = (double)_min;
   _clientInfoArray[ 5 ] = (double)_max;
   _clientInfoArray[ 6 ] = (double)_geo_state;
   _clientInfoArray[ 7 ] = (double)_pre_state;
   _clientInfoArray[ 8 ] = (double)_teacher_state;
   
   vprDEBUG(vesDBG,3) << " Construct data array to send to server side : " << std::endl
	  << "    command id     : " << _clientInfoArray[ 0 ] << std::endl
	  << "    iso_value      : " << _clientInfoArray[ 1 ] << std::endl
	  << "    timesteps      : " << _clientInfoArray[ 2 ] << std::endl
	  << "    sc             : " << _clientInfoArray[ 3 ] << std::endl
	  << "    min            : " << _clientInfoArray[ 4 ] << std::endl
	  << "    max            : " << _clientInfoArray[ 5 ] << std::endl
	  << "    geo_state      : " << _clientInfoArray[ 6 ] << std::endl
	  << "    pre_state      : " << _clientInfoArray[ 7 ] << std::endl
	  << "    teacher_state  : " << _clientInfoArray[ 8 ] << std::endl << vprDEBUG_FLUSH;

   if ( !CORBA::is_nil( _server ) )
   {
      try
      {
         _server->SetClientInfoData( _clientInfoArray );
         vprDEBUG(vesDBG,3) << " Done Setting client data " << std::endl << vprDEBUG_FLUSH;
      }
      catch ( ... )
      {
         std::string( "Failed to send voice command to VE-Xplorer. Probably need to disconnect and reconnect.");
      }
   }
   else
   {
      vprDEBUG(vesDBG,3) << "VEVoiceCommand : Just testing..." << std::endl << vprDEBUG_FLUSH;
   }
}
