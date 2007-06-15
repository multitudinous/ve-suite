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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include "VE_Open/skel/moduleC.h"
#include "VE_CE/Executive_i.h"

#include <iostream>

#include <tao/BiDir_GIOP/BiDirGIOP.h>
#include <orbsvcs/CosNamingC.h>

//This Exe_server act as the servant of the Executive
//This Exe_server is also the Unit's client and the UI's client.
XERCES_CPP_NAMESPACE_USE

int main (int argc, char* argv[])
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
      return 1;
   }

   try
   {      
      // First initialize the ORB, 
      CORBA::ORB_var orb = CORBA::ORB_init (argc, argv, "Yang");

      //Here is the part to contact the naming service and get the reference for the executive
      CORBA::Object_var naming_context_object =
         orb->resolve_initial_references ("NameService");

      CosNaming::NamingContext_var naming_context =
         CosNaming::NamingContext::_narrow (naming_context_object.in ());

      //Here is the code to set up the server
      CORBA::Object_var poa_object = orb->resolve_initial_references ("RootPOA"); // get the root poa
      PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_object.in());
      PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
      //poa_manager->activate ();
      CORBA::PolicyList policies (1);
      policies.length (1);

      CORBA::Any pol;
      pol <<= BiDirPolicy::BOTH;
      policies[0] =
         orb->create_policy (BiDirPolicy::BIDIRECTIONAL_POLICY_TYPE, pol);

      // Create POA as child of RootPOA with the above policies.  This POA
      // will receive request in the same connection in which it sent
      // the request
      PortableServer::POA_var child_poa =
         poa->create_POA( "childPOA", poa_manager.in (), policies);

      // Creation of childPOA is over. Destroy the Policy objects.
      for (CORBA::ULong i = 0; i < policies.length (); ++i)
      {
         policies[i]->destroy ();
      }

      poa_manager->activate();

      //Create the Servant, pass in the pointer of the naming context
      Body_Executive_i exec_i(naming_context.in());

      PortableServer::ObjectId_var id =
         PortableServer::string_to_ObjectId( "Executive" );

      child_poa->activate_object_with_id (id.in(), &exec_i);

      //Activate it to obtain the object reference
      CORBA::Object_var objectRef = child_poa->id_to_reference( id.in () );
      Body::Executive_var exec = Body::Executive::_narrow( objectRef.in() );

      CosNaming::Name name(1);
      name.length(1);
      name[0].id = CORBA::string_dup( "Executive" );

      std::cout << " VE_CE : Trying to Register to Naming Service" << std::endl;
      //Bind the object
      try 
      {
         naming_context->bind(name, exec.in());
      } 
      catch( CosNaming::NamingContext::AlreadyBound& ex )
      {
         naming_context->rebind( name, exec.in() );
      }
      std::cout << " VE_CE : Registered to Naming Service" << std::endl;
      orb->run();

      // Destroy the POA, waiting until the destruction terminates
      poa->destroy(1, 1);
      // Finally destroy the ORB
      orb->destroy();
   }
   catch (CORBA::Exception &) 
   {
      std::cerr << "CORBA exception raised : Unable to connect to Naming Service!" << std::endl;
   }
   return 0;
}
