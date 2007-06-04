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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <Executive_i.h>
#include <orbsvcs/CosNamingC.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>
#include <iostream>
#include "cfdWebServices.h"
int main(int argc, char* argv[])
{

#ifndef USE_TEST_FILE

   printf("initing corba...\n");
   CORBA::ORB_var orb = CORBA::ORB_init( argc, argv,"VE_Suite_ORB" );
   if ( CORBA::is_nil( orb.in() ) )
   {
      printf("nil thingy.  Quitting\n");
      exit(0);
   }
   //Here is the part to contact the naming service and get the reference for the executive
   printf("resolving initial references...\n");
   CORBA::Object_var naming_context_object =
   orb->resolve_initial_references ("NameService"); 
   printf("Doing the IOR thing...\n");
   CORBA::String_var sior1(orb->object_to_string( naming_context_object.in() ) );
   std::cout << "|  IOR of the server side : " << std::endl << sior1 << std::endl;
   CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in());

   //Here is the code to set up the server
   CORBA::Object_var poa_object = orb->resolve_initial_references ("RootPOA"); // get the root poa
   PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_object.in());
   PortableServer::POAManager_var poa_manager = poa->the_POAManager();

   printf("tao stuff now...\n");
   // Create policy with BiDirPolicy::BOTH
   CORBA::PolicyList policies( 1 );
   policies.length( 1 );

   CORBA::Any pol;
   pol <<= BiDirPolicy::BOTH;
   policies[ 0 ] = orb->create_policy( BiDirPolicy::BIDIRECTIONAL_POLICY_TYPE,
                     pol);

   // Create POA as child of RootPOA with the above policies.  This POA 
   // will receive request in the same connection in which it sent 
   // the request 
   printf("creating POA...\n");
   PortableServer::POA_var child_poa = poa->create_POA ("childPOA",
                       poa_manager.in(),
                       policies);

   // Creation of childPOA is over. Destroy the Policy objects. 
   printf("destroying policies...\n");
   for (CORBA::ULong i = 0; i < policies.length (); ++i)
   {
         policies[i]->destroy();
   }
   poa_manager->activate();

   // Create webservice class here
   // vjobsWrapper->init( naming_context.in(), orb.in() );
   // this recieves data from the executive and writes data to 
   // mysql
   cfdWebServices webService( naming_context.in(), child_poa.in() );
   orb->run();
#else //USE_TEST_FILE
   printf("NOTICE:  WebService is using test file input.\n");
   cfdWebServices webService( NULL, NULL );
#endif   //USE_TEST_FILE
   printf("done.  Exiting now\n");
   return 0;
}
