
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
 * File:          $RCSfile: cfdVEXplorer.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdAppWrapper.h"
#include "cfdVjObsWrapper.h"
#ifdef _TAO
#include "Executive_i.h"
#include <orbsvcs/CosNamingC.h>
#else
#include <omniORB4/CORBA.h>
#endif
#include <iostream>
using namespace std;
int main(int argc, char* argv[])
{
#ifdef _TAO
   CORBA::ORB_var orb = CORBA::ORB_init( argc, argv,"VE_Suite_ORB" );
#else
   CORBA::ORB_var orb = CORBA::ORB_init( argc, argv,"omniORB4" ); 
#endif // _TAO
   if ( CORBA::is_nil( orb.in() ) )
      exit(0);
  
   //Here is the part to contact the naming service and get the reference for the executive
   CORBA::Object_var naming_context_object =
     orb->resolve_initial_references ("NameService"); 
   CORBA::String_var sior1(orb->object_to_string( naming_context_object.in() ) );
   cout << "|  IOR of the server side : " << endl << sior1 << endl;
   CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in());

   //Here is the code to set up the server
   CORBA::Object_var poa_object = orb->resolve_initial_references ("RootPOA"); // get the root poa
   PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_object.in());
   PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
   poa_manager->activate();

   cfdVjObsWrapper* vjobsWrapper = new cfdVjObsWrapper();
   vjobsWrapper->init( naming_context.in(), orb.in(), argc, argv );
   cfdAppWrapper* appWrapper = new cfdAppWrapper( argc, argv, vjobsWrapper );

   orb->run();
   delete vjobsWrapper;
   delete appWrapper;

   return 0;
}
