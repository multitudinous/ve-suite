/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <Executive_i.h>
#include <orbsvcs/CosNamingC.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>
#include <iostream>
#include <cfdWebServices.h>
int main( int argc, char* argv[] )
{

#ifndef USE_TEST_FILE

    std::cout << "initing corba..." << std::endl;
    CORBA::ORB_var orb = CORBA::ORB_init( argc, argv, "VE_Suite_ORB" );
    if( CORBA::is_nil( orb.in() ) )
    {
        std::cout << "nil thingy.  Quitting" << std::endl;
        exit( 0 );
    }
    //Here is the part to contact the naming service and get the reference for the executive
    std::cout << "resolving initial references..." << std::endl;
    CORBA::Object_var naming_context_object =
        orb->resolve_initial_references( "NameService" );
    std::cout << "Doing the IOR thing..." << std::endl;
    CORBA::String_var sior1( orb->object_to_string( naming_context_object.in() ) );
    std::cout << "|  IOR of the server side : " << std::endl << sior1 << std::endl;
    CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow( naming_context_object.in() );

    //Here is the code to set up the server
    CORBA::Object_var poa_object = orb->resolve_initial_references( "RootPOA" ); // get the root poa
    PortableServer::POA_var poa = PortableServer::POA::_narrow( poa_object.in() );
    PortableServer::POAManager_var poa_manager = poa->the_POAManager();

    std::cout << "tao stuff now..." << std::endl;
    // Create policy with BiDirPolicy::BOTH
    CORBA::PolicyList policies( 1 );
    policies.length( 1 );

    CORBA::Any pol;
    pol <<= BiDirPolicy::BOTH;
    policies[ 0 ] = orb->create_policy( BiDirPolicy::BIDIRECTIONAL_POLICY_TYPE,
                                        pol );

    // Create POA as child of RootPOA with the above policies.  This POA
    // will receive request in the same connection in which it sent
    // the request
    std::cout << "creating POA..." << std::endl;
    PortableServer::POA_var child_poa = poa->create_POA( "childPOA",
                                                         poa_manager.in(),
                                                         policies );

    // Creation of childPOA is over. Destroy the Policy objects.
    std::cout << "destroying policies..." << std::endl;
    for( CORBA::ULong i = 0; i < policies.length(); ++i )
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
    std::cout << "NOTICE:  WebService is using test file input." << std::endl;
    cfdWebServices webService( NULL, NULL );
#endif   //USE_TEST_FILE
    std::cout << "done.  Exiting now" << std::endl;
    return 0;
}
