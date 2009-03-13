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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
//TAO headers need to be first or else windows complains
#include <orbsvcs/CosNamingC.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>
#include <tao/TAO_Internal.h>
//End TAO headers

#include <ves/open/moduleS.h>
#include "AppWrapper.h"
#include "VjObsWrapper.h"

#include <iostream>
#include <cstdlib>
#include <vpr/System.h>
#include <vrj/Kernel/Kernel.h>

#include <ves/VEConfig.h>

using namespace ves::xplorer;

int main( int argc, char* argv[] )
{
    std::cout
        << "|-----------------------------------------------------------------|"
        << std::endl
        << "|\tVE-Xplorer Version "
        << VES_MAJOR_VERSION << "."
        << VES_MINOR_VERSION << "."
        << VES_PATCH_VERSION << "."
        << SVN_VES_REVISION << std::endl
        << "|-----------------------------------------------------------------|"
        << std::endl;

    try
    {
        //std::string Advanced_Resource_Factory( "static Advanced_Resource_Factory \"-ORBReactorType select_st -ORBInputCDRAllocator null -ORBConnectionCacheLock null -ORBFlushingStrategy blocking\"" );
        //std::string Client_Strategy_Factory( "static Client_Strategy_Factory \"-ORBProfileLock null -ORBClientConnectionHandler RW\"" );
        //std::string  Server_Strategy_Factory( "static Server_Strategy_Factory \"-ORBConcurrency thread-per-connection -ORBPOALock thread -ORBThreadPerConnectionTimeout 1\"" );
        
        //resource factory args, server strategy factory args, client args
        //TAO::ORB::default_svc_conf_entries( 0, Server_Strategy_Factory.c_str(), 0 );
        
        CORBA::ORB_var orb = CORBA::ORB_init( argc, argv, "VE_Suite_ORB" );

        //Here is the part to contact the naming service and get the reference for the executive
        CORBA::Object_var naming_context_object =
            orb->resolve_initial_references( "NameService" );
        //orb->perform_work();
        //CORBA::String_var sior1( orb->object_to_string( naming_context_object.in() ) );
        //std::cout << "|\tIOR of the server side : " << std::endl << sior1 << std::endl;
        CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow( naming_context_object.in() );
        //orb->perform_work();
        //Here is the code to set up the server
        CORBA::Object_var poa_object = orb->resolve_initial_references( "RootPOA" ); // get the root poa
        PortableServer::POA_var poa = PortableServer::POA::_narrow( poa_object.in() );
        PortableServer::POAManager_var poa_manager = poa->the_POAManager();

        // Create policy with BiDirPolicy::BOTH
        CORBA::PolicyList policies( 1 );
        policies.length( 1 );

        CORBA::Any pol;
        pol <<= BiDirPolicy::BOTH;
        policies[ 0 ] =
            orb->create_policy( BiDirPolicy::BIDIRECTIONAL_POLICY_TYPE, pol );

        // Create POA as child of RootPOA with the above policies.  This POA
        // will receive request in the same connection in which it sent
        // the request

        PortableServer::POA_var child_poa = poa->create_POA( "childPOA",
                                                             poa_manager.in(), policies );

        // Creation of childPOA is over. Destroy the Policy objects.
        for( CORBA::ULong i = 0; i < policies.length(); ++i )
        {
            policies[i]->destroy();
        }
        poa_manager->activate();

        //Initialize Xplorer CORBA interfaces
        VjObsWrapper* vjobsWrapper = new VjObsWrapper();
        vjobsWrapper->init( naming_context.in(), orb.in(), child_poa.in(), NULL, argc, argv );

        //Start the juggler kernel here so that we can run on darwin
        vrj::Kernel* kernel = vrj::Kernel::instance(); // Declare a new Kernel
#if __VJ_version >= 2003000
        kernel->init( argc, argv );
#elif __VJ_version == 2000003
#endif
        for( int i = 1; i < argc; ++i )    // Configure the kernel
        {
            if( std::string( argv[ i ] ) == std::string( "-VESDesktop" ) )
            {
                //skip the resolutions
                i = i + 2;
            }
            else if( std::string( argv[ i ] ) == std::string( "-VESCluster" ) )
            {
                //Skip the master computer name
                i = i + 1;
            }
            else if( std::string( argv[ i ] ) == std::string( "-VESRTT" ) )
            {
                //Skip the rtt flag
            }
            else
            {
                kernel->loadConfigFile( argv[i] );
            }
        }

        AppWrapper* appWrapper = new AppWrapper( argc, argv, vjobsWrapper );

        kernel->waitForKernelStop();              // Block until kernel stops

        //Block and wait for juggler and the orb to stop
        //orb->run();
        /*while( appWrapper->JugglerIsRunning() )
        {
            vpr::System::msleep( 10 );  // one-second delay
            if( orb->work_pending() )
            {
                orb->perform_work();
            }
        }*/

        //appWrapper->m_thread->new_thread->join();
        delete appWrapper;
    }
    catch ( CORBA::SystemException& ex )
    {
        std::cerr << "Caught CORBA::SystemException." << std::endl
            << ex._info().c_str() << std::endl 
            << " The nameserver is probably not started yet or " << std::endl
            << " the computer name and port number passed into " << std::endl
            << " VE-Xplorer do not match the computer name and " << std::endl
            << " port number specified in the launcher.      " << std::endl;
    }
    catch( CORBA::Exception& ex )
    {
        std::cerr << "Caught CORBA::Exception." << std::endl
            << ex._info().c_str() << std::endl ;
    }
    catch( std::exception& e )
    {
        std::cerr << "VE-Xplorer Init: Caught unknown exception." << std::endl
            << e.what() << std::endl;
        for( int i = 1; i < argc; ++i )
        {
            std::cerr << "argv[ " << i << " ] = " << argv[ i ] << std::endl;
        }
        std::cerr << "NOTE: If you are running VE-Suite from the " << std::endl
            << "launcher and are receiving this error be sure " << std::endl
            << "that you are using the --dev flag for "
            << "development work." << std::endl;
    }

    return 0;
}
