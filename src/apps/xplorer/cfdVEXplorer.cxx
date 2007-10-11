
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <apps/xplorer/cfdAppWrapper.h>
#include <apps/xplorer/cfdVjObsWrapper.h>

#include <apps/ce/Executive_i.h>
#include <orbsvcs/CosNamingC.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>

#include <iostream>
#include <cstdlib>
#include <vpr/System.h>

#include <ves/xplorer/cfdThread.h>
#include <ves/VEConfig.h>
using namespace VE_Xplorer;

int main(int argc, char* argv[])
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
        CORBA::ORB_var orb = CORBA::ORB_init( argc, argv,"VE_Suite_ORB" );

        //Here is the part to contact the naming service and get the reference for the executive
        CORBA::Object_var naming_context_object =
        orb->resolve_initial_references ("NameService"); 
        CORBA::String_var sior1(orb->object_to_string( naming_context_object.in() ) );
        std::cout << "|\tIOR of the server side : " << std::endl << sior1 << std::endl;
        CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in());

        //Here is the code to set up the server
        CORBA::Object_var poa_object = orb->resolve_initial_references ("RootPOA"); // get the root poa
        PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_object.in());
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

        PortableServer::POA_var child_poa = poa->create_POA ("childPOA",
            poa_manager.in(), policies);

        // Creation of childPOA is over. Destroy the Policy objects. 
        for( CORBA::ULong i = 0; i < policies.length (); ++i)
        {
            policies[i]->destroy();
        }

        poa_manager->activate();

        cfdVjObsWrapper* vjobsWrapper = new cfdVjObsWrapper();
        vjobsWrapper->init( naming_context.in(), NULL, child_poa.in(), NULL, argc, argv );
        cfdAppWrapper* appWrapper = new cfdAppWrapper( argc, argv, vjobsWrapper );

        //orb->run();
        while( appWrapper->JugglerIsRunning() )
        {
            vpr::System::msleep( 10 );  // one-second delay
            if( orb->work_pending() )
            {
                orb->perform_work();
            }
        }

        appWrapper->_thread->new_thread->join();
        delete appWrapper;
    }
    catch( CORBA::SystemException& ) 
    {
        std::cerr << "Caught CORBA::SystemException." << std::endl
                  << " The nameserver is probably not started yet or " << std::endl
                  << " the computer name and port number passed into " << std::endl
                  << " VE-Xplorer do not match the computer name and " << std::endl
                  << " port number specified in the VES script.      " << std::endl
                  << " Please start the nameserver with -> VES -nserv . " << std::endl;
    }
    catch( CORBA::Exception& ) 
    {
        std::cerr << "Caught CORBA::Exception." << std::endl;
    }
    catch ( ... ) 
    {
        std::cerr << "Caught unknown exception." << std::endl;
    }

    return 0;
}
