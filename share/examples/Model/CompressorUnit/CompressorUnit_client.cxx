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
#include "moduleC.h"
#include "orbsvcs/CosNamingC.h"
#include "CompressorUnit_i.h"
#include "tao/BiDir_GIOP/BiDirGIOP.h"   
//This Unit_client act as the executive's client 
//This Unit is also the Unit servant for the executive' Unit client

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
  std::string UNITNAME = "Compressor";
  try {
    // First initialize the ORB, 
    CORBA::ORB_var orb =
      CORBA::ORB_init (argc, argv,
                       "" /* the ORB name, it can be anything! */);

    //Here is the part to contact the naming service and get the reference for the executive
    CORBA::Object_var naming_context_object =
      orb->resolve_initial_references ("NameService");
    CosNaming::NamingContext_var naming_context =
      CosNaming::NamingContext::_narrow (naming_context_object.in ());
    
    CosNaming::Name name(1);
    name.length(1);
    name[0].id = CORBA::string_dup ("Executive");
    
    CORBA::Object_var exec_object = naming_context->resolve(name);

    //Now downcast the object reference to the appropriate type
    
    Body::Executive_var exec = Body::Executive::_narrow(exec_object.in());
      
    // other client code
    
    //...........
    
    //Here is the code to set up the server
    CORBA::Object_var poa_object = orb->resolve_initial_references ("RootPOA"); // get the root poa
    PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_object.in());
    PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
   	CORBA::PolicyList policies (1);
    policies.length (1);

    CORBA::Any pol;
    pol <<= BiDirPolicy::BOTH;
    policies[0] =
        orb->create_policy (BiDirPolicy::BIDIRECTIONAL_POLICY_TYPE,
                            pol
                            ACE_ENV_ARG_PARAMETER);
    ACE_TRY_CHECK;

    // Create POA as child of RootPOA with the above policies.  This POA
    // will receive request in the same connection in which it sent
    // the request
    PortableServer::POA_var child_poa =
             poa->create_POA ("childPOA",
                              poa_manager.in (),
                              policies
                              ACE_ENV_ARG_PARAMETER);
    ACE_TRY_CHECK;

    // Creation of childPOA is over. Destroy the Policy objects.
    for (CORBA::ULong i = 0; i < policies.length (); ++i)
	{
       policies[i]->destroy (ACE_ENV_SINGLE_ARG_PARAMETER);
       ACE_TRY_CHECK;
    }

    poa_manager->activate (ACE_ENV_SINGLE_ARG_PARAMETER);
    ACE_TRY_CHECK;
	  
    //Create the Servant
    Body_Unit_i unit_i(exec.in(), UNITNAME);
    
	PortableServer::ObjectId_var id =
        PortableServer::string_to_ObjectId (CORBA::string_dup(UNITNAME.c_str()));

    child_poa->activate_object_with_id (id.in (),
                                          &unit_i
                                          ACE_ENV_ARG_PARAMETER);

    //Activate it to obtain the object reference
    Body::Unit_var unit =  Body::Unit::_narrow(child_poa->id_to_reference (id.in ()
                                    ACE_ENV_ARG_PARAMETER));
    ACE_TRY_CHECK;
            
     
//    CosNaming::Name Unitname(1);
//    Unitname.length(1);
//    Unitname[0].id = CORBA::string_dup (UNITNAME.c_str());
    //Bind the object

//	try	{
//      naming_context->bind(Unitname, unit.in());
//    }catch(CosNaming::NamingContext::AlreadyBound& ex){
//      naming_context->rebind(Unitname, unit.in());
//    }

    
    //Call the Executive CORBA call to register it to the Executive
    exec->RegisterUnit(unit_i.UnitName_.c_str(), unit.in(), 0); //0 means a normal module
    
    orb->run();

    // Destroy the POA, waiting until the destruction terminates
    poa->destroy (1, 1);
    // Finally destroy the ORB
    orb->destroy();
  }
  catch (CORBA::Exception &) {
    cerr << "CORBA exception raised!" << endl;
    
  }
  return 0;
}
