#include "moduleC.h"
#include "orbsvcs/CosNamingC.h"
#include "Unit_i.h"
#include <iostream>

//This Unit_client act as the executive's client 
//This Unit is also the Unit servant for the executive' Unit client

int main (int argc, char* argv[])
{
  std::string UNITNAME = "CMU_Unit";
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
    poa_manager->activate ();
    
    //Create the Servant
    Body_Unit_i unit_i(exec.in(), UNITNAME);

    //Activate it to obtain the object reference
    Body::Unit_var unit = unit_i._this();
     
    CosNaming::Name Unitname(1);
    Unitname.length(1);
    Unitname[0].id = CORBA::string_dup (UNITNAME.c_str());

    //Bind the object
	try {
		naming_context->bind(Unitname, unit.in());
    } catch(CosNaming::NamingContext::AlreadyBound& ex){
			naming_context->rebind(Unitname, unit.in());
	}

    //Call the Executive CORBA call to register it to the Executive
    exec->RegisterUnit(unit_i.UnitName_.c_str());
    
    orb->run();

    // Destroy the POA, waiting until the destruction terminates
    poa->destroy (1, 1);
    // Finally destroy the ORB
    orb->destroy();
  }
  catch (CORBA::Exception &ex) {
	  std::cerr << "CORBA exception raised!" << std::endl;
  }
  return 0;
}
