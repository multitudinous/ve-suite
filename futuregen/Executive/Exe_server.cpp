#include "moduleC.h"
#include <orbsvcs/CosNamingC.h>
//#include "ace/streams.h"
#include "Executive_i.h"
#include <iostream>
using namespace std;

//This Exe_server act as the servant of the Executive
//This Exe_server is also the Unit's client and the UI's client.

int main (int argc, char* argv[])
{ 
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
    CORBA::Object_var poa_object =orb->resolve_initial_references ("RootPOA"); // get the root poa
    PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_object.in());
    PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
    poa_manager->activate ();
     
    //Create the Servant, pass in the pointer of the naming context
    Body_Executive_i exec_i(naming_context.in());

    //Activate it to obtain the object reference
    Body::Executive_var exec = exec_i._this();
     
    CosNaming::Name name(1);
    name.length(1);
    name[0].id = CORBA::string_dup ("Executive");
   
    //Bind the object
    try {
      naming_context->bind(name, exec.in());
    } catch (CosNaming::NamingContext::AlreadyBound& ex){
      naming_context->rebind(name, exec.in());
    }
    orb->run();

    // Destroy the POA, waiting until the destruction terminates
    poa->destroy (1, 1);
    // Finally destroy the ORB
    orb->destroy();
    }catch (CORBA::Exception &) {
    cerr << "CORBA exception raised!" << endl;
  }
  return 0;
}
