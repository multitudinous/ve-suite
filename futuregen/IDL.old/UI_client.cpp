#include "moduleC.h"
#include "orbsvcs/CosNamingC.h"
#include "UI_i.h"

//This UI_client act as the executive's client 
//This UI is also the UI servant for the executive' UI client

int main (int argc, char* argv[])
{
  std::string UINAME = "UIclientYang";
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
    CORBA::Object_var poa_object =
      orb->resolve_initial_references ("RootPOA"); // get the root poa

    PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_object.in());
    PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
    poa_manager->activate ();
    
    //Create the Servant
    Body_UI_i ui_i(exec.in(), UINAME);

    //Activate it to obtain the object reference
    Body::UI_var ui = ui_i._this();
     
    CosNaming::Name UIname(1);
    UIname.length(1);
    UIname[0].id = CORBA::string_dup (UINAME.c_str());
   
    //Bind the object
    naming_context->bind(UIname, ui.in());
    
    //Call the Executive CORBA call to register it to the Executive
    exec->RegisterUI(ui_i.UIName_.c_str());
    
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
