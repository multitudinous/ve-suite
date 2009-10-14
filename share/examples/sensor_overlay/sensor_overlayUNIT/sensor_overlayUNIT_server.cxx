/*
 Adam J. Shuttleworth
 Multiple DAQ used in VE-Suite Unit plugin
 18September09
 
 To run:
 
 sensor_overlayUNIT_server -ORBInitRef NameService=corbaloc:iiop:localhost:1239/NameService
 */


#include "Sensor_DataI.h"
#include "orbsvcs/CosNamingC.h"

int main (int argc, char* argv[])
{
	try {
        // First initialize the ORB, that will remove some arguments...
		CORBA::ORB_var orb = CORBA::ORB_init (argc, argv,"" /* the ORB name, it can be anything! */);
		CORBA::Object_var poa_object =orb->resolve_initial_references ("RootPOA");
		PortableServer::POA_var poa =PortableServer::POA::_narrow (poa_object.in ());
		PortableServer::POAManager_var poa_manager =poa->the_POAManager ();
		poa_manager->activate ();
		
		// Create the servant
		Sensor_Data_i servant;
		
		
		
		
		// Activate it to obtain the object reference
		Sensor_Data_var sensor_data =
		servant._this ();
		
		
		
		
		// Get the Naming Context reference
		CORBA::Object_var naming_context_object =
		orb->resolve_initial_references ("NameService");
		CosNaming::NamingContext_var naming_context =
		CosNaming::NamingContext::_narrow (naming_context_object.in ());
		
		// Create and initialize the name.
		CosNaming::Name name (1);
		name.length (1);
		name[0].id = CORBA::string_dup ("Sensor Data");
		
		// Bind the object
		naming_context->bind (name, sensor_data.in ());
		
		
		
		
		
		
		std::cout<<"before orb run"<<std::endl;
		
		
		orb->run ();
		
		// Destroy the POA, waiting until the destruction terminates
		// poa->destroy (1, 1);
		orb->destroy ();
		
		std::cout<<"after orb destroy"<<std::endl;
		
		return 0;
	}
	catch (CORBA::Exception &) {
		cerr << "CORBA exception raised!" << endl;
	}
	return 1;
}
