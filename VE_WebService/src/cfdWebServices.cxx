//***insert license here***\\



#include "cfdWebServices.h"

bool cfdWebServices::initCorba(int &argc, char* argv[])
{
   #ifdef _TAO
      this->_orb = CORBA::ORB_init( argc, argv,"VE_Suite_ORB" );
   #else
      this->_orb = CORBA::ORB_init( argc, argv,"omniORB4" ); 
   #endif // _TAO
      if ( CORBA::is_nil( orb.in() ) )
         return false;
     
      //Here is the part to contact the naming service and get the reference for the executive
      CORBA::Object_var naming_context_object =
        orb->resolve_initial_references ("NameService"); 
      CORBA::String_var sior1(orb->object_to_string( naming_context_object.in() ) );
      std::cout << "|  IOR of the server side : " << std::endl << sior1 << std::endl;
      CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in());

      //Here is the code to set up the server
      CORBA::Object_var poa_object = orb->resolve_initial_references ("RootPOA"); // get the root poa
      PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_object.in());
      PortableServer::POAManager_var poa_manager = poa->the_POAManager();

   #ifdef _TAO
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

      PortableServer::POA_var child_poa = poa->create_POA ("childPOA",
                          poa_manager.in(),
                          policies);

      // Creation of childPOA is over. Destroy the Policy objects. 
      for (CORBA::ULong i = 0; i < policies.length (); ++i)
      {
            policies[i]->destroy();
      }
   #endif // _TAO

      poa_manager->activate();

      this->_orb->run();
      return true;
}

