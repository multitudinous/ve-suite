//***insert license here***\\



#include "cfdWebServices.h"
#include <iostream>
#include <string>
#include <sstream>

#include <vrj/Util/Debug.h>
#include <vpr/System.h>

#include <orbsvcs/CosNamingC.h>


cfdWebServices::cfdWebServices( CosNaming::NamingContext* inputNameContext, PortableServer::POA* childPOA )
{
  this->namingContext = inputNameContext;
   try
   {
      XMLPlatformUtils::Initialize();
   }
   catch(const XMLException &toCatch)
   {
      std::cerr << "Error during Xerces-c Initialization.\n"
            << "  Exception message:"
            << XMLString::transcode(toCatch.getMessage()) << std::endl;
      return;
   }

   this->_doneWithCalculations = true;
   this->runGetEverythingThread = true;
   //this->updateNetworkString = false;
   //this->thread = 0;

   //this->naming_context = CosNaming::NamingContext::_duplicate( 
   //   corbaManager->_vjObs->GetCosNaming()->naming_context );
   this->masterNode = new cfdGroup();
   this->masterNode->SetName( "cfdExecutive_Node" );
   cfdPfSceneManagement::instance()->GetWorldDCS()->AddChild( this->masterNode );

   av_modules = new cfdVEAvail_Modules();
   network = new Network();

   //time_t* timeVal;
   long id = (long)time( NULL );
   std::ostringstream dirStringStream;
   dirStringStream << "VEClient" << id;
   std::string UINAME = dirStringStream.str();
   bool isOrbInit = false;

   exec = NULL;
   this->uii = 0;
   if (!isOrbInit)
   {
      //init_orb_naming();
      isOrbInit = true;
   }

   try
   {
      CosNaming::Name name(1);
      name.length(1);
      name[0].id = CORBA::string_dup ("Executive");
    
      CORBA::Object_var exec_object = this->namingContext->resolve(name);
      this->exec = Body::Executive::_narrow(exec_object.in());

      //Create the Servant
      uii = new Body_UI_ithis->(exec, UINAME);
      //Body_UI_i ui_i( UINAME);

      PortableServer::ObjectId_var id = 
         PortableServer::string_to_ObjectId( CORBA::string_dup( "cfdExecutive" ) ); 
    
      //activate it with this child POA 
      child_poa->activate_object_with_id( id.in(), &(*ui_i) );

      // obtain the object reference
      Body::UI_var unit =  
      Body::UI::_narrow( childPOA->id_to_reference( id.in() ) );

      // Don't register it to the naming service anymore
      // the bind call will hang if you try to register
      // Instead, the new idl make the ref part of the register call 
      // Naming Service now is only used for boot trap 
      // to get the ref for Executive

      //Call the Executive CORBA call to register it to the Executive
      exec->RegisterUI( ui_i->UIName_.c_str(), unit.in() );
      std::cout << "|\tConnected to the Executive " << std::endl;   
	   //this->thread = new cfdThread();
      //thread->new_thread = new vpr::Thread( new vpr::ThreadMemberFunctor< cfdExecutive > ( this, &cfdExecutive::GetEverything ) );
   } 
   catch (CORBA::Exception &) 
   {      
      std::cerr << "|\tExecutive not present or VEClient registration error" << std::endl;
   }

}

cfdWebServices::~cfdWebServices()
{
   delete uii;
   delete network;
   delete masterNode;
}



