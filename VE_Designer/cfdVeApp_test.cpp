#include "cfdVeApp.h"
#include "cfdChangeManager.h"
#include "cfdGAserver.h"
#include "cfdControllerObserver.h"
#include ""

cfdVeApp::cfdVeApp(char* param_filename)
   :mGAserver( new cfdGAServer(param_filename) ), mChangeManager( new cfdChangeManager(param_filename) )
{
   mValueLock.acquire();
   {
      this->ChangeManagerFunc = new vpr::ThreadMemberFunctor<cfdChangeManager>(mChangeManager, &cfdChangeManager::runChangeManagerThread);
      this->GAServerFunc = new vpr:: ThreadMemberFunctor<cfdGAServer>(mGAServer, &cfdGAServer::runGAServerThread);
      this->vjTh[0] = new vpr::Thread(this->ChangeManagerFunc);
      this->vjTh[1] = new vpr::Thread(this->GAServerFunc);
   }
   
   mValueLock.release();
   mValueLock.yield();
}

cfdVeApp::cfdVeApp(char* param_filename, char *displayhost)
   :mGAServer( new cfdGAServer(param_filename, displayhost) ), mChangeManager( new cfdChangeManager(param_filename, displayhost))
{
   mValueLock.acquire();
   {
      this->ChangeManagerFunc = new vpr::ThreadMemberFunctor<cfdChangeManager>(mChangeManager, &cfdChangeManager::runChangeManagerThread);
      this->GAServerFunc = new vpr:: ThreadMemberFunctor<cfdGAServer>(mGAServer, &cfdGAServer::runGAServerThread);

      this->vjTh[0] = new vpr::Thread(this->ChangeManagerFunc);
      this->vjTh[1] = new vpr::Thread(this->GAServerFunc);
   }
   
   mValueLock.release();
   mValueLock.yield();

}

cfdVeApp::~cfdVeApp()
{

}

int main(int argc, char* argv[])
{

  
   try
   {
      int temp =0;
   
      CORBA::ORB_var orb = CORBA::ORB_init(temp,0,"omniORB4");
      try
      {
         CORBA::Object_var naming_context_object = orb->resolve_initial_references ("NameService");
         CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in ());

         if(CORBA::is_nil(naming_context))
         {
            std::cerr<<"Failed to narrow the naming context"<<std::endl;    
            return 0;
         }
      }
      catch(CORBA::ORB::InvalideName& ex)
      {
         std::cerr<<"Service required is invalid [does not exist]."<<std::endl;
         return 0;
      }
            
      CORBA::Object_var poa_object =
            orb->resolve_initial_references ("RootPOA"); // get the root poa

      PortableServer::POA_var poa = PortableServer::POA::_narrow(obj);
      PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
      poa_manager->activate ();
 
      cfdControllerObserver* ControllerObserver = new cfdControllerObserver( );  // Delcare an instance of my application
      ControllerObserver->SetCORBAVariables( naming_context.in(), orb.in(), poa.in() );

      VjDesignObs_var vjdesignobs = ControllerObserver->_this();
      CORBA::String_var sior(orb->object_to_string(vjdesignobs.in()));
      std::cout << "|  IOR of the server(cfdVeApp) side : " << endl << sior << endl;
      
      try
      {
         CosNaming::Name name;
         name.length(1);

         name[0].id   = (const char*) "DesignModel";
         name[0].kind = (const char*) "DesignModelObject";
   //Bind the object
         try	
         {
            naming_context->bind(name, vjdesignobs.in());
         }
         catch(CosNaming::NamingContext::AlreadyBound& ex)
         {
            naming_context->rebind(name, vjdesignobs.in());
         }

      
      }

      catch(CORBA::COMM_FAILURE& ex)
      {
         std::cerr<<"Caught system exception COMM_FAILURE -- unable to contact the naming service "<<std::endl;
         return 0;
      }

      catch(CORBA::SystemException& )
      {
         std::cerr<<"Caught a CORBA::SystemException while using the naming sevice "<<std::endl;
         return 0;
      }  
           
   }
   catch(CORBA::SystemException& )
   {
      std::cerr<<"Caught CORBA::SystemException."<<std::endl;
      return 0;
   }
   catch(CORBA::Exception& )
   {
      std::cerr<<"Caught CORBA::Exception. "<<std::endl;
      return 0;
   }
   catch(omniORB::fatalException& fe)
   {
      std::cerr<<"Caught omniORB::fatalException:"<<std::endl;
      std::cerr<<" file: "<<fe.file()<<std::endl;
      std::cerr<<" line: "<<fe.line()<<std::endl;
      std::cerr<<" mesg: "<<fe.errmsg()<<std::endl;
      return 0;
   }
   catch(...)
   {
      std::cerr<<"Caught unknown exception."<<std::endl;
      return 0;
   }

   //ga evolution
   //
   char file_name[100];
   printf("|   Enter parameter filename: ");
   scanf("%s",filein_name);
   std::cout << std::endl;
   std::cout << "|   Initializing........................... Parameter File Reader |" << std::endl;

   unsigned int seed =16000;
   srand48(seed);
   
   if(argc!=2)
   {
      cfdVeApp *myveapplication = new cfdVeApp(file_name, argc);
   }
   else
   {
      cfdVeApp * myveapplication = new cfdVeApp(file_name, argv[1]);
   }

   mGAserver->evolve();

   return 1;
      
 }




