#include "cfdControllerObserver.h"
cfdControllerObserver::cfdControllerObserver(char* filename)
{
   std::cout<<"Hello,the cfdControllerObserver is ready to get state informatiom from GUI...."<<std::endl;
    this->param_file =cfdVeReadParamFactory::getInstance().getParamFile(filename);
    this->connecttoCobra = this->attach();
    if(!this->connecttoCobra)
    {
       std::cerr<<"[ERR] Can't connect to NamingService"<<std::endl;
       exit(1);
    }
}


cfdControllerObserver::~cfdControllerObserver()
{

}

void cfdControllerObserver::setCORBAVariables( CosNaming::NamingContext_ptr naming, CORBA::ORB_ptr orb, PortableServer::POA_ptr poa )
{
   this->naming_context = CosNaming::NamingContext::_duplicate( naming );
   this->orb = CORBA::ORB::_duplicate( orb );
   this->poa = PortableServer::POA::_duplicate( poa );
}

bool cfdControllerObserver::attach()
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
      catch(CORBA::ORB::InvalidName& ex)
      {
         std::cerr<<"Service required is invalid [does not exist]."<<std::endl;
         return 0;
      }
            
      CORBA::Object_var poa_object =
            orb->resolve_initial_references ("RootPOA"); // get the root poa

      PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_object.in());
      PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
      poa_manager->activate ();
 
      //this->controllerobserver = new cfdControllerObserver( );  // Delcare an instance of my application
      msillyclass = new cfdSillyClass();
      this->setCORBAVariables( naming_context.in(), orb.in(), poa.in() );

      //VjDesignObs_var vjdesignobs = controllerobserver->_this();
      VjDesignObs_var vjdesignobs = msillyclass->_this();
      CORBA::String_var sior(orb->object_to_string(vjdesignobs.in()));
      std::cout << "|  IOR of the server(cfdVeApp) side : " << std::endl << sior << std::endl;
      
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

   return 1;
}




void cfdControllerObserver::setID(int value)
{
   this->opID = value;

}

void cfdControllerObserver::runControllerObserverThread(void *unused)
{
   while(1)
   {
      while(!this->connectionToNamingService)
      {
         this->connectionToNamingService = this->attach();

      }
      this->opID = this->getID();
      if(this->opID == UPDATE_INTERACTIVE_DISPLAY)
      {
         mValueLock.acquire();
         {
            this->reset();
            this->param_file->setVRserverName(this->getVRserverName());
            this-> converttovtk = true;
            this->statechanged = true;
            notifyObserver();
         }
         mValueLock.release();
         
      }
      else if (this->opID == UPDATE_INTERACTIVE_DESIGN) //change design variables
      {
         mValueLock.acquire();
         {
            this->reset();
            this->designparamschanged = true;
            this->converttovtk =true;
            this->statechanged = true;
            notifyObserver();
         }
         mValueLock.release();
               
      }
      else if (this->opID == UPDATE_INTERACTIVE_INITIAL_SEARCH)
      {
         mValueLock.acquire();
         {
            this->reset();
            this->designparamschanged = true;
            this->converttovtk = true;
            this->savegooddesign = true;
            this->statechanged = true;
            notifyObserver();
         }
         mValueLock.release();
      }
      else if (this->opID == UPDATE_INTERACTIVE_GA)
      {
         mValueLock.acquire();
         {
            this->reset();
            this->interactiveGA = true;
            this->converttovtk = true;
            this->statechanged = true;
            notifyObserver();
         }
         mValueLock.release();
      }
      else
      {
         mValueLock.acquire();
         {
            this->reset();
         }
         std::cout<<"There is no operation from users.."<<std::endl;
      }

   }
}

void cfdControllerObserver::reset()
{
   this->designparamschanged = false;
   this->converttovtk = false;
   this->interactiveGA = false;
   this->savegooddesign = false;
   this->statechanged = false;
      
}

bool cfdControllerObserver::getState()
{
   return this->statechanged;
}

bool cfdControllerObserver::getDesignParamsChangedFlag()
{
   return this->designparamschanged;
}

bool cfdControllerObserver::getConvertToVtkFlag()
{
   return this->converttovtk;
}

bool cfdControllerObserver::getSaveDesignFlag()
{
   return this->savegooddesign;
}

bool cfdControllerObserver::getInteractiveGAFlag()
{
   return this->interactiveGA;

}

int cfdControllerObserver::getOpID()
{
   return this->opID;
}

char* cfdControllerObserver::getVRserverName()
{
   return this->vrservername;
}

char* cfdControllerObserver::getNewVtkFileName()
{
   return this->newvtkfilename;
}
