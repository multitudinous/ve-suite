#include "cfdVeController.h"

cfdVeController::cfdVeController(char* filename)
{
    this->param_file = ( cfdVeChangerManager::getInstance()).getParamFile(filename);
    this->mVjDesigner = new VjDesignObs_i();

}

cfdVeController::~cfdVeController()
{
  delete this->mVjDesigner;
  delete this->param_file;
  delete this->mVjDesigner;

}

bool cfdController::attach()
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
 
      this->setCORBAVariables( naming_context.in(), orb.in(), poa.in() );
      
      VjDesignObs_var vjdesignobs = mVjDesigner->_this();
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

void cfdVeController::runControllerThread(void *unused)
{

   while(1)
   {
      while(!this->connectingToNamingService)
      {
         this->connectionToNamingService = this->attach();

      }
      /*
         This is the place where users can change to fix their own requirements
         Basically, opID decides what kind of operation users want. According to the opID, the controller gets the new information he wants.
         And then the controller notify its observers (model and view)
      */
      this->opID = this->mVjDesigner->getID();
      if(this->opID == UPDATE_INTERACTIVE_DISPLAY)
      {
        ( cfdVeChangerManager::getInstance()).setOPID(this->opID);
         this->setInteractiveDisplay();
         notifyObserver();
         this->reSetopID(-1);
      }
      else if (this->opID == UPDATE_INTERACTIVE_DESIGN) //change design variables
      {
        ( cfdVeChangerManager::getInstance()).setOPID(this->opID);
        this->setInteractiveDesign();
        notifyObserver();
        this->reSetopID(-1);      
      }
      else if (this->opID == UPDATE_INTERACTIVE_GA)
      {
         ( cfdVeChangerManager::getInstance()).setOPID(this->opID);
         this->setInteractiveGA();
         notifyObserver();
         this->reSetopID(-1);
      }
      else
      {  
         mValueLock.acquire();
         {
            this->reset();
         }
         std::cout<<"There is no operation from users.."<<std::endl;
         this->reSetopID(-1);
      }

   }

}

void cfdVeController::reSetopID(int value)
{
   this->opID = value;
}

void cfdVeController::reset()
{
   this->designparamschanged = false;
   this->converttovtk = false;
   this->interactiveGA = false;
   this->savegooddesign = false;
   this->statechanged = false;
      
}

void cfdVeController::setInteractiveDisplay()
{
   
   ( cfdVeChangerManager::getInstance()).setVRserverName(this->mVjDesigner->getVRserverName());
   ( cfdVeChangerManager::getInstance()).setModeForOldModel(this->mVjDesigner->getMode_oldModel());
   if(( cfdVeChangerManager::getInstance()).getModelForOldModel())
   {
      ( cfdVeChangerManager::getInstance()).setTransForOldModel(this->mVjDesigner->getTransForOldModel()); 
      ( cfdVeChangerManager::getInstance()).setRotForOldModel(this->mVjDesigner->getRotForOldModel());
      
   }
   ( cfdVeChangerManager::getInstance()).setNewVtkFileName(this->mVjDesigner->getNewVtkFileName());
  }

void cfdVeController::setInteractiveDesign()
{
   //int count;
   //count = this->param_file->getNumDesignParams();
   
   ( cfdVeChangerManager::getInstance()).setActiveDesignParams(this->mVjDesigner->getActiveDesignParams());
   ( cfdVeChangerManager::getInstance()).setDesignParams(this->mVjDesigner->getDesignParams());  
   ( cfdVeChangerManager::getInstance()).setSaveMode(this->mVjDesigner->getSaveMode());
   
   ( cfdVeChangerManager::getInstance()).setDesignDisplayMode(this->mVjDesigner->getDispalyMode());

   if(( cfdVeChangerManager::getInstance()).getDisplayMode())
   {
      this->setInteractiveDisplay();
   }
  
}

void cfdVeController::setInteractiveGA()
{
   ( cfdVeChangerManager::getInstance()).setCheckGene(this->mVjDesigner->getCheckGene());
   ( cfdVeChangerManager::getInstance()).setCrossoverRate(this->mVjDesigner->getCrossoverRate());
   ( cfdVeChangerManager::getInstance()).setMutationRate(this->mVjDesigner->getMutationRate());
   ( cfdVeChangerManager::getInstance()).setGADisplayMode(this->mVjDesigner->getGADisplay());
   
   if(( cfdVeChangerManager::getInstance()).getGADisplayMode())
   {
      this->setInteractiveDisplay();
   }
}
