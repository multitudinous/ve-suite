#ifndef CFD_CONTROLLEROBSERVER_H
#define CFD_CONTROLLEROBSERVER_H

#include "cfdVeEnum.h"
#include <VjDesignObs_i.h>
#include "cfdSubject.h"

class cfdSillyClass:public VjDesignObs_i
{
   public:
      cfdSillyClass()
      {
         std::cout<<"ready to connect to namingservice"<<std::endl;
      }
      ~ cfdSillyClass(){}

};

class cfdControllerObserver:public cfdSubject
{
   public:
      cfdControllerObserver(char* filename);
     
      ~cfdControllerObserver();
      bool attach();
      void runControllerObserverThread(void *unused);
      void setID(int);
      int  getID();
      void reset();
      void setCORBAVariables(CosNaming::NamingContext_ptr, CORBA::ORB_ptr, PortableServer::POA_ptr);

   public:
      cfdVeReadParam* param_file;
      cfdSillyClass*  msillyclass;
      
   private:
      bool connectToNamingService;
      bool statechanged;  
      bool designparamschanged;
      bool converttovtk;
      bool savegooddesign;
      bool interactiveGA;
      int opID;
      char* vrservername;
      char* newvtkfilename;

      vpr::Mutex  mValueLock;
      CosNaming::NamingContext_var naming_context;
      CORBA::ORB_var orb;
      PortableServer::POA_var poa;
      
   public:
      bool getState();
      bool getDesignParamsChangedFlag();
      bool getConvertToVtkFlag();
      bool getSaveDesignFlag();
      bool getInteractiveGAFlag();
      char* getVRserverName();
      char* getNewVtkFileName();
      int  getOpID();


};

#endif
