#ifndef CFD_VEAPP_H
#define CFD_VEAPP_H

#include "cfdChangeManager.h"
#include "cfdGAServer.h"
#include "cfdControllerObserver.h"
#include "VjDesignObs_i.h"

#include <vpr/vpr.h>
#include <vpr/Thread/Thread.h>
#include <vpr/Sync/Mutex.h>
#include <vpr/Sync/Guard.h>
#include <vpr/Util/Debug.h>

class cfdVeApp
{
   public:
      cfdVeApp(char* );
      cfdVeApp(char*, char* );
      ~cfdVeApp();
   
   public:
   //classes and variables for multithreading
      vpr::ThreadMemberFunctor<cfdChangeManager> *ChangeManagerFunc;
      vpr::ThreadMemberFunctor<cfdGAServer>  *GAServerFunc;
      vpr::Thread *vjTh[2];

   public:
      cfdGAServer* mGAserver;
      cfdChangeManager* mChangeManager;
      
   private:
      vpr::Mutex     mValueLock;
        
};

#endif
