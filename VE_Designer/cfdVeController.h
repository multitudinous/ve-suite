#ifndef CFD_VECONTROLLER_H
#define CFD_VECONTROLLER_H

#include "cfdChangeManager.h"
#include <VjDesignObs_i.h>
#include "cfdSubjectBase.h"
#include "cfdVeEnum.h"

class cfdVeController:public cfdSubjectBase, public VjDesignObs_i
{
   public:
      cfdVeController(char*);
      ~cfdVeController();
      void runControllerThread(void * unused);
      bool attach();
      void reSetOpID(int value);

      /*
      functions for sending information
      */
      void setInteractiveDisplay();
      void setInteractiveDesign();
      void setInteractiveGA();
      
   private:
      cfdVeReadParam* param_file; 
      bool connectingToNamingService;
      int opID;
      VjDesignObs_i* mVjDesigner;

};




#endif
