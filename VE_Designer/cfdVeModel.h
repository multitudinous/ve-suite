#ifndef CFD_VEMODEL_H
#define CFD_VEMODEL_H

#include "cfdSubjectBase.h"
#include "cfdObserverBase.h"
#include "cfdChangeManager.h"
#include "cfdGAserver.h"
#include "cfdVeController.h"
#include "cfdDirectoryHandler.h"
#include "cfdVeEnum.h"

/*
 *    cfdVeModel is the interface to the Simulator. It is part of MVC pattern.
 *
 *    cfdVeModel is the subject of cfdVeView
 *                  the observer of cfdVeController
 *
 * 
 */

class cfdVeModel : public cfdSubjectBase, public cfdObserverBase
{
      cfdVeModel(cfdVeController* controller,char* filename);
      ~cfdVeModel();
      void update(cfdSubjectBase*);
      void getOPInfoFromChangeManager();
      void runInteractiveDesignSaveOP(void* unused);
      void runRegularOPThread(void* unused);
      void runInteractiveDesignSaveOP(void* unused);
      void runInteractiveDesignDisplayOP(void* unused);
      void runInteractiveGAUpdateOP(void* unused);
      void runInteractiveGAGeneCheckOP(void* unused);

   public:
      bool isVtkDataReady();
      cfdVeController* _modelsubject;
      cfdChangeManager* changemanager;
      cfdVeReadParam* param_file;

   private:
      vpr::ThreadMemberFunctor<cfdVeModel>  *interactiveOPFunc;
      vpr::ThreadMemberFunctor<cfdVeModel>   *regularOPFunc;
      vpr::Thread *OPTh[2];

      vpr::Mutex     mValueLock;

      /*
      

      *
      */
      cfdGAserver*  GAserver;
      cfdGAserver*  interactiveGAserver;

   private:
      int   opID;
      bool  interactiveOPthread_created;
      bool  interactiveddisplayactived;
      float* currentdesignparams;
      int checkedgene;
  


};



#endif
