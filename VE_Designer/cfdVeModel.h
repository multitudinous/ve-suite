#ifndef CFD_VEMODEL_H
#define CFD_VEMODEL_H

#include "cfdSubjectBase.h"
#include "cfdObserverBase.h"
#include "cfdChangeManager.h"
#include "cfdGAserver.h"

/*
 *    cfdVeModel is the interface to the Simulator. It is part of MVC pattern.
 *
 *    cfdVeModel is the subject of cfdVeView
 *                  the observer of cfdVeController
 *
 * 
 */

cfdVeModel(cfdVeController* controller,char* filename);
      ~cfdVeModel();
      void update(cfdSubjectBase*);
      void getOPInfoFromChangeManager();
      void runInteractiveOPThread(void* unused);
      void runRegularOPThread(void* unused);

   public:
      bool isVtkDataReady();
   private:
      cfdVeController* _modelsubject;
      cfdChangeManager* changemanager;
      cfdVeReadParam* param_file;
      
      vpr::ThreadMemberFunctor<cfdVeModel>  *interactiveOPFunc;
      vpr::ThreadMemberFunctor<cfdVeModel>   *regularOPFunc;
      vpr::Thread *OPTh[2];

      /*
      

      *
      */
      cfdGAserver*  GAserver;
      cfdGAserver*  interactiveGAserver;

   private:
      int   opID;
      bool  interactiveOPthread_created;

      int checkedgene;
      float* currentdesignparams; 
  


};



#define
