#ifndef CFD_VEVIEW_H
#define CFD_VEVIEW_H

#include "cfdVeModel.h"
#include "cfdVeController.h"
#include "cfdObserverBase.h"
#include "cfdSubjectBase.h"
#include "cfdChangeManager.h"

class cfdVeView:public cfdObserverBase
{
   public:
      cfdVeView(cfdVeModel*, cfdVeController*);
      ~cfdVeView();
      void update(cfdSubjectBase*);
      void updateFromController();
      void updateFromModel();
   private:
      cfdVeModel* _modelsubject;
      cfdVeController* _controllersubject;
      bool activemessagewindow;
      bool sendingvtkready;

};


#endif
