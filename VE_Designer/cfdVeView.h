#ifndef CFD_VEVIEW_H
#define CFD_VEVIEW_H

#include "cfdVeModel.h"
#include "cfdVeController.h"
class cfdVeView:public cfdObserver()
{
   public:
      cfdVeView();
      ~cfdVeView();
      update();
   private:
      cfdVeModel* _modelsubject;
      cfdVeController* _controllersubject;
      bool activemessagewindow;
      bool sendingvtkready;

};


#endif
