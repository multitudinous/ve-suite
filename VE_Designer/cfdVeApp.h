#ifndef CFD_VEAPP_H
#define CFD_VEAPP_H

#include <cstdlib>

#include "cfdVeModel.h"
#include "cfdVeView.h"
#include "cfdVeController.h"

#include <vpr/vpr.h>
#include <vpr/Thread/Thread.h>
#include <vpr/Sync/Mutex.h>
#include <vpr/Sync/Guard.h>
#include <vpr/Util/Debug.h>

/*
 * using Model View Controller Design Pattern
 *
 * Model--> cfdGAserver
 *
 * View --> cfdApp
 *
 * Controller --> GUI --> cfdControllerObserver
 *
 * Model is the information contents of the component. It can be as simples as an integer or a string, and as complex as an interface to an external database. The MVC component does not need to contain the model itself. It may contain just a link to the model. And this is common if multiple MVC components are connected to a model. 
 *
 * View is the visual representation of the component, usually on a computer screen. The view also track of the state of the component itself. This way it not only knows what to show(the model) but also how to show it.
 *
 * Controller takes care of all the action. It is sent external events like keypresses, mouseclicks, mousemoves, etc. It knows how to change the model in response to these events and it also knows how to change the component's state in the view. 
 *
 * User uses Controller to manipulate the Model, Model notifies View to update, and then users sees the results from the view module.
 *
 *
 *
 *
 */

class cfdVeApp
{
   public:
      cfdVeApp(char*);
      ~cfdVeApp();
      void run();
      void stopModel();

   public:
      cfdVeModel     *mModel;
      cfdVeView      *mView;
      cfdVeController   *mController;
      
      vpr::ThreadMemberFunctor<cfdController> *ControllerFunc;
      vpr::Thread *vjTh[1];

   private:
      vpr::Mutex     mValueLock;
      bool running;
        
};

#endif
