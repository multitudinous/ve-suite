#include "cfdVeView.h"

cfdVeView::cfdVeView(cfdVeModel* model, cfdVeController* controller)
{
   this->_modelsubject = model;
   this->_modelsubject->addObserver(this);

   this->_controllersubject = controller;
   this->_controllersubject->addObserver(this);

}

cfdVeView::~cfdVeView()
{
   //delete this->_modelsubject;
   //delete this->_controllersubject;

}

void cfdVeView::update(cfdSubjectBase* theChangedSubject)
{
   //if update comes from the model
   if(theChangedSubject == (this->_modelsubject))
   {
     this->updateFromModel();      
   }
   else if(theChangeSubject == (this->_controllersubject))
   {
      this->updateFromController();

   }

}

void cfdVeView::updateFromModel()
{
   // when new vtk file is ready, cfdVeView active an info window, so that user can bring new model up

   if(this->_modelsubject->isVtkDataReady())
   {
      this->sendingvtkready = true;
      //plan to use socket to communicate between cfdVeView and Java GUI in order to active the 

   }
   else
   {
      this->sendingvtkready = false;
   }
   
   

   
}

// this function is actived if the display button is clicked 
void cfdVeView::updateFromController()
{
   
   if(this->sendingvtkready)
   {
      

   }
}
