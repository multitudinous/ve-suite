#include "cfdVeApp.h"

cfdVeApp::cfdVeApp(char* param_filename)
{
   this->mController = new cfdVeController(param_filename);
   this->mModel      = new cfdVeModel(mController,param_filename);
   this->mView       = new cfdVeView(mModel, mController,param_filename);

   this->ControllerFunc = new vpr::ThreadMemberFunctor<cfdController>(mController, cfdController::runControllerThread);
   this->vjTh[0] = new vpr::Thread(this->ControllerFunc);
 
}


cfdVeApp::~cfdVeApp()
{
   delete this->mController;
   delete this->mModel;
   delete this->mView;
}

void cfdVeApp::run()
{
   
}

void cfdApp::stopModel()
{
   this->running = false;
}

int main(int argc, char* argv[])
{
  
   char file_name[100];
   printf("|   Enter parameter filename: ");
   scanf("%s",file_name);
   std::cout << std::endl;
   std::cout << "|   Initializing........................... Parameter File Reader |" << std::endl;

   unsigned int seed =16000;
   srand48(seed);
  
   cfdVeApp *myveapplication = new cfdVeApp(file_name);
  /*
   *  TO DO
   */
   return 1;
      
 }




