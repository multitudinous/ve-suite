#ifndef CFDGASERVER_H
#define CFDGASERVER_H

#include "cfdGAManager.h"
#include "cfdChangeManager.h"

#include <vpr/vpr.h>
#include <vpr/Thread/Thread.h>
#include <vpr/Sync/Mutex.h>

#include <vtkUnstructuredGrid.h>
#include <vtkUnstructuredGridReader.h>
#include <vtkSocketCommunicator.h>

/*
Main calculation thread--doing the regular calculation

Interactive calculation thread--whenever needed, active it.

*/

class cfdGAserver
{
   public:
      cfdGAserver();
      cfdGAserver(char*, cfdChangeManager* );
      ~cfdGAserver();

          
      void getInfoFromChangeManager();
      void initialpops();
      void evolve();
      void singleCase(float*, bool);
           
      void sendVtkFileToVRserver(char* vrservername, char* vtkfilename);
      
      int getCurrentRun();
      int getCurrentMatingEvent();
      float getMaxFitness();
      float getMinFitness();
      float getMutationRate();
      float getCrossoverRate();

   public:
      cfdVeReadParam* param_file;
      cfdGAManager* genechain;
      
   private:
      double* currentdesignparm;
      bool interactiveGA;
      bool interactiveinitialpops;
      int initialtype;
      int popsize;
      int numdesignparams;
      int totalruns;
      int totalmatingevents;
      int geneindex;
      vpr::Mutex        mValueLock;

      bool regularGAthread_created;
      bool regularGA_finished;
      bool interactiveGAthread_created;
      bool interactiveGA_finished;
      bool vtkdata_ready;
      bool vrserver_ready;

      char* VeApp_VRserver_name;
      char* VtkFile_name; 

      float* checked_params;
      char* paramfilename;

   private:
      vtkUnstructuredGridReader* ugrid;
      vtkSocketCommunicator*  sock;
      cfdCalculator* curCalculator;

   private:
      float mutationrate;
      float crossoverrate;
      int currentmatingeven;
      int currentrun;
      std::vector<float* > currentfitness;
      

};


#endif
