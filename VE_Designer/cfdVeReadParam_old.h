#ifndef CFD_VEREADPARAM_H
#define CFD_VEREADPARAM_H

#include <vpr/vpr.h>
#include <vpr/Thread/Thread.h>
#include <vpr/Sync/Mutex.h>


#include <iostream>
#include <vector>
#include <string>
#include <fstream>


class cfdVeReadParam
{
   public:
      cfdVeReadParam(char* );
      ~cfdVeReadParam(){}
      
      void dataRead(char *);
      bool gaParamRead(std::ifstream &);
      void designParamRead(std::ifstream &);



   private:
      //initial information
      bool interactiveGA;
      int initialtype;
      int popsize;
      int totalruns;
      int totalmatingevents;
      int numdesignparams;
      char initialpop_filename[256];
      char maxmin_filename[256];
      char textLine[256];
      
      
      vpr::Mutex     mValueLock;

   private:
      //interactive display information
      bool interactivedisplayflag;
      char* vrservername;
      bool keepoldmodel;
      float* oldmodeltrans; //old model
      float* oldmodelrot;
      char* newvtkfilename;
      
      //interactive design information
      float* currentdesignparams;
      int*   activeparamsflag;
      float* design_params;
      int*   active_params;

      //interactive GA information
      int checkedgene;    
      float* b_checked_params;
      char* directory;
      float crossoverrate;
      float mutationrate;
      bool activeGA;

   public:
      void setVRserverName(char* name);
      char* getVRserverName();

      void setModeForOldModel(bool mode);
      bool getModeForOldModel();

      void setTransForOldModel(float* trans);
      float* getTransForOldModel();

      void setRotForOldModel(float* rot);
      float* getRotForOldModel();

      void setNewVtkFileName(char* name);
      char* getNewVtkFileName();

      void setCurrentDesignParams(float* value);
      float* getCurrentDesignParams();

      void setActiveParams(int* value);
      int* getActiveParams();

      void setCheckedGene(int value);
      int getCheckedGene();

      void setCrossOverRate(float value);
      float getCrossOverRate();

      void setMutationRate(float value);
      float getMutationRate();

      //functions to get the initial params
   public:
      int getInitialType();
      int getNumDesignParams();
      int getPopSize();
      int getTotalRuns();
      int getTotalMatingEvents();
      bool getInteractiveDisplayFlag(); 
      char* getBoundaryFileName();
      char* getInitialPopFileName();
      char* getDirectoryName(); 
       
      
};

/*std::ostream& operator<<(std::ostream& os, const cfdVeReadParam& rp)
{
   os << "ParamFile\n";
   os << "Popsize: " << rp.popsize << "\n";
   os << "totalruns: " << rp.totalruns << "\n";
   os << "totalmatinevents: " << rp.totalmatingevents << "\n";
   os << "numdesignparams: " << rp.numdesignparams << "\n";
   os << "initalpop_filename: " << rp.initialpop_filename << "\n";
   os << "maxmin_filename: " << rp.maxmin_filename << "\n";
   os << std::flush;
   return os;
}*/

#endif
