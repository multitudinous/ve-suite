#ifndef CFD_CHANGEMANAGER_H
#define CFD_CHANGEMANAGER_H

#include <map>
#include <string>
#include "cfdVeReadParam.h"

/*
 *this is a singleton, which managers the change to make sure all the changes are legal.
 */
class cfdChangeManager
{
   public:

      /**
       * Returns the sole instance of this singleton
       */
      static cfdChangeManager& getInstance();

      /**
       * Returns the requested ParamReader; will load the ParamReader if it
       * does not already exist.
       *
       * @param   filename    the name of the file for this ParamReader to parse.
       *
       * @return     a cfdVEReadParam object associated with filename.
       */
      cfdVeReadParam* getParamFile(char *);
      
      void setInfoFromParamFile(char*);
      void otherOperation();
      int  getOpID();
      bool changeStatus();

      
      
      
   private:
      
      ///Copyright
      cfdChangeManager()
      {}
      ~cfdChangeManager();

      ///the instance of this singleton
      static cfdChangeManager* myinstance;
      
      ///all of the parameter files that have been opened
      std::map<std::string, cfdVeReadParam*> mFileMap;

      void otherOperations();
      
   public:
      
      void setOPID(int value);
      int  getOPID();
      
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

      void setSaveMode(bool value);
      bool getSaveMode();

      void setDesignDisplayMode(bool value);
      bool getDesignDisplayMode();

      void setCheckedGene(int value);
      int getCheckedGene();

      void setCrossOverRate(float value);
      float getCrossOverRate();

      void setMutationRate(float value);
      float getMutationRate();

      void setChangeType(int value);
      int  getChangeType();

      void setGADisplayMode(bool value);
      bool getGADisplayMode();

      

   private:
      /*
      params for interactive display
      */
      bool interactivedisplayflag;
      char* vrservername;
      char* cfdservername;
      bool keepoldmodel;
      float oldmodeltrans[3]; //old model
      float oldmodelrot[3];
      char* newvtkfilename;

      /*
      params for interactive design
      */
      float* currentdesignparams;
      int*   activeparamsflag;
      float* design_params;
      int*   active_params;
      bool   savemode;
      bool   designdisplaymode;
      
      
      
     
     /*
     params for interactive GA
     */
      int checkedgene;    
      float* b_checked_params;
      char* directory;
      float crossoverrate;
      float mutationrate;
      bool GAdisplaymode;

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

#endif
