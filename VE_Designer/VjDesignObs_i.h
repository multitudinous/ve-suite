#ifndef _VJDESINGOBS_I_H_
#define _VJDESINGOBS_I_H_

#include "VjDesignObs.h"
#include "cfdVeReadParam.h"
#include "cfdVeReadParamFactory.h"

#include <cstdlib>
#include <vpr/vpr.h>
#include <vpr/Sync/Mutex.h>
#include <vpr/Sync/Guard.h>
#include <vpr/Util/Debug.h>


class VjDesignObs_i : virtual public POA_VjDesignObs, public PortableServer::RefCountServantBase
{
   public:
      VjDesignObs_i(char* filename);
      VjDesignObs_i();
      virtual ~VjDesignObs_i(){}
      void update();
   

   private:
      CORBA::Object_var obj;
      CORBA::ORB_ptr orb;
      vpr::Mutex     mValueLock;
      cfdVeReadParam *param_file;
   
   public:
      void setClientInfoFlag(const short value);
      short getClientInfoFlag();

      void setInteractiveState_UI(const short value);
      short getInteractiveState_UI();

      void setInteractiveState_cfd(const short value);
      short getInteractiveState_cfd();

      void setInteractiveDisplay_UI(const short value);
      short getInteractiveDisplay_UI();

      void setInteractiveGA(const short value);
      short getInteractiveGA();

      void setActiveDesignParams(const VjDesignObs::active_flag &value);
      //VjDesignObs::active_flag* getActiveDesignParams();
      short* getActiveDesignParams();
         
      void setDesignParams(const VjDesignObs::design_params &value);
      //VjDesignObs::design_params* getDesignParams();
      double* getDesignParams();
     
      void setSaveMode(const short value);
      short getSaveMode();

      void setDisplayMode(const short value);
      short getDisplayMode();

      void setVRserverName(const char* value);
      char* getVRserverName();
      
      void setModeForOldModel(const short value);
      short getModeForOldModel();

      void setTransForOldModel(const VjDesignObs::design_params &value);
      //VjDesignObs::design_params* getTrans_oldModel();
      double* getTransForOldModel();
      
      void setRotForOldModel(const VjDesignObs::design_params &value);
      //VjDesignObs::design_params* getRot_oldModel();
      double* getRotForOldModel();
      
      void setNewVtkFileName(const char* value);
      char* getNewVtkFileName();
      
      void setCheckedGene(const short value);
      short getCheckedGene();

      void setCrossoverRate(const double value);
      double getCrossoverRate();

      void setMutationRate(const double value);
      double getMutationRate();

   public:

   // Buffer variables...always right
      int      b_client;
      long     b_Id;
      short    b_getclientInfo;
      int      b_numClientInfo;
      short    b_interactivedisplay_UI;
      short    b_interactivestate_UI;
      short    b_interactivestate_cfd;
      short    b_updateInteractiveState_design;
      short    b_updateInteractiveState_GA;
      short    b_interactiveGA;
      char*    b_VRservername;
      char*    b_vtkfilename;
      short     b_mode;
      short    b_savemode;      
      
      /*VjDesignObs::design_params*  b_local_designparams;
      VjDesignObs::design_params*  b_trans;
      VjDesignObs::design_params*  b_rot;*/
      //VjDesignObs::file_name*    b_vtkfilename;

      double*  b_local_designparams;
      double*  b_trans;
      double*  b_rot;
      
      short    b_checkedgene;
      double   b_mutationrate;
      double   b_crossoverrate;
      

   // cfdVeApp side variables
      double*  VeApp_local_params;
      short*   VeApp_activeindex;
      short    VeApp_readytosendparams;
      short    VeApp_interactiveGA;
       
};

#endif

