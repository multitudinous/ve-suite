#ifndef CFD_VEREADPARAM_H
#define CFD_VEREADPARAM_H

#include <cppdom/cppdom.h>
#include <vpr/vpr.h>
#include <vpr/Thread/Thread.h>
#include <vpr/Sync/Mutex.h>


#include <iostream>
#include <vector>
#include <cstring>
#include <fstream>


class cfdVeReadParam
{
   public:
      cfdVeReadParam(char* );
      ~cfdVeReadParam(){}
      
      void loadDataFromXML(char* filename );
      bool gaParamRead(std::ifstream &);
      void designParamRead(std::ifstream &);

      /*inline  gmtl::Vec3f readVec3f( cppdom::NodePtr node )
	   {
		   gmtl::Vec3f v;
         
		   v[0] = node->getAttribute("x").getValue<float>();
		   v[1] = node->getAttribute("y").getValue<float>();
		   v[2] = node->getAttribute("z").getValue<float>();
         return v;
	   }*/

     public:
      void  initialDefaultInfo();
      
      char* getVRserverName();
      char* getCFDserverName();
      char* getCFDpackageName();
      
      bool getModeForOldModel();
      float* getTransForOldModel();
      float* getRotForOldModel();
      char* getNewVtkFileName();
      
      bool getSaveMode();
      bool getDisplayMode();
      
      float getCrossOverRate();
      float getMutationRate();
      int  getCheckedGene();
      char* getDirectoryName();
     
      int getInitialType();
      int getNumDesignParams();
      int getPopSize();
      int getTotalRuns();
      int getTotalMatingEvents();
      char* getBoundaryFileName();
      char* getInitialPopFileName();
      int getChangeType();
      /*
      

      */
   
   private:
      //initial information
      bool inateractiveGA;
      bool interactivedesign;
      bool interactivedisplay;
      char* cfdpackage;
      int initialtype;
      int popsize;
      int totalruns;
      int totalmatingevents;
      int numdesignparams;
      char* initialpopfilename;
      char* boundaryfilename;
      char textLine[256];
      int changetype;
      
      
      vpr::Mutex     mValueLock;


   private:
      //interactive display information
      bool interactivedisplayflag;
      char* vrservername;
      char* cfdservername;
      bool keepoldmodel;
      float oldmodeltrans[3]; //old model
      float oldmodelrot[3];
      char* newvtkfilename;
      
      //interactive design information
      float* currentdesignparams;
      int*   activeparamsflag;
      float* design_params;
      int*   active_params;
      bool savemode;
      bool designdisplaymode;

      //interactive GA information
      int checkedgene;    
      float* b_checked_params;
      char* directory;
      float crossoverrate;
      float mutationrate;
      bool activeGA;

      
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
