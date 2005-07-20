#ifndef GEOMETRY_H
#define GEOMETRY_H

#include "VE_Conductor/Framework/interface.h"
#include <map>
#include <vector>
#include "VE_Conductor/Framework/GeometryDataManager.h"

class VE_CONDUCTOR_EXPORTS Geometry 
{
   public:
      Geometry(int);
      ~Geometry(){}
      
      // This is the load function of the module, unpack the input 
      // string and fill up the UI according to this
      void UnPack( Interface* );
      // Does the opposite of unpack
      Interface* Pack( void );

      void SetID(int);
      
      void SetCurModuleGeomInfoPackage();
      
   protected:
      
      // data holder 
      Interface mod_pack;
      int cur_moduleid;

      // Geometry data
      std::map<std::string, GeometryInfoPackage> _geominfopackagemap;

      std::string cur_modulename;


    };
#endif //GEOMETRY_H

