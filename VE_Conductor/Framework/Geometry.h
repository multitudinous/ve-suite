#ifndef GEOMETRY_H
#define GEOMETRY_H

#include <map>
#include <vector>
#include "VE_Conductor/Framework/GeometryDataBuffer.h"

class Interface;

class VE_GUIPLUGINS_EXPORTS Geometry 
{
public:
   Geometry(int);
   ~Geometry();
   Geometry( const Geometry& );
   Geometry& operator= ( const Geometry& );
   
   // This is the load function of the module, unpack the input 
   // string and fill up the UI according to this
   void UnPack( Interface* );
   // Does the opposite of unpack
   Interface* Pack( void );

   void SetID(int);
   
   void SetCurModuleGeomInfoPackage();
   void SetGeometryDataBuffer( GeometryDataBuffer* );
   
protected:
   // data holder 
   int cur_moduleid;

   // Geometry data
   std::string cur_modulename;

private:
   Interface* mod_pack;
   std::map< std::string, GeometryInfoPackage > _geominfopackagemap;
   GeometryDataBuffer* geometryDataBuffer;
};
#endif //GEOMETRY_H

