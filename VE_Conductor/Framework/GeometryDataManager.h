#ifndef GEOMETRY_DATAMANAGER_H
#define GEOMETRY_DATAMANAGER_H

#include "VE_Conductor/Framework/GeometryDataBuffer.h"

class GeometryDataManager
{
  public:
      static GeometryDataManager& getInstance();
      GeometryDataBuffer* GetGeometryDataBuffer();

   private:
      GeometryDataManager():_GeometryDataBuffer(0){}
      ~GeometryDataManager()
      {
         delete _GeometryDataBuffer;
         _GeometryDataBuffer =0;
      }

   private:
      GeometryDataBuffer* _GeometryDataBuffer;
      static GeometryDataManager* myinstance;

};
#endif
