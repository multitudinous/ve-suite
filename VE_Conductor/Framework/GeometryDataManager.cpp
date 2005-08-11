#include "VE_Conductor/Framework/GeometryDataManager.h"
#include "VE_Conductor/Framework/GeometryDataBuffer.h"

GeometryDataManager* GeometryDataManager::myinstance =0;

GeometryDataManager& GeometryDataManager::getInstance()
{
   if(myinstance == 0)
   {
      myinstance = new GeometryDataManager();

   }
   return *myinstance;

}

GeometryDataManager::~GeometryDataManager()
{
   delete _GeometryDataBuffer;
   _GeometryDataBuffer =0;
}

GeometryDataBuffer* GeometryDataManager::GetGeometryDataBuffer()
{
   if(!_GeometryDataBuffer)
   {
      _GeometryDataBuffer = new GeometryDataBuffer;

   }

   return _GeometryDataBuffer;
}
