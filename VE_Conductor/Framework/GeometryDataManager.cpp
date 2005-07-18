#include "VE_Conductor/Framework/GeometryDataManager.h"

GeometryDataManager* GeometryDataManager::myinstance =0;

GeometryDataManager& GeometryDataManager::getInstance()
{
   if(myinstance == 0)
   {
      myinstance = new GeometryDataManager();

   }
   return *myinstance;

}

GeometryDataBuffer* GeometryDataManager::GetGeometryDataBuffer()
{
   if(!_GeometryDataBuffer)
   {
      _GeometryDataBuffer = new GeometryDataBuffer;

   }

   return _GeometryDataBuffer;
}
