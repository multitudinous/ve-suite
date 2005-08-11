#ifndef GEOMETRY_DATAMANAGER_H
#define GEOMETRY_DATAMANAGER_H

class GeometryDataBuffer;

class GeometryDataManager
{
  public:
      static GeometryDataManager& getInstance();
      GeometryDataBuffer* GetGeometryDataBuffer();

   private:
      GeometryDataManager():_GeometryDataBuffer(0){;}
      ~GeometryDataManager();

   private:
      GeometryDataBuffer* _GeometryDataBuffer;
      static GeometryDataManager* myinstance;	
};
#endif
