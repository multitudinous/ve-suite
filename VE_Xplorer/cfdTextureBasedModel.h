#ifndef CFD_TEXTURE_BASED_MODEL_H
#define CFD_TEXTURE_BASED_MODEL_H
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include "cfdReadParam.h"

#include "cfdTextureManager.h"
#include "cfdVolumeVisualization.h"

#include <vector>
class cfd3DTextureBasedModel{
public:
   cfd3DTextureBasedModel();
   cfd3DTextureBasedModel(const cfd3DTextureBasedModel& tbm);
   ~cfd3DTextureBasedModel();

   void SetParameterFileName(char* filename);
   void InitializeModel();

   void AddVectorTextureManager(cfdTextureManager tm,char* vectorName);
   void AddScalarTextureManager(cfdTextureManager tm,char* scalarName);

   void SetActiveScalar(char* scalarName);
   void SetActiveVector(char* vectorName);

   void SetActiveScalar(int whichScalar);
   void SetActiveVector(int whichVector);

   cfdTextureManager* GetActiveScalarTexture();
   cfdTextureManager* GetActiveVectorTexture();

   cfdVolumeVisualization* GetVolumeVisualizaionWithScalar(int whichScalar);
   cfdVolumeVisualization* GetVolumeVisualizationWithVector(int whichVector);
   cfdVolumeVisualization* GetVolumeVisualizaionWithScalar(char* name);
   cfdVolumeVisualization* GetVolumeVisualizationWithVector(char* name);
   
   float* GetScalarBoundingBox();
   float* GetVectorBoundingBox();
   cfd3DTextureBasedModel& operator=(const cfd3DTextureBasedModel& tbm);
protected:
   void _createTextureManager(char* filename);
   cfdReadParam _paramReader;

   cfdTextureManager* _activeScalar;
   cfdTextureManager* _activeVector;
   cfdVolumeVisualization* _volumeVizNode;

   std::vector<char*> _scalarNames;
   std::vector<char*> _vectorNames;

   std::vector<cfdTextureManager> _vectorDataTextures;
   std::vector<cfdTextureManager> _scalarDataTextures;
   char* _paramFileName;
   
};
#endif
#endif// CFD_TEXTURE_BASED_MODEL_H

