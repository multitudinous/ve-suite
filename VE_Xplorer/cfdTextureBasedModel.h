#ifndef CFD_TEXTURE_BASED_MODEL_H
#define CFD_TEXTURE_BASED_MODEL_H

#include "cfdTextureManager.h"
#include <vector>
class cfd3DTextureBasedModel{
public:
   cfd3DTextureBasedModel();
   cfd3DTextureBasedModel(const cfd3DTextureBasedModel& tbm);
   ~cfd3DTextureBasedModel();

   void AddVectorTextureManager(cfdTextureManager tm,char* vectorName);
   void AddScalarTextureManager(cfdTextureManager tm,char* scalarName);

   void SetActiveScalar(char* scalarName);
   void SetActiveVector(char* vectorName);

   void SetActiveScalar(int whichScalar);
   void SetActiveVector(int whichVector);

   cfdTextureManager* GetActiveScalarTexture();
   cfdTextureManager* GetActiveVectorTexture();
   
   float* GetScalarBoundingBox();
   float* GetVectorBoundingBox();
   cfd3DTextureBasedModel& operator=(const cfd3DTextureBasedModel& tbm);
protected:
   cfdTextureManager* _activeScalar;
   cfdTextureManager* _activeVector;

   std::vector<char*> _scalarNames;
   std::vector<char*> _vectorNames;

   std::vector<cfdTextureManager> _vectorDataTextures;
   std::vector<cfdTextureManager> _scalarDataTextures;
};
#endif// CFD_TEXTURE_BASED_MODEL_H
