#ifndef CFD_SCALAR_SHADER_MANAGER_H
#define CFD_SCALAR_SHADER_MANAGER_H
#ifdef VE_PATENTED
#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include "cfdOSGTransferShaderManager.h"
class cfdTextureManager;

class cfdScalarShaderManager:public cfdOSGTransferShaderManager{
public:
   cfdScalarShaderManager(){_useTM = true;}
   virtual ~cfdScalarShaderManager(){}
   virtual void Init();
   virtual void UpdateTextureManager(cfdTextureManager* tm);
   void SetScalarRange(float* range);
   void UpdateScalarMax(float maxScalar);
   void UpdateScalarMin(float minScalar);

protected:
   void _updateTransferFunction();
   float _scalarRange[2];
   void _initTransferFunctions();
   void _initPropertyTexture();
};
#endif// _CFD_USE_SHADERS
#endif//_OSG
#endif
#endif// CFD_SCALAR_SHADER_MANAGER_H
