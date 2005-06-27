#ifndef CFD_SCALAR_SHADER_MANAGER_H
#define CFD_SCALAR_SHADER_MANAGER_H
#ifdef VE_PATENTED
#ifdef _OSG
#include "VE_TextureBased/cfdOSGTransferShaderManager.h"
namespace VE_TextureBased{
   class cfdTextureManager;
}
namespace VE_TextureBased{
   class VE_TEXTURE_BASED_EXPORTS cfdScalarShaderManager
      :public cfdOSGTransferShaderManager{
      public:
         cfdScalarShaderManager();
         virtual ~cfdScalarShaderManager(){}
         virtual void Init();
         virtual void UpdateTextureManager(cfdTextureManager* tm);
         void SetScalarRange(float* range);
         void UpdateScalarMax(float maxScalar);
         void UpdateScalarMin(float minScalar);
         void ActivateIsoSurface();
         void DeactivateIsoSurface();
         void SetIsoSurfaceValue(float percentScalarRange);
      protected:
         void _setupStateSetForGLSL();
         bool _isoSurface;
         void _updateTransferFunction();
         float _scalarRange[2];
         float _percentScalarRange;
         void _initTransferFunctions();
         void _initPropertyTexture();
   };
}
#endif//_OSG
#endif
#endif// CFD_SCALAR_SHADER_MANAGER_H
