#ifndef CFD_OSG_GAMMA_SHADER_MANAGER_H
#define CFD_OSG_GAMMA_SHADER_MANAGER_H

#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include "cfdOSGTransferShaderManager.h"

class cfdOSGGammaShaderManager:public cfdOSGTransferShaderManager{
public:
   cfdOSGGammaShaderManager(){_useTM = true;}
   virtual ~cfdOSGGammaShaderManager(){}
   virtual void Init();

protected:
   void _initPropertyTexture();
};
#endif// _CFD_USE_SHADERS
#endif//_OSG
#endif// CFD_OSG_GAMMA_SHADER_MANAGER_H
