#ifndef CFD_SCALAR_VOLUME_VIS_HANDLER_H
#define CFD_SCALAR_VOLUME_VIS_HANDLER_H

#ifdef _OSG
namespace osg { class Group; }
#ifdef CFD_USE_SHADERS
class cfdOSGScalarShaderManager;
class cfdOSGGammaShaderManager;

class cfdTextureManager;

#include "cfdVolumeVisNodeHandler.h" 


class cfdScalarVolumeVisHandler : public cfdVolumeVisNodeHandler{
public:
   cfdScalarVolumeVisHandler();
   cfdScalarVolumeVisHandler(const cfdScalarVolumeVisHandler& vvnh);
   virtual ~cfdScalarVolumeVisHandler();
   virtual void Init();
 
   cfdScalarVolumeVisHandler& operator=(const cfdScalarVolumeVisHandler& vvnh);
protected:
   virtual void _setUpDecorator();
   virtual void _applyTextureMatrix();
   void _createTransferShader();

   cfdOSGScalarShaderManager* _sSM;
   cfdOSGGammaShaderManager* _transferSM;

   
};
#endif//CFD_USE_SHADERS
#endif //_OSG
#endif// CFD_SCALAR_VOLUME_VIS_HANDLER_H
