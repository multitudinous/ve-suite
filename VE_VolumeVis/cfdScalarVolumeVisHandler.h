#ifndef CFD_SCALAR_VOLUME_VIS_HANDLER_H
#define CFD_SCALAR_VOLUME_VIS_HANDLER_H
#ifdef VE_PATENTED
#ifdef _OSG
namespace osg { class Group; }
//#ifdef CFD_USE_SHADERS
class cfdScalarShaderManager;

class cfdTextureManager;

#include "cfdVolumeVisNodeHandler.h" 


class cfdScalarVolumeVisHandler : public cfdVolumeVisNodeHandler{
public:
   cfdScalarVolumeVisHandler();
   cfdScalarVolumeVisHandler(const cfdScalarVolumeVisHandler& vvnh);
   virtual ~cfdScalarVolumeVisHandler();
   virtual void Init();
   virtual void SetTextureManager(cfdTextureManager* tm);
   cfdScalarShaderManager* GetScalarShaderManager(){return _transferSM;}
   cfdScalarVolumeVisHandler& operator=(const cfdScalarVolumeVisHandler& vvnh);

protected:
   virtual void _setUpDecorator();
   virtual void _applyTextureMatrix();
   void _createTransferShader();
   cfdScalarShaderManager* _transferSM;
};
//#endif//CFD_USE_SHADERS
#endif //_OSG
#endif
#endif// CFD_SCALAR_VOLUME_VIS_HANDLER_H
