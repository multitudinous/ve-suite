#ifndef CFD_SCALAR_VOLUME_VIS_HANDLER_H
#define CFD_SCALAR_VOLUME_VIS_HANDLER_H

#ifdef _OSG
namespace osg { class Group; }
#ifdef CFD_USE_SHADERS
class cfdOSGScalarShaderManager;
#endif
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
#ifdef CFD_USE_SHADERS
   cfdOSGScalarShaderManager* _sSM;
#endif
   
};
#endif //_OSG
#endif// CFD_SCALAR_VOLUME_VIS_HANDLER_H
