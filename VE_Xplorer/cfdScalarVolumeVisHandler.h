#ifndef CFD_SCALAR_VOLUME_VIS_HANDLER_H
#define CFD_SCALAR_VOLUME_VIS_HANDLER_H

#ifdef _OSG
#include <osg/Group>

#include "cfdScalarVolumeVisHandler.h" 
#include "cfdOSGScalarShaderManager.h"
#include "cfdTextureManager.h"
#include "cfdVolumeVisNodeHandler.h" 
#include "cfdSwitch.h"

class cfdScalarVolumeVisHandler : public cfdVolumeVisNodeHandler{
public:
   cfdScalarVolumeVisHandler();
   cfdScalarVolumeVisHandler(const cfdScalarVolumeVisHandler& vvnh);
   virtual ~cfdScalarVolumeVisHandler();
   virtual void Init();
   void SetTextureManager(cfdTextureManager* tm);
 
#ifdef CFD_USE_SHADERS
   void EnableVolumeShader();
   void DisableVolumeShader();
#endif
   cfdScalarVolumeVisHandler& operator=(const cfdScalarVolumeVisHandler& vvnh);
protected:
   virtual void _attachVolumeVisNodeToGraph();
#ifdef CFD_USE_SHADERS
   cfdOSGScalarShaderManager* _sSM;
   osg::ref_ptr<osg::Group> _scalarFragGroup;
#endif
   cfdTextureManager* _tm;
   cfdSwitch* _shaderSwitch;
   
};

#endif //_OSG
#endif// CFD_SCALAR_VOLUME_VIS_HANDLER_H
