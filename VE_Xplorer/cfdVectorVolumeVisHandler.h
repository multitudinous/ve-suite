#ifndef CFD_VECTOR_VOLUME_VIS_HANDLER_H
#define CFD_VECTOR_VOLUME_VIS_HANDLER_H

#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include <osg/Group>
#include <osg/Texture3D>
#include <osg/Projection>

#include "cfdOSGAdvectionShaderManager.h"
#include "cfdTextureManager.h"
#include "cfdVolumeVisNodeHandler.h"
#include "cfdUpdateTextureCallback.h"
#include "cfd3DTextureCullCallback.h"
#include "cfdOSGPingPongTexture3d.h"
#include "cfdSwitch.h"

class cfdVectorVolumeVisHandler : public cfdVolumeVisNodeHandler{
public:
   cfdVectorVolumeVisHandler();
   cfdVectorVolumeVisHandler(const cfdVectorVolumeVisHandler& vvnh);
   virtual ~cfdVectorVolumeVisHandler();
   virtual void Init();
   void SetTextureManager(cfdTextureManager* tm);
   void SetPBufferManager(cfdPBufferManager* pbm);
 
 
   void EnableAdvectionShader();
   void DisableAdvectionShader();
   void PingPongTextures();

   cfdVectorVolumeVisHandler& operator=(const cfdVectorVolumeVisHandler& vvnh);
protected:
   virtual void _attachVolumeVisNodeToGraph();
   void _createTexturePingPong();
   void _initPropertyTexture();
   void _createVelocityFromTextureManager();

   cfdOSGAdvectionShaderManager* _aSM;
   cfdSwitch* _shaderSwitch;
   cfdTextureManager* _tm;
   cfdUpdateTextureCallback* _velocityCbk;
   cfdPBufferManager* _pbuffer;
   cfd3DTextureCullCallback* _cullCallback;
   cfdOSGPingPongTexture3D* _texturePingPong;
   osg::ref_ptr<osg::Group> _advectionFragGroup;
   osg::ref_ptr<osg::Group> _advectionSlice;
   osg::ref_ptr<osg::Texture3D> _property;
   osg::ref_ptr<osg::Texture3D> _velocity;

   
};
#endif //
#endif //_OSG
#endif// CFD_SCALAR_VOLUME_VIS_HANDLER_H
