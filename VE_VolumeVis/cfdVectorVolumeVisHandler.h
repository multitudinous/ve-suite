#ifndef CFD_VECTOR_VOLUME_VIS_HANDLER_H
#define CFD_VECTOR_VOLUME_VIS_HANDLER_H
#ifdef VE_PATENTED
#ifdef _OSG
//#ifdef CFD_USE_SHADERS
namespace osg{
   class Group;
   class Texture3D;
}
class cfdOSGAdvectionShaderManager;
class cfdOSGTransferShaderManager;

class cfdTextureManager;
class cfdPBufferManager;


class cfdUpdateTextureCallback;
class cfd3DTextureCullCallback;
class cfdOSGPingPongTexture3D;

#include "cfdVolumeVisNodeHandler.h"

class cfdVectorVolumeVisHandler : public cfdVolumeVisNodeHandler{
public:
   cfdVectorVolumeVisHandler();
   cfdVectorVolumeVisHandler(const cfdVectorVolumeVisHandler& vvnh);
   virtual ~cfdVectorVolumeVisHandler();
   virtual void Init();
   virtual void SetTextureManager(cfdTextureManager* tm);

   void SetPBufferManager(cfdPBufferManager* pbm);
   void PingPongTextures();
   cfdOSGAdvectionShaderManager* GetAdvectionShaderManager(){return _aSM;}
   cfdVectorVolumeVisHandler& operator=(const cfdVectorVolumeVisHandler& vvnh);
protected:
   virtual void _setUpDecorator();
   virtual void _applyTextureMatrix();
   void _createTexturePingPong();
   void _initPropertyTexture();
   void _createVelocityFromTextureManager();
   void _createTransferShader();
   void _setupTransferPropertyStateSet();
   void _setupAdvectionPropertyStateSet();

   bool _ssIsSet;

   cfdOSGAdvectionShaderManager* _aSM;
   cfdOSGTransferShaderManager* _transferSM;
   
   cfd3DTextureCullCallback* _cullCallback;
   cfdUpdateTextureCallback* _velocityCbk;
   cfdPBufferManager* _pbuffer;
   cfdOSGPingPongTexture3D* _texturePingPong;

   osg::ref_ptr<osg::Group> _advectionSlice;
   osg::ref_ptr<osg::Group> _propertyTextureGroup;
   osg::ref_ptr<osg::Texture3D> _property;
   osg::ref_ptr<osg::Texture3D> _velocity;

};
//#endif //
#endif //_OSG
#endif// CFD_SCALAR_VOLUME_VIS_HANDLER_H
#endif
