#ifndef CFD_OSG_PING_PONG_TEXTURE_3D_H
#define CFD_OSG_PING_PONG_TEXTURE_3D_H
#ifdef VE_PATENTED
#ifdef _PERFORMER
#elif _OSG
#include <osg/Node>
namespace osg
{
   class Texture3D;
}
#include "VE_Installer/include/VEConfig.h"
namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdOSGPingPongTexture3D{
      public:
         cfdOSGPingPongTexture3D();
         cfdOSGPingPongTexture3D(const cfdOSGPingPongTexture3D& pp);
         virtual ~cfdOSGPingPongTexture3D();

         void SetPingTexture(unsigned int tunit,osg::Node* ping);
         void SetPongTexture(unsigned int tunit,osg::Node* pong);
         void PingPongTextures();
         osg::Texture3D* GetCurrentTexture();
   
         cfdOSGPingPongTexture3D& operator=(const cfdOSGPingPongTexture3D& pp);
      protected:
         osg::ref_ptr<osg::Node> _previous;
         osg::ref_ptr<osg::Node> _current;
         unsigned int _pingUnit;
         unsigned int _pongUnit;
   };
}
#endif //_OSG
#endif
#endif // CFD_OSG_PING_PONG_TEXTURE_3D_H
