#ifndef CFD_OSG_PING_PONG_TEXTURE_3D_H
#define CFD_OSG_PING_PONG_TEXTURE_3D_H
#ifdef _PERFORMER
#elif _OSG
#include <osg/Texture3D>
class cfdOSGPingPongTexture3D{
public:
   cfdOSGPingPongTexture3D();
   cfdOSGPingPongTexture3D(const cfdOSGPingPongTexture3D& pp);
   virtual ~cfdOSGPingPongTexture3D();

   void SetPingTexture(osg::Texture3D* ping);
   void SetPongTexture(osg::Texture3D* pong);

   void PingPongTextures();
   
   cfdOSGPingPongTexture3D& operator=(const cfdOSGPingPongTexture3D& pp);
protected:
   osg::ref_ptr<osg::Texture3D> _ping;
   osg::ref_ptr<osg::Texture3D> _pong;
};
#endif //_OSG
#endif // CFD_OSG_PING_PONG_TEXTURE_3D_H
