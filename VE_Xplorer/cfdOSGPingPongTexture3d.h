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
   void InitTextures();
   void PingPongTextures();
   
   cfdOSGPingPongTexture3D& operator=(const cfdOSGPingPongTexture3D& pp);

   class cfdPingPongSubload : public osg::Texture3D::SubloadCallback{
      public:
         cfdPingPongSubload(unsigned int w,
                          unsigned int h,
                          unsigned int d);
         ~cfdPingPongSubload(){}
         void SetPingPongTexture(osg::Texture3D* texture);
         void SetSubloadTextureSize(const int width, 
                                 const int height, 
                                 const depth);
         
      void subload(const osg::Texture3D& texture,osg::State& state) const;
      void load(const osg::Texture3D& texture,osg::State&) const;
   protected:
      osg::ref_ptr<osg::Texture3D> _swapTexture;
      unsigned int _textureWidth;
      unsigned int _textureHeight;
      unsigned int _textureDepth;
   };
protected:
   osg::ref_ptr<osg::Texture3D> _ping;
   osg::ref_ptr<osg::Texture3D> _pong;
};
#endif //_OSG
#endif // CFD_OSG_PING_PONG_TEXTURE_3D_H
