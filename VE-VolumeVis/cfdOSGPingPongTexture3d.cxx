#ifdef _PERFORMER
#elif _OSG
#ifdef CFD_USE_SHADERS
#include "cfdOSGPingPongTexture3D.h"
#include <osg/Image>
#include <osg/State>

//////////////////////////////////////////////////
//Constructors                                  //
//////////////////////////////////////////////////
cfdOSGPingPongTexture3D::cfdOSGPingPongTexture3D()
{
   
}
//////////////////////////////////////////////////////
cfdOSGPingPongTexture3D::cfdOSGPingPongTexture3D(const
                           cfdOSGPingPongTexture3D& pp)
{
   _ping = pp._ping;
   _pong = pp._pong;
}
///////////////////////////////////////////////////
//Destructor                                     //
///////////////////////////////////////////////////
cfdOSGPingPongTexture3D::~cfdOSGPingPongTexture3D()
{
}
//////////////////////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::SetPingTexture(osg::Texture3D* ping)
{
   _ping = ping;
}
//////////////////////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::SetPongTexture(osg::Texture3D* pong)
{
   _pong = pong;
}
////////////////////////////////////////////
void cfdOSGPingPongTexture3D::InitTextures()
{
   if(_ping.valid()&&_pong.valid()){
      int w = 0;
      int h = 0;
      int d = 0;
      _ping->getTextureSize(w,h,d);

      osg::ref_ptr<cfdPingPongSubload> pong = new cfdPingPongSubload(w,h,d);
      pong->SetPingPongTexture(_pong.get());

      _ping->setSubloadCallback(pong.get());
      
   }
}
////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::PingPongTextures()
{
   return;
   //this may be more than is needed
   //copy the ping image
   osg::ref_ptr<osg::Image> tempPing = new osg::Image(*_ping->getImage());
                        
   //copy the pong image
   osg::ref_ptr<osg::Image> tempPong = new osg::Image(*_pong->getImage());
   
   //switch ping image to pong
   _ping->setImage(_pong->getImage());

   //switch pong image to ping
   _pong->setImage(tempPing.get());
}

/////////////////////////////////////////////////////////////////////   
//equal operator                                                   //
/////////////////////////////////////////////////////////////////////   
cfdOSGPingPongTexture3D&
cfdOSGPingPongTexture3D::operator=(const cfdOSGPingPongTexture3D& pp)
{
   if(this != &pp){
      _ping = pp._ping;
      _pong = pp._pong;
   }
   return *this;
}
////////////////////////////////////////
//Constructors                        //
////////////////////////////////////////
cfdOSGPingPongTexture3D::cfdPingPongSubload::cfdPingPongSubload(unsigned int w,
                                   unsigned int h,
                                   unsigned int d)
{
   SetSubloadTextureSize(w,h,d);
}   
////////////////////////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::cfdPingPongSubload::SetPingPongTexture(osg::Texture3D* texture)
{
   _swapTexture = texture;
}
///////////////////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::cfdPingPongSubload::subload(const osg::Texture3D& texture,
                                 osg::State& state) const
{
   if(_swapTexture.valid()){
      texture.getExtensions(state.getContextID(),false)->glTexSubImage3D(GL_TEXTURE_3D,
                             0,
                             0,0,0, 
                             _textureWidth,
                             _textureHeight,
                             _textureDepth, 
                             GL_RGBA, 
                             GL_UNSIGNED_BYTE,
                             (unsigned char*)_swapTexture->getImage()->data());
   }
}
///////////////////////////////////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::cfdPingPongSubload::load(const osg::Texture3D& texture,
                                                 osg::State& state) const
{
   texture.getExtensions(state.getContextID(),true)->glTexImage3D(GL_TEXTURE_3D, 0, 
                                          GL_RGBA, 
                                          _textureWidth,
                                          _textureHeight,
                                          _textureDepth,
                                          0, GL_RGBA, 
                                          GL_UNSIGNED_BYTE, 
                                          0);
}
////////////////////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::cfdPingPongSubload::SetSubloadTextureSize(const int width, 
                                          const int height,
                                          const depth)
{
   _textureWidth = width;
   _textureHeight = height;
   _textureDepth= depth;
}
#endif //
#endif //_OSG
