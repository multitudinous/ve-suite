#ifdef _PERFORMER
#elif _OSG
#ifdef CFD_USE_SHADERS
#include "cfdOSGPingPongTexture3D.h"
#include <osg/Image>
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
////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::PingPongTextures()
{
   //this may be more than is needed
   //copy the ping image
   osg::ref_ptr<osg::Texture3D> tempPing = new osg::Texture3D(*_ping.get());
                        
   //copy the pong image
   //osg::ref_ptr<osg::Image> tempPong = new osg::Image(*_pong->getImage());
   
   //switch ping image to pong
   _ping = _pong;
   _ping->getImage()->dirty();
   //switch pong image to ping
   _pong = tempPing;
   _pong->getImage()->dirty();
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
#endif //
#endif //_OSG
