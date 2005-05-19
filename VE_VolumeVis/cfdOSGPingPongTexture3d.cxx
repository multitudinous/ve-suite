#ifdef VE_PATENTED
#ifdef _PERFORMER
#elif _OSG
#ifdef CFD_USE_SHADERS
#include "cfdOSGPingPongTexture3d.h"
#include <osg/Image>
#include <osg/State>
#include <osg/Texture3D>
#include <osg/StateAttribute>
static bool start = false;
//////////////////////////////////////////////////
//Constructors                                  //
//////////////////////////////////////////////////
cfdOSGPingPongTexture3D::cfdOSGPingPongTexture3D()
{
    _pingUnit = 0;
    _pongUnit = 0;
}
//////////////////////////////////////////////////////
cfdOSGPingPongTexture3D::cfdOSGPingPongTexture3D(const
                           cfdOSGPingPongTexture3D& pp)
{
   _previous = pp._previous;
   _current = pp._current;
   _pingUnit = pp._pingUnit;
   _pongUnit = pp._pongUnit;
}
///////////////////////////////////////////////////
//Destructor                                     //
///////////////////////////////////////////////////
cfdOSGPingPongTexture3D::~cfdOSGPingPongTexture3D()
{
}
//////////////////////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::SetPingTexture(unsigned int unit,
                                        osg::Node* ping)
{
   _pingUnit = unit;
   _previous = ping;
}
//////////////////////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::SetPongTexture(unsigned int unit,
                                        osg::Node* pong)
{
   _pongUnit = unit;
   _current = pong;
}
/////////////////////////////////////////////////////////////
osg::Texture3D* cfdOSGPingPongTexture3D::GetCurrentTexture()
{
   if(_current.valid()){
      osg::ref_ptr<osg::Texture3D> texture = 
         dynamic_cast<osg::Texture3D*>(_current->getStateSet()->getTextureAttribute(_pongUnit,
                                                          osg::StateAttribute::TEXTURE));
      return texture.get();
   }
   return 0;
}
////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::PingPongTextures()
{
   osg::ref_ptr<osg::StateSet> tmpStateSet = _previous->getStateSet();
   unsigned int tmpUnit = 0;
   tmpUnit = _pingUnit;
   _pingUnit = _pongUnit;
   _previous->setStateSet(_current->getStateSet());
   _pongUnit = tmpUnit;
   _current->setStateSet(tmpStateSet.get());
   return;
}
/////////////////////////////////////////////////////////////////////   
//equal operator                                                   //
/////////////////////////////////////////////////////////////////////   
cfdOSGPingPongTexture3D&
cfdOSGPingPongTexture3D::operator=(const cfdOSGPingPongTexture3D& pp)
{
   if(this != &pp){
      _previous = pp._previous;
      _current = pp._current;
   }
   return *this;
}
#endif //
#endif //_OSG
#endif
