#include "cfdUpdateTextureCallback.h"
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/State>
//////////////////////////////////////////////////////////////////
//Constructor                                                   //
//////////////////////////////////////////////////////////////////
cfdUpdateTextureCallback::cfdUpdateTextureCallback():
 _subloadMode(AUTO),
_textureWidth(0),
_textureHeight(0),
_textureDepth(0),
_subloadTextureOffsetX(0),
_subloadTextureOffsetY(0),
_subloadTextureOffsetZ(0),
_subloadImageOffsetX(0),
_subloadImageOffsetY(0),
_subloadImageOffsetZ(0),
_subloadImageWidth(0),
_subloadImageHeight(0),
_subloadImageDepth(0)
{
   _tm = 0;
   _delay = 1.0;

}
/////////////////////////////////////////////////////////////////
cfdUpdateTextureCallback::cfdUpdateTextureCallback(const cfdUpdateTextureCallback& cb)

{
   _tm = cb._tm;
   _delay = cb._delay;
}
///////////////////////////////////////////////////////////////////
cfdUpdateTextureCallback::~cfdUpdateTextureCallback()
{
   if(_tm){
      delete _tm;
      _tm = 0;
   }
}
///////////////////////////////////////////////////////////////////////////
void cfdUpdateTextureCallback::SetTextureManager(cfdTextureManager* tm)
{
   _tm = tm;
}
/////////////////////////////////////////////////////////////
void cfdUpdateTextureCallback::SetDelayTime(double delayTime)
{
   _delay = delayTime;
}
////////////////////////////////////////////////////////////////////////////////////
void cfdUpdateTextureCallback::load(const osg::Texture3D& texture,osg::State& state )const 
{
   texture.getExtensions(state.getContextID(),false)->glTexImage3D(GL_TEXTURE_3D, 0, 
                                          GL_RGBA, 
                                          _textureWidth,
                                          _textureHeight,
                                          _textureDepth,
                                          0, GL_RGBA, 
                                          GL_UNSIGNED_BYTE, 
                                          (unsigned char*)_tm->getNextField());
}
//////////////////////////////////////////////////////////////////////////////////////////////
void cfdUpdateTextureCallback::subload(const osg::Texture3D& texture,osg::State& state) const
{
   if(state.getFrameStamp()){
     double currTime = state.getFrameStamp()->getReferenceTime();
     if(_tm){  
        if(_tm->timeToUpdate(currTime,_delay)){
           texture.getExtensions(state.getContextID(),false)->glTexSubImage3D(GL_TEXTURE_3D,
                             0,
                             0,0,0, 
                             _textureWidth,
                             _textureHeight,
                             _textureDepth, 
                             GL_RGBA, 
                             GL_UNSIGNED_BYTE,
                             (unsigned char*)_tm->getNextField());
        }
     }
   }   
}
#endif
