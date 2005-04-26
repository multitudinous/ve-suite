#ifdef VE_PATENTED
#include "cfdUpdateTextureCallback.h"


#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/State>

#include <osg/Node>
#include <osg/NodeVisitor>
#include "cfdTextureManager.h"
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
   _isSlave = false;
   _currentFrame = 0;
   _isLuminance = false;
   _update = true;
}
/////////////////////////////////////////////////////////////////
cfdUpdateTextureCallback::cfdUpdateTextureCallback(const cfdUpdateTextureCallback& cb)

{
   _tm = cb._tm;
   _delay = cb._delay;
   _isSlave = cb._isSlave;
   _currentFrame = cb._currentFrame;
   _isLuminance = cb._isLuminance;
}
///////////////////////////////////////////////////////////////////
cfdUpdateTextureCallback::~cfdUpdateTextureCallback()
{
   /*if(_tm){
      delete _tm;
      _tm = 0;
   }*/
}
///////////////////////////////////////////////////////////////////////////
void cfdUpdateTextureCallback::SetTextureManager(cfdTextureManager* tm)
{
   if(_tm != tm)
   {
      _tm = tm;
      _update = true;
   }else{
      _update = false;
   }
   return;
}
/////////////////////////////////////////////////////////////
void cfdUpdateTextureCallback::SetDelayTime(double delayTime)
{
   _delay = delayTime;
}
////////////////////////////////////////////////////////
unsigned int cfdUpdateTextureCallback::GetCurrentFrame()
{
   if(_tm){
      return _tm->GetCurrentFrame();
   }
   return 0;
}
////////////////////////////////////////////////////////////////////////
void cfdUpdateTextureCallback::SetCurrentFrame(unsigned int cFrame)
{
   if(_tm){
      _currentFrame = cFrame;
      _isSlave = true;
   }
}
////////////////////////////////////////////////////////////////////////////////////
void cfdUpdateTextureCallback::load(const osg::Texture3D& texture,osg::State& state )const 
{
   if(_isLuminance)
   {
      texture.getExtensions(state.getContextID(),false)->glTexImage3D(GL_TEXTURE_3D, 0, 
                                          GL_ALPHA, 
                                          _textureWidth,
                                          _textureHeight,
                                          _textureDepth,
                                          0, GL_ALPHA, 
                                          GL_UNSIGNED_BYTE, 
                                          (unsigned char*)_tm->dataField(0));
   
   }else{
      texture.getExtensions(state.getContextID(),false)->glTexImage3D(GL_TEXTURE_3D, 0, 
                                          GL_RGBA, 
                                          _textureWidth,
                                          _textureHeight,
                                          _textureDepth,
                                          0, GL_RGBA, 
                                          GL_UNSIGNED_BYTE, 
                                          (unsigned char*)_tm->dataField(0));
   }
}
//////////////////////////////////////////////////////////////////////////////////////////////
void cfdUpdateTextureCallback::subload(const osg::Texture3D& texture,osg::State& state) const
{
   if(state.getFrameStamp()){
     double currTime = state.getFrameStamp()->getReferenceTime();
     if(_tm){  

        if(!_isSlave){
           //master node in the cluster
           if(_tm->timeToUpdate(currTime,_delay)||_update){
              if(_isLuminance)
              {
                 texture.getExtensions(state.getContextID(),false)->glTexSubImage3D(GL_TEXTURE_3D,
                             0,
                             0,0,0, 
                             _textureWidth,
                             _textureHeight,
                             _textureDepth, 
                             GL_ALPHA, 
                             GL_UNSIGNED_BYTE,
                             (unsigned char*)_tm->getNextField());
              
              }else{
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
        }else{
           if(_isLuminance)
           {
              texture.getExtensions(state.getContextID(),false)->glTexSubImage3D(GL_TEXTURE_3D,
                             0,
                             0,0,0, 
                             _textureWidth,
                             _textureHeight,
                             _textureDepth, 
                             GL_ALPHA, 
                             GL_UNSIGNED_BYTE,
                             (unsigned char*)_tm->dataField(_currentFrame));
           }else{
              texture.getExtensions(state.getContextID(),false)->glTexSubImage3D(GL_TEXTURE_3D,
                             0,
                             0,0,0, 
                             _textureWidth,
                             _textureHeight,
                             _textureDepth, 
                             GL_RGBA, 
                             GL_UNSIGNED_BYTE,
                             (unsigned char*)_tm->dataField(_currentFrame));
           }
        }
     }
   }   
}
#endif
#endif
