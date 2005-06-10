#ifdef VE_PATENTED
#include "VE_VolumeVis/cfdSimpleTextureCallback.h"
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/State>
#include <osg/TexMat>
#include <osg/StateAttribute>
#include <iostream>
//////////////////////////////////////////////////////////////////
//Constructor                                                   //
//////////////////////////////////////////////////////////////////
cfdSimpleTextureCallback::cfdSimpleTextureCallback()
{
   _isLuminance = false;
}
////////////////////////////////////////////////////////////////////////////////////
void cfdSimpleTextureCallback::load(const osg::Texture3D& texture,osg::State& state )const 
{
   if(_isLuminance){
      texture.getExtensions(state.getContextID(),true)->glTexImage3D(GL_TEXTURE_3D, 0, 
                                          GL_ALPHA, 
                                          _textureWidth,
                                          _textureHeight,
                                          _textureDepth,
                                          0, GL_ALPHA, 
                                          GL_UNSIGNED_BYTE, 
                                          0);
   }else{
      texture.getExtensions(state.getContextID(),true)->glTexImage3D(GL_TEXTURE_3D, 0, 
                                          GL_RGBA, 
                                          _textureWidth,
                                          _textureHeight,
                                          _textureDepth,
                                          0, GL_RGBA, 
                                          GL_UNSIGNED_BYTE, 
                                          0);
   }
}
//////////////////////////////////////////////////////////////////////////////////////////////
void cfdSimpleTextureCallback::subload(const osg::Texture3D& texture,osg::State& state) const
{
  
        
       
}
#endif
#endif
