#ifdef VE_PATENTED
#include "cfdSimpleTextureCallback.h"
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
}
////////////////////////////////////////////////////////////////////////////////////
void cfdSimpleTextureCallback::load(const osg::Texture3D& texture,osg::State& state )const 
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
//////////////////////////////////////////////////////////////////////////////////////////////
void cfdSimpleTextureCallback::subload(const osg::Texture3D& texture,osg::State& state) const
{
  
        
       
}
#endif
#endif
