
#include "cfdCopyTextureCallback.h"
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/State>
#include <osg/Node>
#include <osg/NodeVisitor>
//////////////////////////////////////////////////////////////////
//Constructor                                                   //
//////////////////////////////////////////////////////////////////
cfdCopyTextureCallback::cfdCopyTextureCallback():
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

}
//////////////////////////////////////////////////////////////////////////////////////////
void cfdCopyTextureCallback::load(const osg::Texture3D& texture,osg::State& state )const 
{
   texture.getTextureSize(_textureWidth,_textureHeight,_textureDepth);
   texture.getExtensions(state.getContextID(),false)->glTexImage3D(GL_TEXTURE_3D, 0, 
                                          GL_RGBA, 
                                          _textureWidth,
                                          _textureHeight,
                                          _textureDepth,
                                          0, GL_RGBA, 
                                          GL_UNSIGNED_BYTE, 
                                          0);
}
#endif

