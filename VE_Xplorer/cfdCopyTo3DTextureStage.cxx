
#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include <cassert>
#include "cfdCopyTo3DTextureStage.h"

//////////////////////////////////////////////////
cfdCopyTo3DTextureStage::cfdCopyTo3DTextureStage()
:osgUtil::RenderStage()
{
   _pbuffer = 0L;
   _texture = 0;
  
   _localState = new osg::State;
   _localState->setReportGLErrors(true);

   _pbuffer = 0;

   _width = 0;
   _height = 0;
   _nSlices = 0;
   _whichSlice = 0;
   _whichDir = 2;
}
///////////////////////////////////////////////////
cfdCopyTo3DTextureStage::~cfdCopyTo3DTextureStage()
{
   if(_pbuffer){
      _pbuffer->cleanUpPBuffer();
      _pbuffer = 0;
   }
}
/////////////////////////////////////
void cfdCopyTo3DTextureStage::reset()
{
    RenderStage::reset();
}
/////////////////////////////////////////////////////////////////////////////////////
void cfdCopyTo3DTextureStage::draw(osg::State& state, osgUtil::RenderLeaf*& previous)
{
   if (_stageDrawnThisFrame) return;

   if(_pbuffer->isCreated() && _texture.valid()){
      _pbuffer->activate();
      _width = _pbuffer->width();
      _height= _pbuffer->height();

      const unsigned int contextID = state.getContextID();
      osg::Texture::TextureObject* textureObject = _texture->getTextureObject(contextID);
      if (textureObject == 0){
         // Make sure texture is loaded, subload callback required.
         _texture->apply(state);
      }
      //draw to the pbuffer
      RenderStage::draw(*_localState,previous);
      _pbuffer->deactivate();
      //copy into our texture
      //or should this be the local state?
      switch(_whichDir){
         //x slice
         case 2:
            _texture->copyTexSubImage3D(state,
                                     0,0,_whichSlice,
                                     0,0,_width,_height);
            break;
         //y slice
         case 1:
            _texture->copyTexSubImage3D(state,
                                     0,_whichSlice,0,
                                     0,0,_width,_height);
            break;
         //z slice
         case 0:
         default:
            _texture->copyTexSubImage3D(state,
                                     _whichSlice,0,0,
                                     0,0,_width,_height);
            break;
      };
      
      
   }
}
#endif
#endif
