#ifdef VE_PATENTED
#ifdef _OSG
#include <osg/FrameStamp>
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
   /*if(_pbuffer){
      _pbuffer->cleanUpPBuffer();
      _pbuffer = 0;
   }*/
}
//////////////////////////////////////////////////////////////////
void cfdCopyTo3DTextureStage::SetShaderStateSet(osg::StateSet* ss)
{
   _shader = ss;
}
//////////////////////////////////////////////////////////////////////////
void cfdCopyTo3DTextureStage::SetWhichSliceToUpdate(unsigned int nSlices)
{
   _whichSlice = nSlices;
}
/////////////////////////////////////
void cfdCopyTo3DTextureStage::reset()
{
    RenderStage::reset();
    if(_whichSlice == _nSlices){
       _whichSlice = 0;
    }
}
//////////////////////////////////////////////////////////////////
void cfdCopyTo3DTextureStage::draw(osg::State& state, 
                               osgUtil::RenderLeaf*& previous)
{
   if (_stageDrawnThisFrame) return;

   if(_pbuffer->isCreated()){
      _texture->getTextureSize(_width,_height,_nSlices);
      _pbuffer->activate();
      
      const unsigned int contextID = state.getContextID();
      osg::Texture::TextureObject* textureObject = _texture->getTextureObject(contextID);
      if (textureObject == 0){
         _texture->apply(state);
      }
      if(!_fs.valid()){
         _fs = new osg::FrameStamp();
      }
      _fs->setReferenceTime(state.getFrameStamp()->getReferenceTime());
      _fs->setFrameNumber(state.getFrameStamp()->getFrameNumber());
      
      _localState->setFrameStamp(_fs.get());
      for(unsigned int i = 1; i < _nSlices-1; i++){
         RenderStage::draw(*_localState.get(),previous);
         _texture->copyTexSubImage3D(state,
                                  1,1,i,
                                  1,1,_width-1,_height-1);

         //need this to draw multiple slices
         _stageDrawnThisFrame = false;
      }
      _stageDrawnThisFrame =true;
      _pbuffer->deactivate();
   }
}
#endif
#endif
#endif
