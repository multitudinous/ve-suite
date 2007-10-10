/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef _OSG
#include <osg/FrameStamp>
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
#include <osg/RenderInfo>
#endif
#include <osg/State>
#include <cassert>
#include "VE_Xplorer/TextureBased/cfdCopyTo3DTextureStage.h"
using namespace VE_TextureBased;
//////////////////////////////////////////////////
cfdCopyTo3DTextureStage::cfdCopyTo3DTextureStage()
:osgUtil::RenderStage()
{
   _texture = 0;
  
   _localState = new osg::State;

#ifdef _PBUFFER
   _pbuffer = 0;
#endif
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
/*void cfdCopyTo3DTextureStage::draw(osg::State& state, 
                               osgUtil::RenderLeaf*& previous)*/
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
void cfdCopyTo3DTextureStage::draw(osg::RenderInfo& renderInfo,
                                   osgUtil::RenderLeaf*& previous)
#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
void cfdCopyTo3DTextureStage::draw(osg::State& state, 
                               osgUtil::RenderLeaf*& previous)
#endif
{
   if (_stageDrawnThisFrame) return;

#ifdef _PBUFFER
   if(_pbuffer->isCreated()){
      _texture->getTextureSize(_width,_height,_nSlices);
      _pbuffer->activate();
      //const unsigned int contextID = 0;
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
      const unsigned int  contextID = renderInfo.getContextID();
#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
	  const unsigned int  contextID = state.getContextID();
#endif
      osg::Texture::TextureObject* textureObject = _texture->getTextureObject(contextID);
      if (textureObject == 0)
      {
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2))
         _texture->apply(*renderInfo.getState()/*state*/);
#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
         _texture->apply(state);
#endif
      }
        ///check here for changes if vectors don't work
      /*if(!_fs.valid()){
         _fs = new osg::FrameStamp();
      }
      _fs->setReferenceTime(state.getFrameStamp()->getReferenceTime());
      _fs->setFrameNumber(state.getFrameStamp()->getFrameNumber());
      
      _localState->setFrameStamp(_fs.get());*/
      for(unsigned int i = 1; i < _nSlices-1; i++)
      {
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
          RenderStage::draw(renderInfo,previous);
         _texture->copyTexSubImage3D(*renderInfo.getState()/*state*/,
                                  1,1,i,
                                  1,1,_width-1,_height-1);

#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
         RenderStage::draw(*_localState.get(),previous);
         _texture->copyTexSubImage3D(state,
                                  1,1,i,
                                  1,1,_width-1,_height-1);
#endif


         //need this to draw multiple slices
         _stageDrawnThisFrame = false;
      }
      _stageDrawnThisFrame =true;
      _pbuffer->deactivate();
   }
#endif
}
#endif
