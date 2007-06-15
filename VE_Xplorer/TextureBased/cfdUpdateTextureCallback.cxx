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
#include "VE_Xplorer/TextureBased/cfdUpdateTextureCallback.h"

#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/State>

#include <osg/Node>
#include <osg/NodeVisitor>
#include "VE_Xplorer/TextureBased/cfdTextureManager.h"
using namespace VE_TextureBased;
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
   _delay = 0.0001f;
   _isSlave = false;
   _currentFrame = 0;
   _isLuminance = false;
   _update = false;
}
/////////////////////////////////////////////////////////////////////////////////////
cfdUpdateTextureCallback::cfdUpdateTextureCallback(const cfdUpdateTextureCallback& cb)
:osg::Texture3D::SubloadCallback(cb)
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
   _update = false;
}
////////////////////////////////////////////////////////
unsigned int cfdUpdateTextureCallback::GetCurrentFrame()
{
   if(_tm){
      return _tm->GetCurrentFrame();
   }
   return 0;
}
///////////////////////////////////////////////////////////////////////////////////
void cfdUpdateTextureCallback::SetCurrentFrame(unsigned int cFrame, bool makeSlave)
{
   if(_tm){
      _currentFrame = cFrame;
      if(makeSlave)
         _isSlave = true;
   }
}
////////////////////////////////////////////////////////////////////////////////////
void cfdUpdateTextureCallback::load(const osg::Texture3D& texture,osg::State& state )const 
{
   if(_isLuminance)
   {
      texture.getExtensions(state.getContextID(),false)->glTexImage3D(GL_TEXTURE_3D, 0, 
                                          GL_LUMINANCE_ALPHA, 
                                          _textureWidth,
                                          _textureHeight,
                                          _textureDepth,
                                          0, GL_LUMINANCE_ALPHA, 
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
         if(!_isSlave && _tm->getPlayMode() == cfdTextureManager::PLAY){
            //master node in the cluster
            if(_tm->TimeToUpdate()||_update)
            {
               if(_isLuminance)
               {
                  texture.getExtensions(state.getContextID(),false)->glTexSubImage3D(GL_TEXTURE_3D,
                             0,
                             0,0,0, 
                             _textureWidth,
                             _textureHeight,
                             _textureDepth, 
                             GL_LUMINANCE_ALPHA, 
                             GL_UNSIGNED_BYTE,
                             (unsigned char*)_tm->getCurrentField());
              
               }else{
                  texture.getExtensions(state.getContextID(),false)->glTexSubImage3D(GL_TEXTURE_3D,
                             0,
                             0,0,0, 
                             _textureWidth,
                             _textureHeight,
                             _textureDepth, 
                             GL_RGBA, 
                             GL_UNSIGNED_BYTE,
                             (unsigned char*)_tm->getCurrentField());
               }
               ///reset the update flag so we don't subload every frame!!
               _update = false;
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
                             GL_LUMINANCE_ALPHA, 
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
