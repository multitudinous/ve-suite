/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef VE_PATENTED
#include "VE_TextureBased/cfdSimpleTextureCallback.h"
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/State>
#include <osg/TexMat>
#include <osg/StateAttribute>
#include <iostream>
using namespace VE_TextureBased;
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
