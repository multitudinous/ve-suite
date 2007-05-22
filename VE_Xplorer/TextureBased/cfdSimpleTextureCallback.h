/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#ifndef CFD_SIMPLE_TEXTURE_CALLBACK_H
#define CFD_SIMPLE_TEXTURE_CALLBACK_H
/*!\file cfdSimpleTextureCallback.h
* cfdSimpleTextureCallback API
*/

/*!\class VE_TextureBased::cfdSimpleTextureCallback
*
*/
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/Texture3D>
#include "VE_Installer/include/VEConfig.h"
namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdSimpleTextureCallback 
      : public  osg::Texture3D::SubloadCallback{
      public:
         cfdSimpleTextureCallback();
         virtual ~cfdSimpleTextureCallback(){}
        
         void setTextureSize(const int width,
                           const int height, 
                           const int depth)
         {
            _textureWidth = width;
            _textureHeight = height;
            _textureDepth= depth;
         }
         void SetIsLuminance(bool isLum){_isLuminance = isLum;} 
         void subload(const osg::Texture3D& texture,osg::State& state) const;
         void load(const osg::Texture3D& texture,osg::State&) const;
   
      protected:
         bool _isLuminance;
         mutable GLsizei _textureWidth, _textureHeight,_textureDepth;
   };
}
#endif //OSG
#endif //CFD_SIMPLE_TEXTURE_CALLBACK_H

