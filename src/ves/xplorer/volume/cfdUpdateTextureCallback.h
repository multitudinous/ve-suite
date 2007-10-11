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
#ifndef CFD_UPDATE_TEXTURE_CALLBACK_H
#define CFD_UPDATE_TEXTURE_CALLBACK_H
/*!\file cfdUpdateTextureCallback.h
* cfdUpdateTextureCallback API
*/

/*!\class VE_TextureBased::cfdUpdateTextureCallback
*
*/
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/Texture3D>
namespace osg
{
   class Node;
   class NodeVisitor;
   class State;
}
#include <ves/VEConfig.h>

namespace VE_TextureBased
{
   class cfdTextureManager;
}
namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdUpdateTextureCallback 
                              : public  osg::Texture3D::SubloadCallback{
      public:
         cfdUpdateTextureCallback();
         cfdUpdateTextureCallback(const cfdUpdateTextureCallback& cb);
         virtual ~cfdUpdateTextureCallback();
         enum SubloadMode {OFF,AUTO,IF_DIRTY};

         inline void setSubloadMode(const SubloadMode mode) { _subloadMode = mode; }
         inline const SubloadMode getSubloadMode() const { return _subloadMode; }
         inline void setSubloadTextureOffset(const int x, 
                                         const int y,
                                         const int z)
         {
            _subloadTextureOffsetX = x;
            _subloadTextureOffsetY = y;
            _subloadTextureOffsetZ = z;
         }

         /** Get the texture subload texture offsets. */
         inline void getSubloadTextureOffset(int& x, 
                                         int& y, 
                                         int& z) const
         {
            x = _subloadTextureOffsetX;
            y = _subloadTextureOffsetY;
            z = _subloadTextureOffsetZ; 
         }
        
         /** Set the texture subload width. If width or height are zero then
          * the repsective size value is calculated from the source image sizes. */
         inline void setSubloadTextureSize(const int width, 
                                       const int height,
                                       const int depth)
         {
            _textureWidth = width;
            _textureHeight = height;
            _textureDepth= depth;
         }

         /** Get the texture subload width. */
         inline void getSubloadTextureSize(int& width, 
                                       int& height, 
                                       int& depth) const
         {
            width = _textureWidth;
            height = _textureHeight;
            depth = _textureHeight;
         }
         /** Set the subload image offsets. */
         inline void setSubloadImageOffset(const int x,
                                       const int y,
                                       const int z)
         {
            _subloadImageOffsetX = x;
            _subloadImageOffsetY = y;
            _subloadImageOffsetZ = z;
         }

         /** Get the subload image offsets. */
         inline void getSubloadImageOffset(int& x, 
                                       int& y, 
                                       int& z) const
         {
            x = _subloadImageOffsetX;
            y = _subloadImageOffsetY;
            z = _subloadImageOffsetZ;
         }

         /** Set the image subload width. If width or height are zero then
           * the repsective size value is calculated from the source image sizes. */
         inline void setSubloadImageSize(const int width, 
                                      const int height,
                                      const int depth)
         {
            _subloadImageWidth = width;
            _subloadImageHeight = height;
            _subloadImageDepth= depth;
         }

         /** Get the image subload width. */
         inline void getSubloadImageSize(int& width, 
                                      int& height,
                                      int& depth) const
         {
            width = _subloadImageWidth;
            height = _subloadImageHeight;
            depth = _subloadImageDepth;
         }
    
         void SetIsLuminance(bool isLum){_isLuminance = isLum;}
         void SetTextureManager(cfdTextureManager* tm);
         void SetDelayTime(double delayTime);

         void SetCurrentFrame(unsigned int cFrame, bool forceUpdate = true);
         unsigned int GetCurrentFrame();

         void subload(const osg::Texture3D& texture,osg::State& state) const;
         void load(const osg::Texture3D& texture,osg::State&) const;
   
      protected:
         cfdTextureManager* _tm;
         double _delay;
         bool _isSlave;
         bool _isLuminance;
         mutable bool _update;
         unsigned int _currentFrame;
         SubloadMode _subloadMode;
         mutable GLsizei _textureWidth, _textureHeight,_textureDepth;
         GLint _subloadTextureOffsetX, _subloadTextureOffsetY,_subloadTextureOffsetZ;
         GLint _subloadImageOffsetX, _subloadImageOffsetY,_subloadImageOffsetZ;
         GLsizei _subloadImageWidth, _subloadImageHeight,_subloadImageDepth;
   };
}
#endif //OSG
#endif //CFD_UPDATE_TEXTURE_CALLBACK_H
