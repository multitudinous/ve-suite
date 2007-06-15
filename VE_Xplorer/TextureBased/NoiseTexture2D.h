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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef NOISE_TEXTURE_2D_H
#define NOISE_TEXTURE_2D_H
/*!\file NoiseTexture.h
  Texture-Based Volume Rendering NoiseTexture API
  */
/*!\class VE_TextureBased::NoiseTexture2D
 * Class defining noise texture.
 */
#include <osg/Image>
#include <osg/Texture2D>
#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
class TransferFunction;
class VE_TEXTURE_BASED_EXPORTS NoiseTexture2D
{
public:
   ///Constructor
   ///\param x The x size
   ///\param x The y size
   NoiseTexture2D(unsigned int x=32,unsigned int y =32);

   ///Copy Constructor
   NoiseTexture2D(const NoiseTexture2D& rhs);

   ///Destructor
   virtual ~NoiseTexture2D();

   ///\return The noise texture
   osg::Texture2D* GetNoiseTexture();

   ///Get the resolution
   ///\return The x resolution
   unsigned int GetResolutionX(){return _resolution[0];}

   ///Get the resolution
   ///\return The y resolution
   unsigned int GetResolutionY(){return _resolution[1];}
   ///equal operator
   ///\param rhs The right hand side
   NoiseTexture2D& operator=(const NoiseTexture2D& rhs);
protected:
   unsigned int _resolution[2];///<The resolution of the noise image
   unsigned char* _noiseData;///<The data
   osg::ref_ptr<osg::Texture2D> _noiseTexture;///<The noise texture
};
}
#endif //PREINTEGRATION_TEXTURE_2D_H
