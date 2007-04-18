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
 * Date modified: $Date: 2007-04-05 15:23:00 -0500 (Thu, 05 Apr 2007) $
 * Version:       $Rev: 7270 $
 * Author:        $Author: biv $
 * Id:            $Id: cfdVolumeVisualization.h 7270 2007-04-05 20:23:00Z biv $
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PREINTEGRATION_TEXTURE_2D_H
#define PREINTEGRATION_TEXTURE_2D_H
/*!\file PreIntegrationTexture.h
  Texture-Based Volume Rendering PreIntegrationTexture API
  */
/*!\class VE_TextureBased::PreIntegrationTexture2D
 * Class defining preintegrated classification texture for texture-based volume rendering .
 */
#include <osg/Image>
#include <osg/Texture2D>
#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
class TransferFunction;
class VE_TEXTURE_BASED_EXPORTS PreIntegrationTexture2D
{
public:
   ///Constructor
   PreIntegrationTexture2D();

   ///Copy Constructor
   PreIntegrationTexture2D(const PreIntegrationTexture2D& rhs);

   ///Destructor
   virtual ~PreIntegrationTexture2D();

   ///Set the transfer function for preintegrations
   ///\param tf The 1D transfer function
   void SetTransferFunction(TransferFunction* tf);

   ///Update the 2D preintegration texture
   void FullUpdate();

   ///Only update the diagonal values
   void FastUpdate();

   ///\return The Pre-Integrated texture
   osg::Texture2D* GetPreIntegratedTexture();

   ///equal operator
   ///\param rhs The right hand side
   PreIntegrationTexture2D& operator=(const PreIntegrationTexture2D& rhs);
protected:
   ///Initialize the front and back integration values based on 
   ///the current transfer function data
   void _initializeSliceIntegrationValues();

   ///Calculate a specific component for the preintegration table
   ///\param ds Distance between slices
   ///\param component The rgba component to calculate 
   ///\param sliceMin The minimum slice value
   ///\param sliceMax The maximum slice value
   unsigned char _calculateComponent(float ds, unsigned int component,
                            unsigned int sliceMin, unsigned int sliceMax);

   TransferFunction* _tf;///<The 1D transfer function
   float* _sliceIntegrationValues;///<The preintegrated values
   unsigned char* _rawData;///<The raw texture data 
   osg::ref_ptr<osg::Image> _imageData;///<The image
   osg::ref_ptr<osg::Texture2D> _preIntegratedTexture;///<The image
};
}
#endif //PREINTEGRATION_TEXTURE_2D_H
