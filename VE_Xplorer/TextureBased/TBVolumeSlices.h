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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TEXTURE_BASED_VOLUME_SLICES_H
#define TEXTURE_BASED_VOLUME_SLICES_H
#ifdef _OSG

/*!\file TBVolumeSlices.h
  Texture-Based Volume Rendering Slices API
  */
/*!\class VE_TextureBased::TextureBasedVolumeSlices
 * Class defining slices for texture-based volume rendering .
 */

#include "VE_Installer/include/VEConfig.h"

#include <osg/Drawable>
namespace osg
{
   class Matrixf;  
}
namespace VE_TextureBased
{
class  VE_TEXTURE_BASED_EXPORTS TextureBasedVolumeSlices : public osg::Drawable
{
public:
   ///Constructor
   TextureBasedVolumeSlices();

   ///Constructor
   ///\param dataBoundingBox The data bounding box
   ///\param minNumberOfSlices The minimum number of slices to use.\n Lowest is 32.
   TextureBasedVolumeSlices(float* dataBoundingBox, 
                            unsigned int minNumberOfSlices=32);

   ///Copy constructor
   TextureBasedVolumeSlices(const TextureBasedVolumeSlices& slices,
                            const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY);

   META_Object(VE_TextureBased,VE_TextureBased::TextureBasedVolumeSlices)
  
   ///Update the bounding box for this dataset
   ///\param dataBoundingBox The data bounding box
   void SetDataBoundingBox(float* boundingBox);


   void SetNumberOfSlices(unsigned int numberOfSlices);

   // of OpenGL primitives.
   virtual void drawImplementation(osg::State& currentState) const;

   // we need to set up the bounding box of the data too, so that the scene graph knows where this
   // objects is, for both positioning the camera at start up, and most importantly for culling.
   virtual osg::BoundingBox computeBound() const;
protected:
   ///Draw a quad along the lookAtVector of the camera at position z
   ///\param zPosition Distance along the lookAtVector to draw the slice
   void _drawQuadSlice(float zPosition)const;

   ///Make sure the slice spacing is up to date
   void _ensureSliceDelta();
 
   ///Initialize the bbox intersecting slices vertex program.
   void _initBBoxIntersectionSlicesVertexProgram();

   std::string _bboxSlicer;///<Source code for creating intersecting bbox slices vertex program.
   unsigned int _nSlices;///<The number of slices to render.
   float _deltaZ;///<The slice spacing.
   float _diagonal;///<The length of the diagonal
   osg::BoundingBox _bbox;///<The bbox constructed from the diagonal of the data
   mutable osg::Matrixf _modelViewMatrix;///<The current model view matrix;
   mutable osg::Vec3 _center;///<The center of the data
   mutable osg::Vec3 _eyeCenter;///<The center of the data in eye space.
};
}
#endif //_OSG
#endif// TEXTURE_BASED_VOLUME_SLICES_H
