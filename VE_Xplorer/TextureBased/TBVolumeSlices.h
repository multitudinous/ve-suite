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

#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>
#include <gadget/Type/PositionInterface.h>
//#include <gadget/Type/DigitalInterface.h>

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


   ///Set the number of slices
   ///\param numberOfSlices The number of slices to render
   void SetNumberOfSlices(unsigned int numberOfSlices);

   ///Set the method for rendering the slices
   ///\param method String representing the rendering method\n "VIEW_ALIGNED_QUADS"\n "VIEW_ALIGNED_POLYGON_INTERSECT"
   void SetRenderMethod(std::string method);

   ///Set the texture resolution
   ///\param x The x resolution
   ///\param x The y resolution
   ///\param x The z resolution
   void SetTextureDimensions(unsigned int x,unsigned int y,unsigned int z);
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
   void _ensureSliceDelta()const;

   //These all need to be in a higher level class
   ///Draw a stack of view aligned quads
   void _drawViewAlignedQuadSlices()const;
 
   ///Initialize the bbox intersecting slices vertex program.
   void _initBBoxIntersectionSlicesVertexProgram();

   ///Initialize the edge list. This should be in a higher level class( brick manager)
   void _initEdges();
   ///Convert edge table and sequence table from Patrick's coordinate system to ours
   void _convertEdgeTable();

   ///Calculate the bbox min and max indicies
   void _findBBoxMinMaxIndicies()const;
   
   ///Calculate the sample distance
   ///\param iModelView Inverse modelview matrix
   float _calculateSampleDistance(osg::Matrixf iModelView) const;


   ///Calculate the slice polygon and edge intersections
   ///\param currentState The current opengl state
   ///\param initalSlicePoint The coord to begin calculating slices
   void _calculateEdgeIntersections(osg::State& currentState,osg::Vec4 initialSlicePoint)const;

   ///Calculate the intersection verticies and assosiated texture coordinates
   ///\param currentEdgeIndex The current edge we are calculationg intersection with
   ///\param frontSlicePoint The initial slice location
   ///\param backSlicePoint The back slice location
   ///\param verts The vertcies for the intersecting polygon
   ///\param backTCoords The texture coordinates for the back intersecting polygon
   void _calculateVertsAndTextureCoordinates(unsigned int currentEdgeIndex,
                                             osg::Vec4 frontSlicePoint,
                                             osg::Vec4 backSlicePoint,
                                             float* verts,float* frontTCoords,float* backTCoords)const;
   
   ///Calculate the sample distance
   ///\param minimum Minimum z distance
   ///\param maximum Maximum z distance
   float _calculateDelta(/*osg::Vec3 minimum,osg::Vec3 maximum*/)const;

   std::string _sliceRenderMethod;///<Method for rendering intersection polygons
   std::string _bboxSlicer;///<Source code for creating intersecting bbox slices vertex program.
   mutable unsigned int _nSlices;///<The number of slices to render.
   mutable float _deltaZ;///<The slice spacing.
   float _diagonal;///<The length of the diagonal
   osg::BoundingBox _bbox;///<The bbox constructed from the diagonal of the data
   mutable osg::Vec3 _dimensions;///<The dimensions width-X,depth-Y,height-Z
   mutable osg::Vec4 _center;///<The center of the data
   mutable osg::Vec4 _cameraLocation;///<The camera location in the world
   mutable osg::Vec4 _eyeCenter;///<The center of the data in eye space.

   ///These should be moved to a class that handles brick management
   mutable osg::BoundingBox _eyeSpaceBBox;///<The bbox in eyespace
   mutable osg::Vec4 _slicePlaneNormal;///<The negated view vector
   mutable unsigned int _extremaIndicies[2];///<Holder for the closest and furthest corner of the bbox
   mutable osg::ref_ptr<osg::Vec4Array> _rotatedBBox;///<The rotated bbox.
   mutable osg::ref_ptr<osg::Vec4Array> _coordTransformedBBox;///<The rotated bbox.
   mutable osg::ref_ptr<osg::Vec4Array> _tcoordBBox;///<The rotated bbox.
   mutable gadget::PositionInterface head;///<vjPosInterface Head Position from Juggler;
};
}
#endif //_OSG
#endif// TEXTURE_BASED_VOLUME_SLICES_H
