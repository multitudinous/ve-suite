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

#include "VE_Xplorer/TextureBased/TBVolumeSlices.h"
#include "VE_Xplorer/TextureBased/bboxEdgeConstants.h"
#include <osg/Matrixf>  
// --- VR Juggler Stuff --- //
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>

using namespace gmtl;
using namespace gadget;
#include <iostream>

using namespace VE_TextureBased;
////////////////////////////////////////////////////
TextureBasedVolumeSlices::TextureBasedVolumeSlices()
:osg::Drawable(),
_diagonal(1.0),
_center(0,0,0,1),
_eyeCenter(0,0,0,1),
_nSlices(1),
_deltaZ(1.f),
_sliceRenderMethod("VIEW_ALIGNED_POLYGON_INTERSECT")
{
   _bbox.init(); 
   _extremaIndicies[0] = 0;
   _extremaIndicies[1]= 7;
   _initBBoxIntersectionSlicesVertexProgram();
   _rotatedBBox = new osg::Vec4Array();
   _coordTransformedBBox = new osg::Vec4Array();
  
}
//////////////////////////////////////////////////////////////////////////
TextureBasedVolumeSlices::TextureBasedVolumeSlices(float* dataBoundingBox,
                                                   unsigned int numberOfSlices)
:osg::Drawable()
{
   _nSlices = numberOfSlices;	
   _sliceRenderMethod = "VIEW_ALIGNED_POLYGON_INTERSECT";
   
   _extremaIndicies[0] = 0;
   _extremaIndicies[1]= 7;
   _rotatedBBox = new osg::Vec4Array();
   _coordTransformedBBox = new osg::Vec4Array();
   SetDataBoundingBox(dataBoundingBox);
}
//////////////////////////////////////////////////////////////////////////////////////////
TextureBasedVolumeSlices::TextureBasedVolumeSlices(const TextureBasedVolumeSlices& slices,
                                                   const osg::CopyOp& copyop):
                                                   osg::Drawable(slices,copyop)
{
   _bbox.expandBy(slices._bbox);
   _center = slices._center;
   _eyeCenter = slices._eyeCenter;
   _diagonal = slices._diagonal;
   _nSlices = slices._nSlices;
   _deltaZ = slices._deltaZ;
   _sliceRenderMethod = slices._sliceRenderMethod;
}
/////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_initBBoxIntersectionSlicesVertexProgram()
{
   _bboxSlicer.clear();
   //values change per brick
   _bboxSlicer.append("uniform vec3 center;n");
   _bboxSlicer.append("uniform float dPlaneStart;\n");

   //values change per frame
   _bboxSlicer.append("uniform int frontBBoxVertexIndex;\n");
   _bboxSlicer.append("uniform vec3 viewVector;\n");

   //constants
   _bboxSlicer.append("uniform float deltaPlane;\n");
   _bboxSlicer.append("uniform int edgeSequence[64];\n");
   _bboxSlicer.append("uniform vec3 bbox[8];\n");
   _bboxSlicer.append("uniform int edgeBegin[24];\n");
   _bboxSlicer.append("uniform int edgeEnd[24];\n");

   _bboxSlicer.append("void main();\n");
   _bboxSlicer.append("{\n");

   //calculate our plane position
   _bboxSlicer.append("float dPlane = dPlaneStart + gl_Vertex.y*deltaPlane;\n");
   _bboxSlicer.append("float planePosition;\n");

   //loop over the possible edges of intersection
   _bboxSlicer.append("   for(int e = 0; e < 4; ++e)\n");
   _bboxSlicer.append("   {\n");

   //calculate the beginning and end verticies of the edge
   _bboxSlicer.append("      int beginIndex = edgeSequence[int(frontBBoxVertexIndex*8 + edgeBegin[gl_Vertex.x*4 + e])];\n");
   _bboxSlicer.append("      int endIndex = edgeSequence[int(frontBBoxVertexIndex*8 + edgeEnd[gl_Vertex.x*4 + e])];\n");
   _bboxSlicer.append("      vec3 edgeOrigin = bbox[beginIndex];\n");
   _bboxSlicer.append("      vec3 edgeEndPoint = bbox[endIndex];\n");
   _bboxSlicer.append("      vec3 edgeStart = edgeOrigin + center;\n;");
   _bboxSlicer.append("      vec3 edgeDirection = edgeEndPoint - edgeOrigin;\n;");
   _bboxSlicer.append("      float denominator = dot(edgeDirection,viewVector);\n");
   _bboxSlicer.append("      float lambda = (denom!=0.0)?(dPlane-dot(edgeStart,edgeDirection))/denominator:-1.0;\n");
   _bboxSlicer.append("      if((lambda >= 0.0)&&(lambda <=1.0));\n");
   _bboxSlicer.append("      {\n");
   _bboxSlicer.append("         planePosition = edgeStart + lambda*edgeDirection;\n");
   _bboxSlicer.append("         break;\n");
   _bboxSlicer.append("      }\n");
   _bboxSlicer.append("      gl_Position = gl_ModelViewMatrix*vec4(gl_Vertex,1.0);\n");
   _bboxSlicer.append("      gl_TexCoord[0] = .05*_planePosition + .5;\n");
   _bboxSlicer.append("      }\n");
   
   _bboxSlicer.append("   }\n");
   _bboxSlicer.append("}\n");


}
/////////////////////////////////////////////////////////////////////  
void TextureBasedVolumeSlices::SetDataBoundingBox(float* boundingBox)
{
   float minBBox[3] = {0,0,0};
   float maxBBox[3] = {1,1,1};

   //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
   minBBox[0] = boundingBox[0]; 
   minBBox[1] = boundingBox[2]; 
   minBBox[2] = boundingBox[4]; 
   maxBBox[0] = boundingBox[1]; 
   maxBBox[1] = boundingBox[3]; 
   maxBBox[2] = boundingBox[5];

   //The original bounding box
   _bbox.set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]), 
              osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));

   //Update the essentials
   float radius = _bbox.radius();

   _center = osg::Vec4(_bbox.center()[0],_bbox.center()[1],_bbox.center()[2],1);
   _diagonal = radius*2.0;
   

   _coordTransformedBBox->clear();
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(4),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(5),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(0),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(1),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(6),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(7),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(2),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(3),1));
   
   /*_coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(0),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(1),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(4),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(5),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(2),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(3),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(6),1));
   _coordTransformedBBox->push_back(osg::Vec4(_bbox.corner(7),1));*/
   _ensureSliceDelta();
}
//////////////////////////////////////////////////
void TextureBasedVolumeSlices::_ensureSliceDelta()const 
{
   if(_diagonal && _nSlices)
   {
      float currentDistance = (_coordTransformedBBox->at(_extremaIndicies[1]) - _coordTransformedBBox->at(_extremaIndicies[0])).length();
      _deltaZ = (currentDistance) / (float)(_nSlices);
   }
}
/////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::SetNumberOfSlices(unsigned int numberOfSlices)
{
   _nSlices = (numberOfSlices<32)?32:numberOfSlices;
   _ensureSliceDelta();
}
//////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::SetRenderMethod(std::string method)
{
   _sliceRenderMethod = method;
}
////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_drawViewAlignedQuadSlices()const
{
   //transform the eye point in
   glMatrixMode(GL_MODELVIEW);

   //push the modelview matrix
   glPushMatrix();
      glLoadIdentity();
      float currentZ = _eyeCenter[2]-(.5*_diagonal);
      ///draw the eye space slices
      for(unsigned int z = 0; z < _nSlices; ++z)
      {
         _drawQuadSlice(currentZ); 
         currentZ += _deltaZ;
      } 
   //pop the modelview matrix
   glPopMatrix();
}
/////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::drawImplementation(osg::State& currentState) const
{
   if(_sliceRenderMethod == "VIEW_ALIGNED_QUADS")
   {
      ///transform center to current eye space
      _eyeCenter = _center*currentState.getModelViewMatrix();
      _drawViewAlignedQuadSlices();
   }
   else if(_sliceRenderMethod == "VIEW_ALIGNED_POLYGON_INTERSECT")
   {
      //Calculate the camera position
      osg::Matrixd inverseModelView;
      inverseModelView.invert(currentState.getModelViewMatrix());
      _cameraLocation = osg::Vec4(0,0,0,1)*inverseModelView;

      _extremaIndicies[0] = 0;
      _extremaIndicies[1]= 7;

      _rotatedBBox->clear();

      ///transform the bbox into camera space
      _rotatedBBox->push_back(_coordTransformedBBox->at(0)*currentState.getModelViewMatrix());
      _rotatedBBox->push_back(_coordTransformedBBox->at(1)*currentState.getModelViewMatrix());
      _rotatedBBox->push_back(_coordTransformedBBox->at(2)*currentState.getModelViewMatrix());
      _rotatedBBox->push_back(_coordTransformedBBox->at(3)*currentState.getModelViewMatrix());
      _rotatedBBox->push_back(_coordTransformedBBox->at(4)*currentState.getModelViewMatrix());
      _rotatedBBox->push_back(_coordTransformedBBox->at(5)*currentState.getModelViewMatrix());
      _rotatedBBox->push_back(_coordTransformedBBox->at(6)*currentState.getModelViewMatrix());
      _rotatedBBox->push_back(_coordTransformedBBox->at(7)*currentState.getModelViewMatrix());

      //calculate the slice plane normal
      _slicePlaneNormal = _cameraLocation -_center;
      _slicePlaneNormal.normalize();

      //update the min max indicies for the bbox
      _findBBoxMinMaxIndicies();

      //update the slice delta
      _ensureSliceDelta();
      
      //initial slicing point 
      osg::Vec4 initialSlicePoint(_coordTransformedBBox->at(_extremaIndicies[1]).x(),
                                  _coordTransformedBBox->at(_extremaIndicies[1]).y(),
                                  _coordTransformedBBox->at(_extremaIndicies[1]).z(),1);
      
      //Calcluate edge intersections
      _calculateEdgeIntersections(initialSlicePoint);
   }
}
////////////////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_calculateEdgeIntersections(osg::Vec4 initialSlicePoint)const
{
   osg::Vec4 sliceDelta = _slicePlaneNormal*_deltaZ;
   for(unsigned int i = 0; i <= _nSlices; ++i)
   {
      float verts[18]={0,0,0,
                       0,0,0,
                       0,0,0,
                       0,0,0,
                       0,0,0,
                       0,0,0};


      ///calculate vertex position from intersections
      for(unsigned int j = 0; j < 6; ++j)
      {
         _calculateVertsAndTextureCoordinates(j,initialSlicePoint,verts);
      }
      //render the polygons...
      glBegin(GL_POLYGON);
         glVertex3f(verts[0],verts[1],verts[2]);
         glVertex3f(verts[3],verts[4],verts[5]);
         glVertex3f(verts[6],verts[7],verts[8]);
         glVertex3f(verts[9],verts[10],verts[11]);
         glVertex3f(verts[12],verts[13],verts[14]);
         glVertex3f(verts[15],verts[16],verts[17]);
      glEnd();
      initialSlicePoint+= sliceDelta;
   }
}
////////////////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_calculateVertsAndTextureCoordinates(unsigned int currentEdgeIndex,
                                                                    osg::Vec4 initSlicePoint,
                                                                    float* verts)const
{
   ///texture coordinates aren't calculated because we are auto generating them now
   ///we can add the calculation of the second coordinate if we need to manually here
   osg::Vec4 edgeInitial;
   osg::Vec4 edgeEnd;
   for(unsigned int i = 0; i < 4; ++i)
   {
      edgeInitial = _coordTransformedBBox->at(edgeSequence[_extremaIndicies[0]][edgeIndex[(currentEdgeIndex * 4) +
                                           i][0]]);
      edgeEnd = _coordTransformedBBox->at(edgeSequence[_extremaIndicies[0]][edgeIndex[(currentEdgeIndex * 4) +
                                          i][1]]);
      osg::Vec4 numerator = initSlicePoint - edgeInitial;
      osg::Vec4 denom = edgeEnd-edgeInitial;
      
      float t = (denom!=(osg::Vec4(0,0,0,1)))?(_slicePlaneNormal*numerator)/(_slicePlaneNormal*denom):-1;
      if((t >= 0) && (t <= 1)) 
      {
         // Compute the line intersection
         float x = (float) (edgeInitial.x() + (t * (edgeEnd.x() - edgeInitial.x())));
         float y = (float) (edgeInitial.y() + (t * (edgeEnd.y() - edgeInitial.y())));
         float z = (float) (edgeInitial.z() + (t * (edgeEnd.z() - edgeInitial.z())));
         verts[currentEdgeIndex*3    ] = x;
         verts[currentEdgeIndex*3 + 1] = y;
         verts[currentEdgeIndex*3 + 2] = z;
         break;
      }
   }
}
///////////////////////////////////////////////////////////////////////////////////
float TextureBasedVolumeSlices::_calculateSampleDistance(osg::Matrixf iModelView) const
{
   /*
      Calculate the the minimum and maximum x-, y- or z-position of the
         rotated bounding box. Equivalent to but quicker than:
         maximumD = (maximum, 0, 0), (0, maximum, 0) or (0, 0, maximum);
         minimumD = (minimum, 0, 0), (0, minimum, 0) or (0, 0, minimum);
         maximumV = modelviewInverse * maximumD;
         minimumV = modelviewInverse * minimumD;
       */
   osg::Vec3 maximumV;
   osg::Vec3 minimumV;

   maximumV.set(iModelView.ptr()[8]* _extremaIndicies[1],
                iModelView.ptr()[9]* _extremaIndicies[1],
                iModelView.ptr()[10]* _extremaIndicies[1]);
   minimumV.set(iModelView.ptr()[8] * _extremaIndicies[0],
                iModelView.ptr()[9] * _extremaIndicies[0],
                iModelView.ptr()[10] * _extremaIndicies[0]);

   maximumV-=minimumV;

   return maximumV.length();
} 
//////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_findBBoxMinMaxIndicies()const 
{
   float extremes[2] = {0,0};
   float poleMultiplier = 20.0;
   osg::Vec4 pole;
   pole = _center + _slicePlaneNormal*_diagonal;
   float diff = (pole-_rotatedBBox->at(0)).length();


   extremes[0]  = diff;
   extremes[1]  = -diff;
   //std::cout<<"========================================="<<std::endl;
   //std::cout<<"Pole:"<<std::endl;
   //std::cout<<" "<<pole.x()<<" "<<pole.y()<<" "<<pole.z()<<std::endl;
   for(unsigned int i = 0; i < 8; ++i) 
   {
      diff = (pole-_rotatedBBox->at(i)).length();
      ///closest corner
      if(diff < extremes[0])
      {
         extremes[0] = diff;
         _extremaIndicies[0] = i;
      }
      ///furthest corner
      if(diff > extremes[1]) 
      {
         extremes[1] = diff;
         _extremaIndicies[1] = i;
      }
   } 
   //std::cout<<"BBox Extrema"<<std::endl;
   //std::cout<<"(min,max)"<<std::endl;
   //std::cout<<_extremaIndicies[0]<<","<<_extremaIndicies[1]<<std::endl;

   //std::cout<<"Extrema coords:"<<std::endl;
   //std::cout<<_coordTransformedBBox->at(_extremaIndicies[0]).x()<<","<<_coordTransformedBBox->at(_extremaIndicies[0]).y()<<","<<_coordTransformedBBox->at(_extremaIndicies[0]).z()<<std::endl;
   //std::cout<<_coordTransformedBBox->at(_extremaIndicies[1]).x()<<","<<_coordTransformedBBox->at(_extremaIndicies[1]).y()<<","<<_coordTransformedBBox->at(_extremaIndicies[1]).z()<<std::endl;
}
///////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_drawQuadSlice(float zPosition)const 
{
   glBegin(GL_QUADS);
      glColor4f(0,0,0,1);
      glVertex3f((_eyeCenter[0]-.5*_diagonal),(_eyeCenter[1]-.5*_diagonal),zPosition);
      glVertex3f((_eyeCenter[0]+.5*_diagonal),(_eyeCenter[1]-.5*_diagonal),zPosition);
      glVertex3f((_eyeCenter[0]+.5*_diagonal),(_eyeCenter[1]+.5*_diagonal),zPosition);
      glVertex3f((_eyeCenter[0]-.5*_diagonal),(_eyeCenter[1]+.5*_diagonal),zPosition);
   glEnd();
}
///////////////////////////////////////////////////////////////
osg::BoundingBox TextureBasedVolumeSlices::computeBound() const
{
  return _bbox; 
}

