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
#include <osg/Matrixf>  


using namespace VE_TextureBased;
////////////////////////////////////////////////////
TextureBasedVolumeSlices::TextureBasedVolumeSlices()
:osg::Drawable()
{
   _bbox.init(); 
   _diagonal = 1.0;
   _center.set(0,0,0);
   _eyeCenter.set(0,0,0);
   _nSlices = 1;
   _deltaZ = 1;
}
//////////////////////////////////////////////////////////////////////////
TextureBasedVolumeSlices::TextureBasedVolumeSlices(float* dataBoundingBox,
                                                   unsigned int numberOfSlices)
:osg::Drawable()
{
   _nSlices = numberOfSlices;	
   SetDataBoundingBox(dataBoundingBox);
   
}
//////////////////////////////////////////////////////////////////////////////////////////
TextureBasedVolumeSlices::TextureBasedVolumeSlices(const TextureBasedVolumeSlices& slices,
                                                   const osg::CopyOp& copyop):
                                                   osg::Drawable(slices,copyop)
{
   _bbox.expandBy(slices._bbox);
   _center.set(slices._center);
   _eyeCenter.set(slices._eyeCenter);
   _diagonal = slices._diagonal;
   _nSlices = slices._nSlices;
   _deltaZ = slices._deltaZ;
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

   _center = _bbox.center();
   _diagonal = radius*2.0;
   _ensureSliceDelta();

   //The new bounding box
   minBBox[0] = _center[0] - radius;
   minBBox[1] = _center[1] - radius;
   minBBox[2] = _center[2] - radius;

   maxBBox[0] = _center[0] + radius;
   maxBBox[1] = _center[1] + radius;
   maxBBox[2] = _center[2] + radius;

   _bbox.set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]), 
              osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));
}
//////////////////////////////////////////////////
void TextureBasedVolumeSlices::_ensureSliceDelta()
{
   if(_diagonal && _nSlices)
   {
      _deltaZ = (_diagonal) / (float)(_nSlices-1);
   }
}
/////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::SetNumberOfSlices(unsigned int numberOfSlices)
{
   _nSlices = numberOfSlices;
   _ensureSliceDelta();
}
/////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::drawImplementation(osg::State& currentState) const
{
   ///transform center to current eye space
   _eyeCenter = _center*currentState.getModelViewMatrix();
   //transform the eye point in
   _modelViewMatrix.makeIdentity();
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
//////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_drawQuadSlice(float zPosition)const 
{
   glBegin(GL_QUADS);
      glColor4f(0,0,0,0);
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
