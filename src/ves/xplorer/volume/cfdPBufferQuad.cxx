/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/volume/cfdPBufferQuad.h>

// --- OSG Includes --- //
#include <osg/BoundingBox>
#include <osg/Matrixd>
#include <osg/Texture3D>

// --- C/C++ Includes --- //
#include <iostream>

//need a better way to do this but leaving global for now
unsigned int curSlice = 1;
osg::ref_ptr< osg::Texture3D > texture;

using namespace ves::xplorer::volume;

////////////////////////////////////////////////////////////////////////////////
cfdPBufferQuad::cfdPBufferQuad()
    :
    osg::Drawable()
{
    _nSlices = 0;
    _curSlice = 0;
    _nearFarSlices = 0;
    _slices = 0;
    _bounds[0] = -1;
    _bounds[1] = 1;
    _bounds[2] = -1;
    _bounds[3] = 1;
    _bounds[4] = -1;
    _bounds[5] = 1;
    _deltaZ = 1;
    _bbSet = false;
    _sliceCountSet = false;
    _eye [0] = 0;
    _eye [1] = 0;
    _eye [2] = 0;
    _lookAt [0] = 0;
    _lookAt [1] = 0;
    _lookAt [2] = 0;
    _useAutoTexCoords = false;
    _w = 0;
    _h = 0;
    _d = 0;
    setComputeBoundingBoxCallback( new BBoxCallback( this ) );
}
////////////////////////////////////////////////////////////////////////////////
cfdPBufferQuad::cfdPBufferQuad(
    const cfdPBufferQuad& pbQuad,
    const osg::CopyOp& copyop )
    :
    osg::Drawable( pbQuad, copyop )
{
    _nSlices = pbQuad._nSlices;
    _curSlice = pbQuad._curSlice;
    _nearFarSlices = new float[_nSlices+1];
    _slices = new float[_nSlices];
    for( unsigned int i = 0; i < _nSlices; i++ )
    {
        _slices[i] = pbQuad._slices[i];
    }
    for( unsigned int i = 0; i < _nSlices + 1; i++ )
    {
        _nearFarSlices[i] = pbQuad._nearFarSlices[i];
    }
    _bounds[0] = pbQuad._bounds[0];
    _bounds[1] = pbQuad._bounds[1];
    _bounds[2] = pbQuad._bounds[2];
    _bounds[3] = pbQuad._bounds[3];
    _bounds[4] = pbQuad._bounds[4];
    _bounds[5] = pbQuad._bounds[5];
    _deltaZ = pbQuad._deltaZ;
    _bbSet = pbQuad._bbSet;
    _sliceCountSet = pbQuad._sliceCountSet;
    _eye [0] = pbQuad._eye[0];
    _eye [1] = pbQuad._eye[0];
    _eye [2] = pbQuad._eye[0];
    _lookAt [0] = pbQuad._lookAt[0];
    _lookAt [1] = pbQuad._lookAt[1];
    _lookAt [2] = pbQuad._lookAt[2];
    _useAutoTexCoords = pbQuad._useAutoTexCoords;
    _w = pbQuad._w;
    _h = pbQuad._h;
    _d = pbQuad._d;
    _texture = new osg::Texture3D( *pbQuad._texture );

}
////////////////////////////////////////////////////////////////////////////////
cfdPBufferQuad::~cfdPBufferQuad()
{
    if( _slices )
    {
        delete [] _slices;
        _slices = 0;
    }
    if( _nearFarSlices )
    {
        delete [] _nearFarSlices;
        _nearFarSlices  = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdPBufferQuad::SetTextureToUpdate( osg::Texture3D* t3d )
{
    _texture = t3d;
    texture = _texture;
}
////////////////////////////////////////////////////////////////////////////////
void cfdPBufferQuad::SetBBox( float* bbox )
{
    _bounds[0] = bbox[0];
    _bounds[1] = bbox[1];
    _bounds[2] = bbox[2];
    _bounds[3] = bbox[3];
    _bounds[4] = bbox[4];
    _bounds[5] = bbox[5];

    float minBBox[3];
    float maxBBox[3];

    //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
    minBBox[0] = _bounds[0];
    minBBox[1] = _bounds[2];
    minBBox[2] = _bounds[4];
    maxBBox[0] = _bounds[1];
    maxBBox[1] = _bounds[3];
    maxBBox[2] = _bounds[5];

    osg::BoundingBox bounds;
    bounds.set( osg::Vec3( minBBox[0], minBBox[1], minBBox[2] ),
                osg::Vec3( maxBBox[0], maxBBox[1], maxBBox[2] ) );

    float radius = bounds.radius();
    float center[3] = {0, 0, 0};

    //calculate the eye and look at for the pbuffer camera
    _lookAt[0] = bounds.center()[0];
    _lookAt[1] = bounds.center()[1];
    _lookAt[2] = bounds.center()[2];

    _eye[0] = center[0];
    _eye[1] = maxBBox[1] + radius;
    _eye[2] = center[2];

    _bbSet = true;
}
////////////////////////////////////////////////////////////////////////////////
void cfdPBufferQuad::SetTextureDimensions(
    unsigned int w, unsigned int h, unsigned int d )
{
    _h = h;
    _w = w;
    _d = d;
}

////////////////////////////////////////////////////////////////////////////////
void cfdPBufferQuad::SetUseAutoTexCoords( bool useAutoTCoords )
{
    _useAutoTexCoords = useAutoTCoords;
}
////////////////////////////////////////////////////////////////////////////////
void cfdPBufferQuad::SetNumberOfSlices( unsigned int ns )
{
    _nSlices = ns;
    _sliceCountSet = true;
}
////////////////////////////////////////////////////////////////////////////////
void cfdPBufferQuad::CalculateSlices()
{

    if( !_slices )
    {
        _slices = new float[_nSlices];
    }
    if( _useAutoTexCoords )
    {
        if( !_nearFarSlices )
        {
            _nearFarSlices = new float[_nSlices+1];
        }
        //transform our polys to view space
        osg::Matrix viewMatrix;
        viewMatrix.makeIdentity();

        //these are actually the poly slices and the n/f clip planes
        float dz = ( _bounds[5] - _bounds[4] ) / ( _nSlices - 1.0 );
        float dwz = dz + .001;
        float zDistance = fabs( _eye[2] - _bounds[4] ) + .001;
        float currentZ = zDistance;
        float worldz = _bounds[4];
        for( unsigned int i = 0; i < _nSlices; i++ )
        {
            _nearFarSlices[i] = currentZ;
            _slices[i] = worldz;
            currentZ -= dz;
            worldz += dz;
        }
        _nearFarSlices[_nSlices] = currentZ;
    }
    else
    {
        if( !_nearFarSlices )
        {
            _nearFarSlices = new float[_nSlices];
        }
        //these are the texture coordinate slices
        float dz = 1.0 / ( _nSlices - 1.0 );
        float worldDZ = 2.0 / ( _nSlices - 1.0 );
        float worldZ = -1.0;
        for( unsigned int i = 0; i < _nSlices; i++ )
        {
            _slices[i] = i * dz;
            _nearFarSlices[i] = worldZ + i * worldDZ;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdPBufferQuad::_drawAutoTexCoords() const
{
    //set up things for the current slice
    glMatrixMode( GL_PROJECTION );
    glPushMatrix();
    glLoadIdentity();
    glFrustum( _bounds[0], _bounds[1], _bounds[2], _bounds[3],
               _nearFarSlices[curSlice+1], _nearFarSlices[curSlice] );
    //set the camera
    glMatrixMode( GL_MODELVIEW );
    glPushMatrix();
    glLoadIdentity();
    osg::RefMatrix* matrix = new osg::RefMatrix;
    matrix->set( osg::Matrix::identity() );
    matrix->makeLookAt( osg::Vec3( _eye[0], _eye[1], _eye[2] ),
                        osg::Vec3( _lookAt[0], _lookAt[1], _lookAt[2] ),
                        osg::Vec3( 0.0f, 1.0f, 0.0f ) );

    glLoadMatrixd( matrix->ptr() );

    //draw our quad
    glBegin( GL_QUADS );
    glVertex3f( _bounds[0], _bounds[2], _slices[curSlice] );
    glVertex3f( _bounds[1], _bounds[2], _slices[curSlice] );
    glVertex3f( _bounds[1], _bounds[3], _slices[curSlice] );
    glVertex3f( _bounds[0], _bounds[3], _slices[curSlice] );
    glEnd();
    glPopMatrix();

    glMatrixMode( GL_PROJECTION );
    glPopMatrix();

    glMatrixMode( GL_MODELVIEW );

}
////////////////////////////////////////////////////////////////////////////////
void cfdPBufferQuad::_drawHardCodedTCoords( osg::State& state ) const
{

    float m[16];
    glGetFloatv( GL_TEXTURE_MATRIX, m );

    glMatrixMode( GL_MODELVIEW );
    glPushMatrix();
    glLoadIdentity();

    glMatrixMode( GL_PROJECTION );
    glPushMatrix();
    glLoadIdentity();

    glGetFloatv( GL_TEXTURE_MATRIX, m );

    glBegin( GL_POLYGON );
    glTexCoord3f( 0.0, 0.0, _slices[curSlice] );
    glVertex3f( -1.0, -1.0, -1 );

    glTexCoord3f( 1.0, 0.0, _slices[curSlice] );
    glVertex3f( 1.0, -1.0, -1 );

    glTexCoord3f( 1.0, 1.0, _slices[curSlice] );
    glVertex3f( 1.0, 1.0, -1 );

    glTexCoord3f( 0.0, 1.0, _slices[curSlice] );
    glVertex3f( -1.0, 1.0, -1 );
    glEnd();
    /*texture->copyTexSubImage3D(state,
                            1,1,curSlice,
                            1,1,_w-2,_h-2);*/

    glPopMatrix();
    glMatrixMode( GL_MODELVIEW );
    glPopMatrix();

}
////////////////////////////////////////////////////////////////////////////////
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
void cfdPBufferQuad::drawImplementation( osg::RenderInfo& renderState ) const
#elif ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR<=2))
void cfdPBufferQuad::drawImplementation( osg::State& renderState ) const
#endif
{
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
    osg::State& state = *( renderState.getState() );
#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
    osg::State& state = renderState;
#endif
    if( !_bbSet )
    {
        std::cout << "BBox not set for cfdPBufferQuad!!" << std::endl;
        return;
    }
    if( !_sliceCountSet )
    {
        std::cout << "Slice count not set for cfdPBufferQuad!!" << std::endl;
        return;
    }
    glClearColor( 0, 0, 0, 0 );
    if( curSlice == _nSlices - 1 )
    {
        //reset the current slice
        curSlice = 1;
        glColor4f( 1, 1, 1, 0 );

    }

    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    glDisable( GL_BLEND );
    if( _useAutoTexCoords )
    {
        _drawAutoTexCoords();
    }
    else
    {
        //disable auto-texture generation
        glDisable( GL_TEXTURE_GEN_S );
        glDisable( GL_TEXTURE_GEN_T );
        glDisable( GL_TEXTURE_GEN_R );

        _drawHardCodedTCoords( state );
    }
    curSlice++;
}
////////////////////////////////////////////////////////////////////////////////
/*bool cfdPBufferQuad::computeBound() const
{
   _bbox.init();
   if(!_bbSet)
      return false;

   float minBBox[3];
   float maxBBox[3];
   //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
   if(_useAutoTexCoords){
      minBBox[0] = _bounds[0];
      minBBox[1] = _bounds[2];
      minBBox[2] = _bounds[4];
      maxBBox[0] = _bounds[1];
      maxBBox[1] = _bounds[3];
      maxBBox[2] = _bounds[5];

   }else{
      minBBox[0] = -1;
      minBBox[1] = -1;
      minBBox[2] = -1;
      maxBBox[0] = 1;
      maxBBox[1] = 1;
      maxBBox[2] = 1;
   }
   _bbox.set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]),
                osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));
   _bbox_computed = true;

   return true;

}*/
////////////////////////////////////////////////////////////////////////////////
/*osg::BoundingBox cfdPBufferQuad::computeBound() const
{
   osg::BoundingBox bbox;
   bbox.init();
   if(!_bbSet)
      return bbox;

   float minBBox[3];
   float maxBBox[3];
   //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
   if(_useAutoTexCoords){
      minBBox[0] = _bounds[0];
      minBBox[1] = _bounds[2];
      minBBox[2] = _bounds[4];
      maxBBox[0] = _bounds[1];
      maxBBox[1] = _bounds[3];
      maxBBox[2] = _bounds[5];

   }else{
      minBBox[0] = -1;
      minBBox[1] = -1;
      minBBox[2] = -1;
      maxBBox[0] = 1;
      maxBBox[1] = 1;
      maxBBox[2] = 1;
   }
   bbox.set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]),
                osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));

   return bbox;
}*/
////////////////////////////////////////////////////////////////////////////////
osg::BoundingBox cfdPBufferQuad::BBoxCallback::computeBound(
    const osg::Drawable& quad ) const
{
    osg::BoundingBox bbox;
    bbox.init();
    if( !_pbq->_bbSet )
        return bbox;

    float minBBox[3];
    float maxBBox[3];
    //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
    if( _pbq->_useAutoTexCoords )
    {
        minBBox[0] = _pbq->_bounds[0];
        minBBox[1] = _pbq->_bounds[2];
        minBBox[2] = _pbq->_bounds[4];
        maxBBox[0] = _pbq->_bounds[1];
        maxBBox[1] = _pbq->_bounds[3];
        maxBBox[2] = _pbq->_bounds[5];

    }
    else
    {
        minBBox[0] = -1;
        minBBox[1] = -1;
        minBBox[2] = -1;
        maxBBox[0] = 1;
        maxBBox[1] = 1;
        maxBBox[2] = 1;
    }
    bbox.set( osg::Vec3( minBBox[0], minBBox[1], minBBox[2] ),
              osg::Vec3( maxBBox[0], maxBBox[1], maxBBox[2] ) );

    return bbox;

}
////////////////////////////////////////////////////////////////////////////////
