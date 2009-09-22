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
#include <ves/xplorer/volume/TBVolumeSlices.h>
#include <ves/xplorer/volume/bboxEdgeConstants.h>
#include <ves/xplorer/volume/ExternalPixelBufferObject.h>

// --- OSG Includes --- //
#include <osg/Matrixf>
#include <osg/GL2Extensions>
#include <osg/AlphaFunc>
#include <osg/io_utils>

// --- VR Juggler Stuff --- //
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>

// --- C/C++ Includes --- //
#include <iostream>

using namespace gmtl;
using namespace gadget;

using namespace ves::xplorer::volume;

////////////////////////////////////////////////////////////////////////////////
TextureBasedVolumeSlices::TextureBasedVolumeSlices()
    :
    osg::Drawable(),
    _diagonal( 1.0 ),
    _center( 0.0, 0.0, 0.0 ),
    _eyeCenter( 0.0, 0.0, 0.0 ),
    _nSlices( 1 ),
    _sliceDeltaRatio( 1.0 ),
    //_deltaZ( 1.0 ),
    _sliceRenderMethod( "VIEW_ALIGNED_POLYGON_INTERSECT" )
{
    _bbox.init();
    _extremaIndicies[ 0 ] = 0;
    _extremaIndicies[ 1 ] = 7;
    _initBBoxIntersectionSlicesVertexProgram();
    _rotatedBBox = new osg::Vec4Array();
    _coordTransformedBBox = new osg::Vec4Array();
    _tcoordBBox = new osg::Vec4Array();

    ///these are fixed
    _tcoordBBox->push_back( osg::Vec4( 0.0, 0.0, 1.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 1.0, 0.0, 1.0, 1.0 ) );

    _tcoordBBox->push_back( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 0.0, 1.0, 1.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 1.0, 1.0, 0.0, 1.0 ) );

    _nSlices = 100;
    _deltaZ[ 0 ] = _deltaZ[ 1 ] = 1.0;
}
////////////////////////////////////////////////////////////////////////////////
TextureBasedVolumeSlices::TextureBasedVolumeSlices(
    float* dataBoundingBox,
    unsigned int numberOfSlices )
    :
    osg::Drawable()
{
    _nSlices = 100;
    _deltaZ[ 0 ] = _deltaZ[1] = 1.0;
    _sliceDeltaRatio = 1.0;
    _sliceRenderMethod = "VIEW_ALIGNED_POLYGON_INTERSECT";

    _extremaIndicies[ 0 ] = 0;
    _extremaIndicies[ 1 ] = 7;
    _rotatedBBox = new osg::Vec4Array();
    _coordTransformedBBox = new osg::Vec4Array();
    SetDataBoundingBox( dataBoundingBox );

    _tcoordBBox = new osg::Vec4Array();

    ///these are fixed
    _tcoordBBox->push_back( osg::Vec4( 0.0, 0.0, 1.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 1.0, 0.0, 1.0, 1.0 ) );

    _tcoordBBox->push_back( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 0.0, 1.0, 1.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );
    _tcoordBBox->push_back( osg::Vec4( 1.0, 1.0, 0.0, 1.0 ) );
}
////////////////////////////////////////////////////////////////////////////////
TextureBasedVolumeSlices::TextureBasedVolumeSlices(
    const TextureBasedVolumeSlices& slices,
    const osg::CopyOp& copyop )
    :
    osg::Drawable( slices, copyop )
{
    _bbox.expandBy( slices._bbox );
    _center = slices._center;
    _eyeCenter = slices._eyeCenter;
    _diagonal = slices._diagonal;
    _nSlices = slices._nSlices;
    _deltaZ[ 0 ] = slices._deltaZ[ 0 ];
    _deltaZ[ 1 ] = slices._deltaZ[ 1 ];
    _sliceDeltaRatio = slices._sliceDeltaRatio;
    _sliceRenderMethod = slices._sliceRenderMethod;
    _tcoordBBox = new osg::Vec4Array( *slices._tcoordBBox );
    _coordTransformedBBox = new osg::Vec4Array( *slices._coordTransformedBBox );
    _rotatedBBox = new osg::Vec4Array( *slices._rotatedBBox );
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_initBBoxIntersectionSlicesVertexProgram()
{
    _bboxSlicer.clear();
    //values change per brick
    _bboxSlicer.append( "uniform vec3 center;n" );
    _bboxSlicer.append( "uniform float dPlaneStart;\n" );

    //values change per frame
    _bboxSlicer.append( "uniform int frontBBoxVertexIndex;\n" );
    _bboxSlicer.append( "uniform vec3 viewVector;\n" );

    //constants
    _bboxSlicer.append( "uniform float deltaPlane;\n" );
    _bboxSlicer.append( "uniform int edgeSequence[64];\n" );
    _bboxSlicer.append( "uniform vec3 bbox[8];\n" );
    _bboxSlicer.append( "uniform int edgeBegin[24];\n" );
    _bboxSlicer.append( "uniform int edgeEnd[24];\n" );

    _bboxSlicer.append( "void main();\n" );
    _bboxSlicer.append( "{\n" );

    //calculate our plane position
    _bboxSlicer.append( "float dPlane = dPlaneStart + gl_Vertex.y*deltaPlane;\n" );
    _bboxSlicer.append( "float planePosition;\n" );

    //loop over the possible edges of intersection
    _bboxSlicer.append( "   for(int e = 0; e < 4; ++e)\n" );
    _bboxSlicer.append( "   {\n" );

    //calculate the beginning and end verticies of the edge
    _bboxSlicer.append( "      int beginIndex = edgeSequence[int(frontBBoxVertexIndex*8 + edgeBegin[gl_Vertex.x*4 + e])];\n" );
    _bboxSlicer.append( "      int endIndex = edgeSequence[int(frontBBoxVertexIndex*8 + edgeEnd[gl_Vertex.x*4 + e])];\n" );
    _bboxSlicer.append( "      vec3 edgeOrigin = bbox[beginIndex];\n" );
    _bboxSlicer.append( "      vec3 edgeEndPoint = bbox[endIndex];\n" );
    _bboxSlicer.append( "      vec3 edgeStart = edgeOrigin + center;\n;" );
    _bboxSlicer.append( "      vec3 edgeDirection = edgeEndPoint - edgeOrigin;\n;" );
    _bboxSlicer.append( "      float denominator = dot(edgeDirection,viewVector);\n" );
    _bboxSlicer.append( "      float lambda = (denom!=0.0)?(dPlane-dot(edgeStart,edgeDirection))/denominator:-1.0;\n" );
    _bboxSlicer.append( "      if((lambda >= 0.0)&&(lambda <=1.0));\n" );
    _bboxSlicer.append( "      {\n" );
    _bboxSlicer.append( "         planePosition = edgeStart + lambda*edgeDirection;\n" );
    _bboxSlicer.append( "         break;\n" );
    _bboxSlicer.append( "      }\n" );
    _bboxSlicer.append( "      gl_Position = gl_ModelViewMatrix*vec4(gl_Vertex,1.0);\n" );
    _bboxSlicer.append( "      gl_TexCoord[0] = .05*_planePosition + .5;\n" );
    _bboxSlicer.append( "      }\n" );

    _bboxSlicer.append( "   }\n" );
    _bboxSlicer.append( "}\n" );

}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::SetDataBoundingBox( float* boundingBox )
{
    double minBBox[ 3 ] = { 0.0, 0.0, 0.0 };
    double maxBBox[ 3 ] = { 1.0, 1.0, 1.0 };

    //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
    minBBox[ 0 ] = boundingBox[ 0 ];
    minBBox[ 1 ] = boundingBox[ 2 ];
    minBBox[ 2 ] = boundingBox[ 4 ];
    maxBBox[ 0 ] = boundingBox[ 1 ];
    maxBBox[ 1 ] = boundingBox[ 3 ];
    maxBBox[ 2 ] = boundingBox[ 5 ];

    //The original bounding box
    _bbox.set(
        osg::Vec3d( minBBox[ 0 ], minBBox[ 1 ], minBBox[ 2 ] ),
        osg::Vec3d( maxBBox[ 0 ], maxBBox[ 1 ], maxBBox[ 2 ] ) );

    //Update the essentials
    double radius = _bbox.radius();

    _center = osg::Vec3d( _bbox.center()[ 0 ], _bbox.center()[ 1 ], _bbox.center()[ 2 ] );
    std::cout << "Center " << _center << std::endl;
    _diagonal = radius * 2.0;

    _coordTransformedBBox->clear();
    _coordTransformedBBox->push_back( osg::Vec4( _bbox.corner( 4 ), 1.0 ) );
    _coordTransformedBBox->push_back( osg::Vec4( _bbox.corner( 5 ), 1.0 ) );
    _coordTransformedBBox->push_back( osg::Vec4( _bbox.corner( 0 ), 1.0 ) );
    _coordTransformedBBox->push_back( osg::Vec4( _bbox.corner( 1 ), 1.0 ) );
    _coordTransformedBBox->push_back( osg::Vec4( _bbox.corner( 6 ), 1.0 ) );
    _coordTransformedBBox->push_back( osg::Vec4( _bbox.corner( 7 ), 1.0 ) );
    _coordTransformedBBox->push_back( osg::Vec4( _bbox.corner( 2 ), 1.0 ) );
    _coordTransformedBBox->push_back( osg::Vec4( _bbox.corner( 3 ), 1.0 ) );

    _dimensions.set( 32, 32, 32 );
    _deltaZ[ 0 ] = _calculateDelta();

    _deltaZ[ 1 ] = _deltaZ[ 0 ];
    _ensureSliceDelta( _extremaIndicies, _deltaZ[ 0 ], _sliceDeltaRatio );
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::SetTextureDimensions(
    unsigned int x, unsigned int y, unsigned int z )
{
    _dimensions.set( x, y, z );
    _deltaZ[ 0 ] = _calculateDelta();
    _deltaZ[ 1 ] = _deltaZ[0];
    _sliceDeltaRatio = 1.0;
}
////////////////////////////////////////////////////////////////////////////////
double TextureBasedVolumeSlices::_calculateDelta() const
{
    double delta = 0;
    if( _dimensions.x() < _dimensions.z() )
    {
        if( _dimensions.z() < _dimensions.y() )
        {
            delta = _diagonal * 1.5 / _dimensions.y();
        }
        else
        {
            delta = _diagonal * 1.5 / _dimensions.z();
        }
    }
    else if( _dimensions.x() < _dimensions.y() )
    {
        delta = _diagonal * 1.5 / _dimensions.y();
    }
    else
    {
        delta = _diagonal * 1.5 / _dimensions.x();
    }

    return delta * .25;
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_ensureSliceDelta(
    unsigned int extremaIndicies[],
    double& delta,
    double& deltaRatio ) const
{
    if( _diagonal && _nSlices )
    {
        double currentDistance =
            ( _coordTransformedBBox->at( extremaIndicies[ 1 ] ) -
              _coordTransformedBBox->at( extremaIndicies[ 0 ] ) ).length();
        delta = ( currentDistance ) / ( double )( _nSlices - 1 );
        deltaRatio = delta / _deltaZ[ 0 ];
        currentDistance = osg::Vec3d( 1.0, 1.0, 1.0 ).length();
        _deltaZ[ 1 ] = ( currentDistance * 2.0 ) / ( double )( _nSlices - 1 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::SetNumberOfSlices( unsigned int numberOfSlices )
{
    _nSlices = ( numberOfSlices < 32 ) ? 32 : numberOfSlices;
    _ensureSliceDelta( _extremaIndicies, _deltaZ[ 1 ], _sliceDeltaRatio );
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::SetRenderMethod( std::string method )
{
    _sliceRenderMethod = method;
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_drawViewAlignedQuadSlices() const
{
    //transform the eye point in
    glMatrixMode( GL_MODELVIEW );

    //push the modelview matrix
    glPushMatrix();
    glLoadIdentity();
    double currentZ = _eyeCenter[ 2 ] - ( 0.5 * _diagonal );
    ///draw the eye space slices
    for( unsigned int z = 0; z < _nSlices; ++z )
    {
        _drawQuadSlice( currentZ );
        currentZ += _deltaZ[ 0 ];
    }
    //pop the modelview matrix
    glPopMatrix();
}
////////////////////////////////////////////////////////////////////////////////
#if ( ( OSG_VERSION_MAJOR >= 1 ) && ( OSG_VERSION_MINOR > 2 ) || ( OSG_VERSION_MAJOR >= 2 ) )
void TextureBasedVolumeSlices::drawImplementation( osg::RenderInfo& renderState ) const
#elif ( ( OSG_VERSION_MAJOR <= 1 ) && ( OSG_VERSION_MINOR <= 2 ) )
void TextureBasedVolumeSlices::drawImplementation( osg::State& renderState ) const
#endif
{
#if ( ( OSG_VERSION_MAJOR >= 1 ) && ( OSG_VERSION_MINOR > 2 ) || ( OSG_VERSION_MAJOR >= 2 ) )
    osg::State& currentState = *( renderState.getState() );
#elif ( ( OSG_VERSION_MAJOR <= 1 ) && ( OSG_VERSION_MINOR <= 2 ) )
    osg::State& currentState = renderState;
#endif
    if( _sliceRenderMethod == "VIEW_ALIGNED_QUADS" )
    {
        ///transform center to current eye space
        _eyeCenter = _center * currentState.getModelViewMatrix();
        _drawViewAlignedQuadSlices();
    }
    else if( _sliceRenderMethod == "VIEW_ALIGNED_POLYGON_INTERSECT" )
    {
        osg::Matrixd modelViewMatrix = currentState.getModelViewMatrix();
        //Calculate the camera position
        osg::Matrixd inverseMV;
        inverseMV.invert( modelViewMatrix );

        ///calculate slice normal in eyespace then transform it back
        _cameraLocation = inverseMV.getTrans();
        _eyeCenter = _center * modelViewMatrix;

        _extremaIndicies[ 0 ] = 0;
        _extremaIndicies[ 1 ] = 7;

        ///transform the bbox into camera space
        osg::ref_ptr< osg::Vec4Array > rotatedBBox = new osg::Vec4Array( 8 );
        rotatedBBox->
            push_back( _coordTransformedBBox->at( 0 ) * modelViewMatrix );
        rotatedBBox->
            push_back( _coordTransformedBBox->at( 1 ) * modelViewMatrix );
        rotatedBBox->
            push_back( _coordTransformedBBox->at( 2 ) * modelViewMatrix );
        rotatedBBox->
            push_back( _coordTransformedBBox->at( 3 ) * modelViewMatrix );
        rotatedBBox->
            push_back( _coordTransformedBBox->at( 4 ) * modelViewMatrix );
        rotatedBBox->
            push_back( _coordTransformedBBox->at( 5 ) * modelViewMatrix );
        rotatedBBox->
            push_back( _coordTransformedBBox->at( 6 ) * modelViewMatrix );
        rotatedBBox->
            push_back( _coordTransformedBBox->at( 7 ) * modelViewMatrix );

        //Screen aligned
        osg::Vec3d vec1( 0.0, 0.0, 1.0 );
        vec1 = vec1 * osg::Matrix::rotate( inverseMV.getRotate() );
        osg::Vec3d slicePlaneNormal = vec1;
        //View aligned
        //osg::Vec3d slicePlaneNormal = _cameraLocation - _center;
        slicePlaneNormal.normalize();
        //std::cout << slicePlaneNormal << std::endl;

        //update the min max indicies for the bbox
        unsigned int extremaIndicies[ 2 ] = { 0, 7 };
        _findBBoxMinMaxIndicies(
            rotatedBBox, slicePlaneNormal, extremaIndicies );

        //update the slice delta
        //initial slicing point
        osg::Vec3d initialSlicePoint(
            _coordTransformedBBox->at( extremaIndicies[ 1 ] ).x(),
            _coordTransformedBBox->at( extremaIndicies[ 1 ] ).y(),
            _coordTransformedBBox->at( extremaIndicies[ 1 ] ).z() );

        double currentDelta = 1.0;
        double deltaRatio = 1.0;
        _ensureSliceDelta( extremaIndicies, currentDelta, deltaRatio );

        //Calcluate edge intersections
        _calculateEdgeIntersections(
            currentState, initialSlicePoint,
            slicePlaneNormal, extremaIndicies,
            currentDelta, deltaRatio );
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_calculateEdgeIntersections(
    osg::State& currentState,
    osg::Vec3d initialSlicePoint,
    osg::Vec3d slicePlaneNormal,
    unsigned int extremaIndicies[],
    double currentDelta,
    double deltaRatio ) const
{
    osg::Vec3d sliceDelta = slicePlaneNormal * currentDelta;
    osg::Vec3d backSlicePoint = initialSlicePoint;
    osg::GL2Extensions* gl2extensions = 
        osg::GL2Extensions::Get( currentState.getContextID(), true );
    int dsLocation = currentState.getUniformLocation( "viewRay" );
    int alphaRatioLocation = currentState.getUniformLocation( "alphaRatio" );

    gl2extensions->glUniform3f( dsLocation, sliceDelta[ 0 ], 
                               sliceDelta[ 1 ], sliceDelta[ 2 ] );
    gl2extensions->glUniform1f( alphaRatioLocation, deltaRatio );

    glDisable( GL_TEXTURE_GEN_S );
    glDisable( GL_TEXTURE_GEN_T );
    glDisable( GL_TEXTURE_GEN_R );

    for( unsigned int i = 0; i <= _nSlices; ++i )
    {
        double verts[18] = {0, 0, 0,
                           0, 0, 0,
                           0, 0, 0,
                           0, 0, 0,
                           0, 0, 0,
                           0, 0, 0};
        double backTcoords[18] = {0, 0, 0,
                                 0, 0, 0,
                                 0, 0, 0,
                                 0, 0, 0,
                                 0, 0, 0,
                                 0, 0, 0};
        double frontTcoords[18] = {0, 0, 0,
                                  0, 0, 0,
                                  0, 0, 0,
                                  0, 0, 0,
                                  0, 0, 0,
                                  0, 0, 0};

        double* tempCoords = 0;
        double lastVert[3] = {0, 0, 0};
        lastVert[0] = initialSlicePoint.x();
        lastVert[1] = initialSlicePoint.y();
        lastVert[2] = initialSlicePoint.z();

        ///calculate vertex position from intersections
        for( unsigned int j = 0; j < 6; ++j )
        {
            verts[j*3 ] = lastVert[0];
            verts[j*3 +1] = lastVert[1];
            verts[j*3 +2] = lastVert[2];
            _calculateVertsAndTextureCoordinates( j,
                                                  initialSlicePoint,
                                                  backSlicePoint,
                                                  slicePlaneNormal,
                                                  extremaIndicies,
                                                  verts,
                                                  frontTcoords, backTcoords );
            lastVert[0] = verts[ j*3     ];
            lastVert[1] = verts[ j*3 + 1];
            lastVert[2] = verts[ j*3 + 2];
            backTcoords[j*3 ] = _deltaZ[1];
            backTcoords[j*3 +1] = _deltaZ[1];
            backTcoords[j*3 +2] = _deltaZ[1];
        }
        //render the polygons...
        glBegin( GL_POLYGON );
        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0, &frontTcoords[ 0 ] );
        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0 + 1, &backTcoords[ 0 ] );
        glVertex3d( verts[0], verts[1], verts[2] );

        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0, &frontTcoords[ 3 ] );
        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0 + 1, &backTcoords[ 3 ] );
        glVertex3d( verts[3], verts[4], verts[5] );

        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0, &frontTcoords[ 6 ] );
        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0 + 1, &backTcoords[ 6 ] );
        glVertex3d( verts[6], verts[7], verts[8] );

        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0, &frontTcoords[ 9 ] );
        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0 + 1, &backTcoords[ 9 ] );
        glVertex3d( verts[9], verts[10], verts[11] );

        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0, &frontTcoords[ 12 ] );
        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0 + 1, &backTcoords[ 12 ] );
        glVertex3d( verts[12], verts[13], verts[14] );

        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0, &frontTcoords[ 15 ] );
        getExtensions( currentState.getContextID(), true )->glMultiTexCoord3dv( GL_TEXTURE0 + 1, &backTcoords[ 15 ] );
        glVertex3d( verts[15], verts[16], verts[17] );
        glEnd();
        backSlicePoint = initialSlicePoint;
        initialSlicePoint += sliceDelta;
    }
    glEnable( GL_TEXTURE_GEN_S );
    glEnable( GL_TEXTURE_GEN_T );
    glEnable( GL_TEXTURE_GEN_R );
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_calculateVertsAndTextureCoordinates(
    unsigned int currentEdgeIndex,
    osg::Vec3d frontSlicePoint,
    osg::Vec3d backSlicePoint,
    osg::Vec3d slicePlaneNormal,
    unsigned int extremaIndicies[],
    double* verts,
    double* frontTCoords,
    double* backTCoords )const
{
    osg::Vec3d edgeInitial;
    osg::Vec3d edgeEnd;
    for( unsigned int i = 0; i < 4; ++i )
    {
        osg::Vec4 tempPoint = _coordTransformedBBox->
            at( edgeSequence[extremaIndicies[0]][edgeIndex[( currentEdgeIndex * 4 ) + i][0]] );
        edgeInitial.set( tempPoint.x(), tempPoint.y(), tempPoint.z() );
        
        tempPoint = _coordTransformedBBox->
            at( edgeSequence[extremaIndicies[0]][edgeIndex[( currentEdgeIndex * 4 ) +
                                                                              i][1]] );
        edgeEnd.set( tempPoint.x(), tempPoint.y(), tempPoint.z() );

        osg::Vec3d frontNumerator = frontSlicePoint - edgeInitial;
        osg::Vec3d backNumerator = backSlicePoint - edgeInitial;
        osg::Vec3d denom = edgeEnd - edgeInitial;

        ///check here for denom equality error--biv
        double tFront = ( denom.length2() != 0 ) ? ( slicePlaneNormal * frontNumerator ) / ( slicePlaneNormal * denom ) : -1;
        double tBack = ( denom.length2() != 0 ) ? ( slicePlaneNormal * backNumerator ) / ( slicePlaneNormal * denom ) : -1;
        if (( tFront >= 0 ) && ( tFront <= 1 ) )
        {
            osg::Vec4 tInitial =  _tcoordBBox->at( edgeSequence[extremaIndicies[0]][edgeIndex[( currentEdgeIndex * 4 ) +
                                                   i][0]] );
            osg::Vec4 tEnd = _tcoordBBox->at( edgeSequence[extremaIndicies[0]][edgeIndex[( currentEdgeIndex * 4 ) +
                                              i][1]] );
            // Compute the line intersection
            double x = ( edgeInitial.x() + ( tFront * ( edgeEnd.x() - edgeInitial.x() ) ) );
            double y = ( edgeInitial.y() + ( tFront * ( edgeEnd.y() - edgeInitial.y() ) ) );
            double z = ( edgeInitial.z() + ( tFront * ( edgeEnd.z() - edgeInitial.z() ) ) );
            verts[currentEdgeIndex*3    ] = x;
            verts[currentEdgeIndex*3 + 1] = y;
            verts[currentEdgeIndex*3 + 2] = z;
            // Compute the front texture coords
            x = ( double )( tInitial.x() + ( tFront * ( tEnd.x() - tInitial.x() ) ) );
            y = ( double )( tInitial.y() + ( tFront * ( tEnd.y() - tInitial.y() ) ) );
            z = ( double )( tInitial.z() + ( tFront * ( tEnd.z() - tInitial.z() ) ) );

            frontTCoords[currentEdgeIndex*3    ] = x;
            frontTCoords[currentEdgeIndex*3 + 1] = y;
            frontTCoords[currentEdgeIndex*3 + 2] = z;

            if (( tBack >= 0 ) && ( tBack <= 1 ) )
            {
                // Compute the back texture coords
                x = ( double )( tInitial.x() + ( tBack * ( tEnd.x() - tInitial.x() ) ) );
                y = ( double )( tInitial.y() + ( tBack * ( tEnd.y() - tInitial.y() ) ) );
                z = ( double )( tInitial.z() + ( tBack * ( tEnd.z() - tInitial.z() ) ) );
            }
            backTCoords[currentEdgeIndex*3    ] = x;
            backTCoords[currentEdgeIndex*3 + 1] = y;
            backTCoords[currentEdgeIndex*3 + 2] = z;

            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
double TextureBasedVolumeSlices::_calculateSampleDistance(
    osg::Matrixd iModelView ) const
{
    /*Patrick O'Leary
       Calculate the the minimum and maximum x-, y- or z-position of the
          rotated bounding box. Equivalent to but quicker than:
          maximumD = (maximum, 0, 0), (0, maximum, 0) or (0, 0, maximum);
          minimumD = (minimum, 0, 0), (0, minimum, 0) or (0, 0, minimum);
          maximumV = modelviewInverse * maximumD;
          minimumV = modelviewInverse * minimumD;
        */
    osg::Vec4d maximumV;// = iModelView* _coordTransformedBBox->at(_extremaIndicies[1]);
    osg::Vec4d minimumV;//= iModelView* _coordTransformedBBox->at(_extremaIndicies[0]);;

    maximumV.set( _rotatedBBox->at( _extremaIndicies[1] ).x(),
                  _rotatedBBox->at( _extremaIndicies[1] ).y(),
                  _rotatedBBox->at( _extremaIndicies[1] ).z(), 0 );
    minimumV.set( _rotatedBBox->at( _extremaIndicies[0] ).x(),
                  _rotatedBBox->at( _extremaIndicies[0] ).y(),
                  _rotatedBBox->at( _extremaIndicies[0] ).z(), 0 );

    maximumV -= minimumV;

    return maximumV.length();
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_findBBoxMinMaxIndicies(
    osg::ref_ptr< osg::Vec4Array > rotatedBBox,
    osg::Vec3d slicePlaneNormal,
    unsigned int extremeIndicies[] ) const

{
    //this calculation is done in object coordinates
    double extremes[2] = {0, 0};
    double poleMultiplier = 200.0;
    osg::Vec3d tempCenter( _center.x(), _center.y(), _center.z() );
    osg::Vec3d pole = tempCenter + slicePlaneNormal * _diagonal;
    osg::Vec4d tempbbPoint = rotatedBBox->at( 0 );
    osg::Vec3d bbPoint( tempbbPoint.x(), tempbbPoint.y(), tempbbPoint.z() );
    double diff = ( pole - bbPoint ).length();


    extremes[0]  = diff;
    extremes[1]  = -diff;
    //std::cout<<"========================================="<<std::endl;
    //std::cout<<"Pole:"<<std::endl;
    //std::cout<<" "<<pole.x()<<" "<<pole.y()<<" "<<pole.z()<<std::endl;
    osg::Vec4d tempTransformBBox;
    osg::Vec3d transformBBox;
    for( unsigned int i = 0; i < 8; ++i )
    {
        tempTransformBBox = _coordTransformedBBox->at( i );
        transformBBox.set( tempTransformBBox.x(), tempTransformBBox.y(), tempTransformBBox.z() );
        diff = ( pole - transformBBox ).length();

        ///closest corner
        if( diff < extremes[0] )
        {
            extremes[0] = diff;
            extremeIndicies[0] = i;
        }
        ///furthest corner
        if( diff > extremes[1] )
        {
            extremes[1] = diff;
            extremeIndicies[1] = i;
        }
    }
    //std::cout<<"BBox Extrema"<<std::endl;
    //std::cout<<"(min,max)"<<std::endl;
    //std::cout<<_extremaIndicies[0]<<","<<_extremaIndicies[1]<<std::endl;

    //std::cout<<"Extrema coords:"<<std::endl;
    //std::cout<<_coordTransformedBBox->at(_extremaIndicies[0]).x()<<","<<_coordTransformedBBox->at(_extremaIndicies[0]).y()<<","<<_coordTransformedBBox->at(_extremaIndicies[0]).z()<<std::endl;
    //std::cout<<_coordTransformedBBox->at(_extremaIndicies[1]).x()<<","<<_coordTransformedBBox->at(_extremaIndicies[1]).y()<<","<<_coordTransformedBBox->at(_extremaIndicies[1]).z()<<std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVolumeSlices::_drawQuadSlice( double zPosition ) const
{
    glBegin( GL_QUADS );
    glColor4d( 0.0, 0.0, 0.0, 1.0 );
    glVertex3d(
        _eyeCenter.x() - 0.5 * _diagonal,
        _eyeCenter.y() - 0.5 * _diagonal, zPosition );
    glVertex3d(
        _eyeCenter.x() + 0.5 * _diagonal,
        _eyeCenter.y() - 0.5 * _diagonal, zPosition );
    glVertex3d(
        _eyeCenter.x() + 0.5 * _diagonal,
        _eyeCenter.y() + 0.5 * _diagonal, zPosition );
    glVertex3d(
        _eyeCenter.x() - 0.5 * _diagonal,
        _eyeCenter.y() + 0.5 * _diagonal, zPosition );
    glEnd();
}
////////////////////////////////////////////////////////////////////////////////
osg::BoundingBox TextureBasedVolumeSlices::computeBound() const
{
    return _bbox;
}
////////////////////////////////////////////////////////////////////////////////
