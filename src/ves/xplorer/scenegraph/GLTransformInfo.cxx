/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

// --- VR Juggler Includes --- //
#include <gmtl/Generate.h>

// --- BackdropFX Includes --- //
#include <backdropFX/RTTViewport.h>

// --- STL Includes --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
GLTransformInfo::GLTransformInfo( bool const& inStereo )
    :
    m_inStereo( inStereo ),

    m_viewportOriginX( 0 ),
    m_viewportOriginY( 0 ),
    m_viewportWidth( 0 ),
    m_viewportHeight( 0 ),
    m_bdfxRTTViewport( new backdropFX::RTTViewport( 0, 0, 0, 0 ) ),

    //m_windowOriginX( windowOriginX ),
    //m_windowOriginY( windowOriginY ),
    m_windowWidth( 0 ),
    m_windowHeight( 0 ),

    m_leftFrustum( 0.0 ),
    m_rightFrustum( 0.0 ),
    m_bottomFrustum( 0.0 ),
    m_topFrustum( 0.0 ),
    m_nearFrustum( 0.0 ),
    m_farFrustum( 0.0 ),
    m_fovz( 0.0 ),
    m_aspectRatio( 0.0 )
{
    m_vrjViewMatrix.mState = gmtl::Matrix44d::FULL;
    m_cameraMatrix.mState = gmtl::Matrix44d::FULL;
    m_viewMatrix.mState = gmtl::Matrix44d::FULL;
    m_projectionMatrix.mState = gmtl::Matrix44d::FULL;
    m_windowMatrix.mState = gmtl::Matrix44d::FULL;

    m_projectionMatrix.mData[ 11 ] = -1.0;
    m_projectionMatrix.mData[ 15 ] =  0.0;

    m_windowMatrix.mData[ 10 ] = 0.5;
    m_windowMatrix.mData[ 14 ] = 0.5;
}
////////////////////////////////////////////////////////////////////////////////
GLTransformInfo::GLTransformInfo( GLTransformInfo const& glTransformInfo )
    :
    m_inStereo( glTransformInfo.m_inStereo ),

    m_viewportOriginX( glTransformInfo.m_viewportOriginX ),
    m_viewportOriginY( glTransformInfo.m_viewportOriginY ),
    m_viewportWidth( glTransformInfo.m_viewportWidth ),
    m_viewportHeight( glTransformInfo.m_viewportHeight ),
    m_bdfxRTTViewport( glTransformInfo.m_bdfxRTTViewport.get() ),

    //m_windowOriginX( glTransformInfo.m_windowOriginX ),
    //m_windowOriginY( glTransformInfo.m_windowOriginY ),
    m_windowWidth( glTransformInfo.m_windowWidth ),
    m_windowHeight( glTransformInfo.m_windowHeight ),

    m_leftFrustum( glTransformInfo.m_leftFrustum ),
    m_rightFrustum( glTransformInfo.m_rightFrustum ),
    m_bottomFrustum( glTransformInfo.m_bottomFrustum ),
    m_topFrustum( glTransformInfo.m_topFrustum ),
    m_nearFrustum( glTransformInfo.m_nearFrustum ),
    m_farFrustum( glTransformInfo.m_farFrustum ),
    m_fovz( glTransformInfo.m_fovz ),
    m_aspectRatio( glTransformInfo.m_aspectRatio ),

    m_vrjViewMatrix( glTransformInfo.m_vrjViewMatrix ),
    m_vrjViewMatrixOSG( glTransformInfo.m_vrjViewMatrixOSG ),
    m_cameraMatrix( glTransformInfo.m_cameraMatrix ),
    m_cameraMatrixOSG( glTransformInfo.m_cameraMatrixOSG ),
    m_viewMatrix( glTransformInfo.m_viewMatrix ),
    m_viewMatrixOSG( glTransformInfo.m_viewMatrixOSG ),
    m_projectionMatrix( glTransformInfo.m_projectionMatrix ),
    m_projectionMatrixOSG( glTransformInfo.m_projectionMatrixOSG ),
    m_windowMatrix( glTransformInfo.m_windowMatrix ),
    m_windowMatrixOSG( glTransformInfo.m_windowMatrixOSG )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
GLTransformInfo::~GLTransformInfo()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
int const& GLTransformInfo::GetViewportOriginX() const
{
    return m_viewportOriginX;
}
////////////////////////////////////////////////////////////////////////////////
int const& GLTransformInfo::GetViewportOriginY() const
{
    return m_viewportOriginY;
}
////////////////////////////////////////////////////////////////////////////////
int const& GLTransformInfo::GetViewportWidth() const
{
    return m_viewportWidth;
}
////////////////////////////////////////////////////////////////////////////////
int const& GLTransformInfo::GetViewportHeight() const
{
    return m_viewportHeight;
}
////////////////////////////////////////////////////////////////////////////////
backdropFX::RTTViewport& GLTransformInfo::GetBdfxRTTViewport() const
{
    return *( m_bdfxRTTViewport.get() );
}
////////////////////////////////////////////////////////////////////////////////
/*
int const& GLTransformInfo::GetWindowOriginX() const
{
    return m_windowOriginX;
}
////////////////////////////////////////////////////////////////////////////////
int const& GLTransformInfo::GetWindowOriginY() const
{
    return m_windowOriginY;
}
*/
////////////////////////////////////////////////////////////////////////////////
int const& GLTransformInfo::GetWindowWidth() const
{
    return m_windowWidth;
}
////////////////////////////////////////////////////////////////////////////////
int const& GLTransformInfo::GetWindowHeight() const
{
    return m_windowHeight;
}
////////////////////////////////////////////////////////////////////////////////
double const& GLTransformInfo::GetLeftFrustum() const
{
    return m_leftFrustum;
}
////////////////////////////////////////////////////////////////////////////////
double const& GLTransformInfo::GetRightFrustum() const
{
    return m_rightFrustum;
}
////////////////////////////////////////////////////////////////////////////////
double const& GLTransformInfo::GetBottomFrustum() const
{
    return m_bottomFrustum;
}
////////////////////////////////////////////////////////////////////////////////
double const& GLTransformInfo::GetTopFrustum() const
{
    return m_topFrustum;
}
////////////////////////////////////////////////////////////////////////////////
double const& GLTransformInfo::GetNearFrustum() const
{
    return m_nearFrustum;
}
////////////////////////////////////////////////////////////////////////////////
double const& GLTransformInfo::GetFarFrustum() const
{
    return m_farFrustum;
}
////////////////////////////////////////////////////////////////////////////////
double const& GLTransformInfo::GetFOVZ()
{
    double topAngle = atan( m_topFrustum / m_nearFrustum );
    double tempDiv = fabs( m_bottomFrustum ) / m_nearFrustum;
    double bottomAngle = atan( tempDiv );

    m_fovz = topAngle + bottomAngle;
    return m_fovz;
}
////////////////////////////////////////////////////////////////////////////////
double const& GLTransformInfo::GetAspectRatio() const
{
    return m_aspectRatio;
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const& GLTransformInfo::GetVrjViewMatrix() const
{
    return m_vrjViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const& GLTransformInfo::GetVrjViewMatrixOSG() const
{
    return m_vrjViewMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const& GLTransformInfo::GetVrjCenterViewMatrix() const
{
    return m_vrjCenterViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const& GLTransformInfo::GetVrjCenterViewMatrixOSG() const
{
    return m_vrjCenterViewMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const& GLTransformInfo::GetCameraMatrix() const
{
    return m_cameraMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const& GLTransformInfo::GetCameraMatrixOSG() const
{
    return m_cameraMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const& GLTransformInfo::GetViewMatrix() const
{
    return m_viewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const& GLTransformInfo::GetViewMatrixOSG() const
{
    return m_viewMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const& GLTransformInfo::GetCenterViewMatrix() const
{
    return m_centerViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const& GLTransformInfo::GetCenterViewMatrixOSG() const
{
    return m_centerViewMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const& GLTransformInfo::GetProjectionMatrix() const
{
    return m_projectionMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const& GLTransformInfo::GetProjectionMatrixOSG() const
{
    return m_projectionMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const& GLTransformInfo::GetWindowMatrix() const
{
    return m_windowMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const& GLTransformInfo::GetWindowMatrixOSG() const
{
    return m_windowMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const GLTransformInfo::GetVPWMatrix() const
{
    if( m_inStereo )
    {
        return GetCenterVPWMatrix();
    }
    else
    {
        return m_windowMatrix * m_projectionMatrix * m_viewMatrix;
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const GLTransformInfo::GetVPWMatrixOSG() const
{
    if( m_inStereo )
    {
        return GetCenterVPWMatrixOSG();
    }
    else
    {
        return m_viewMatrixOSG * m_projectionMatrixOSG * m_windowMatrixOSG;
    }
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const GLTransformInfo::GetCenterVPWMatrix() const
{
    return m_windowMatrix * m_projectionMatrix * m_centerViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const GLTransformInfo::GetCenterVPWMatrixOSG() const
{
    return m_centerViewMatrixOSG * m_projectionMatrixOSG * m_windowMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
void GLTransformInfo::UpdateFrustumValues(
    double const& l, double const& r,
    double const& b, double const& t,
    double const& n, double const& f )
{
    m_leftFrustum = l;
    m_rightFrustum = r;
    m_bottomFrustum = b;
    m_topFrustum = t;
    m_nearFrustum = n;
    m_farFrustum = f;

    UpdateProjectionMatrix();
}
////////////////////////////////////////////////////////////////////////////////
void GLTransformInfo::UpdateViewMatrix(
    gmtl::Matrix44d const& vrjViewMatrix,
    gmtl::Matrix44d const& cameraMatrix )
{
    m_vrjViewMatrix = vrjViewMatrix;
    m_vrjViewMatrixOSG.set( m_vrjViewMatrix.mData );
    m_cameraMatrix = cameraMatrix;
    m_cameraMatrixOSG.set( m_cameraMatrix.mData );
    m_viewMatrix = m_vrjViewMatrix * m_cameraMatrix;
    m_viewMatrixOSG.set( m_viewMatrix.mData );
}
////////////////////////////////////////////////////////////////////////////////
void  GLTransformInfo::UpdateCenterViewMatrix(
    gmtl::Matrix44d const& vrjViewMatrix )
{
    m_vrjCenterViewMatrix = vrjViewMatrix;
    m_vrjCenterViewMatrixOSG.set( m_vrjCenterViewMatrix.mData );

    m_centerViewMatrix = m_vrjCenterViewMatrix * m_cameraMatrix;
    m_centerViewMatrixOSG.set( m_centerViewMatrix.mData );
}
////////////////////////////////////////////////////////////////////////////////
void GLTransformInfo::UpdateViewportValues(
    int const& viewportOriginX,
    int const& viewportOriginY,
    int const& viewportWidth,
    int const& viewportHeight )
{
    m_viewportOriginX = viewportOriginX;
    m_viewportOriginY = viewportOriginY;
    m_viewportWidth = viewportWidth;
    m_viewportHeight = viewportHeight;
    m_bdfxRTTViewport->setViewport(
        m_viewportOriginX, m_viewportOriginY,
        m_viewportWidth, m_viewportHeight );

    UpdateWindowMatrix();
}
////////////////////////////////////////////////////////////////////////////////
void GLTransformInfo::UpdateWindowValues(
    int const& windowWidth,
    int const& windowHeight )
{
    m_windowWidth = windowWidth;
    m_windowHeight = windowHeight;

    m_aspectRatio =
        static_cast< double >( m_windowWidth ) /
        static_cast< double >( m_windowHeight );
}
////////////////////////////////////////////////////////////////////////////////
void GLTransformInfo::UpdateProjectionMatrix()
{
    m_projectionMatrix.mData[  0 ] =
        ( 2.0 * m_nearFrustum ) /
        ( m_rightFrustum - m_leftFrustum );
    m_projectionMatrix.mData[  5 ] =
        ( 2.0 * m_nearFrustum ) /
        ( m_topFrustum - m_bottomFrustum );
    m_projectionMatrix.mData[  8 ] =
        ( m_rightFrustum + m_leftFrustum ) /
        ( m_rightFrustum - m_leftFrustum );
    m_projectionMatrix.mData[  9 ] =
        ( m_topFrustum + m_bottomFrustum ) /
        ( m_topFrustum - m_bottomFrustum );
    m_projectionMatrix.mData[ 10 ] =
        -1.0 * ( m_farFrustum + m_nearFrustum ) /
               ( m_farFrustum - m_nearFrustum );
    m_projectionMatrix.mData[ 14 ] =
        ( -2.0 * m_farFrustum * m_nearFrustum ) /
               ( m_farFrustum - m_nearFrustum );

    m_projectionMatrixOSG.set( m_projectionMatrix.mData );
}
////////////////////////////////////////////////////////////////////////////////
void GLTransformInfo::UpdateWindowMatrix()
{
    m_windowMatrix.mData[  0 ] = 0.5 * m_viewportWidth;
    m_windowMatrix.mData[  5 ] = 0.5 * m_viewportHeight;
    m_windowMatrix.mData[ 12 ] = m_windowMatrix.mData[ 0 ] + m_viewportOriginX;
    m_windowMatrix.mData[ 13 ] = m_windowMatrix.mData[ 5 ] + m_viewportOriginY;

    m_windowMatrixOSG.set( m_windowMatrix.mData );
}
////////////////////////////////////////////////////////////////////////////////
