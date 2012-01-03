/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

// --- C/C++ Libraries --- //
#include <iostream>

// --- VR Juggler Includes --- //
#include <gmtl/Generate.h>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
GLTransformInfo::GLTransformInfo(
    const int& viewportOriginX, const int& viewportOriginY,
    const int& viewportWidth, const int& viewportHeight,
    const int& windowOriginX, const int& windowOriginY,
    const int& windowWidth, const int& windowHeight,
    const gmtl::Matrix44d& windowMatrix, const bool inStereo )
    :
    m_viewportOriginX( viewportOriginX ),
    m_viewportOriginY( viewportOriginY ),
    m_viewportWidth( viewportWidth ),
    m_viewportHeight( viewportHeight ),

    m_windowOriginX( windowOriginX ),
    m_windowOriginY( windowOriginY ),
    m_windowWidth( windowWidth ),
    m_windowHeight( windowHeight ),

    m_leftFrustum( 0.0 ),
    m_rightFrustum( 0.0 ),
    m_bottomFrustum( 0.0 ),
    m_topFrustum( 0.0 ),
    m_nearFrustum( 0.0 ),
    m_farFrustum( 0.0 ),
    m_fovz( 0.0 ),
    
    m_windowMatrix( windowMatrix ),
    m_windowMatrixOSG( m_windowMatrix.mData ),
    
    m_inStereo( inStereo )
{
    m_vrjViewMatrix.mState = gmtl::Matrix44d::FULL;
    m_cameraMatrix.mState = gmtl::Matrix44d::FULL;
    m_viewMatrix.mState = gmtl::Matrix44d::FULL;
    m_projectionMatrix.mState = gmtl::Matrix44d::FULL;
    m_projectionMatrix.mData[ 11 ] = -1.0;
    m_projectionMatrix.mData[ 15 ] =  0.0;
}
////////////////////////////////////////////////////////////////////////////////
GLTransformInfo::GLTransformInfo( const GLTransformInfo& glTransformInfo )
    :
    m_viewportOriginX( glTransformInfo.m_viewportOriginX ),
    m_viewportOriginY( glTransformInfo.m_viewportOriginY ),
    m_viewportWidth( glTransformInfo.m_viewportWidth ),
    m_viewportHeight( glTransformInfo.m_viewportHeight ),

    m_windowOriginX( glTransformInfo.m_windowOriginX ),
    m_windowOriginY( glTransformInfo.m_windowOriginY ),
    m_windowWidth( glTransformInfo.m_windowWidth ),
    m_windowHeight( glTransformInfo.m_windowHeight ),

    m_leftFrustum( glTransformInfo.m_leftFrustum ),
    m_rightFrustum( glTransformInfo.m_rightFrustum ),
    m_bottomFrustum( glTransformInfo.m_bottomFrustum ),
    m_topFrustum( glTransformInfo.m_topFrustum ),
    m_nearFrustum( glTransformInfo.m_nearFrustum ),
    m_farFrustum( glTransformInfo.m_farFrustum ),
    m_fovz( glTransformInfo.m_fovz ),

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
const int& GLTransformInfo::GetViewportOriginX() const
{
    return m_viewportOriginX;
}
////////////////////////////////////////////////////////////////////////////////
const int& GLTransformInfo::GetViewportOriginY() const
{
    return m_viewportOriginY;
}
////////////////////////////////////////////////////////////////////////////////
const int& GLTransformInfo::GetViewportWidth() const
{
    return m_viewportWidth;
}
////////////////////////////////////////////////////////////////////////////////
const int& GLTransformInfo::GetViewportHeight() const
{
    return m_viewportHeight;
}
////////////////////////////////////////////////////////////////////////////////
const int& GLTransformInfo::GetWindowOriginX() const
{
    return m_windowOriginX;
}
////////////////////////////////////////////////////////////////////////////////
const int& GLTransformInfo::GetWindowOriginY() const
{
    return m_windowOriginY;
}
////////////////////////////////////////////////////////////////////////////////
const int& GLTransformInfo::GetWindowWidth() const
{
    return m_windowWidth;
}
////////////////////////////////////////////////////////////////////////////////
const int& GLTransformInfo::GetWindowHeight() const
{
    return m_windowHeight;
}
////////////////////////////////////////////////////////////////////////////////
const double& GLTransformInfo::GetLeftFrustum() const
{
    return m_leftFrustum;
}
////////////////////////////////////////////////////////////////////////////////
const double& GLTransformInfo::GetRightFrustum() const
{
    return m_rightFrustum;
}
////////////////////////////////////////////////////////////////////////////////
const double& GLTransformInfo::GetBottomFrustum() const
{
    return m_bottomFrustum;
}
////////////////////////////////////////////////////////////////////////////////
const double& GLTransformInfo::GetTopFrustum() const
{
    return m_topFrustum;
}
////////////////////////////////////////////////////////////////////////////////
const double& GLTransformInfo::GetNearFrustum() const
{
    return m_nearFrustum;
}
////////////////////////////////////////////////////////////////////////////////
const double& GLTransformInfo::GetFarFrustum() const
{
    return m_farFrustum;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetVrjViewMatrix() const
{
    return m_vrjViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& GLTransformInfo::GetVrjViewMatrixOSG() const
{
    return m_vrjViewMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetVrjCenterViewMatrix() const
{
    return m_vrjCenterViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& GLTransformInfo::GetVrjCenterViewMatrixOSG() const
{
    return m_vrjCenterViewMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetCameraMatrix() const
{
    return m_cameraMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& GLTransformInfo::GetCameraMatrixOSG() const
{
    return m_cameraMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetViewMatrix() const
{
    return m_viewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& GLTransformInfo::GetViewMatrixOSG() const
{
    return m_viewMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetCenterViewMatrix() const
{
    return m_centerViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& GLTransformInfo::GetCenterViewMatrixOSG() const
{
    return m_centerViewMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetProjectionMatrix() const
{
    return m_projectionMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& GLTransformInfo::GetProjectionMatrixOSG() const
{
    return m_projectionMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetWindowMatrix() const
{
    return m_windowMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& GLTransformInfo::GetWindowMatrixOSG() const
{
    return m_windowMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d GLTransformInfo::GetVPWMatrix() const
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
const osg::Matrixd GLTransformInfo::GetVPWMatrixOSG() const
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
const gmtl::Matrix44d GLTransformInfo::GetCenterVPWMatrix() const
{
    return m_windowMatrix * m_projectionMatrix * m_centerViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd GLTransformInfo::GetCenterVPWMatrixOSG() const
{
    return m_centerViewMatrixOSG * m_projectionMatrixOSG * m_windowMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
void GLTransformInfo::UpdateFrustumValues(
    const double& l, const double& r,
    const double& b, const double& t,
    const double& n, const double& f )
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
    const gmtl::Matrix44d& vrjViewMatrix,
    const gmtl::Matrix44d& cameraMatrix )
{
    m_vrjViewMatrix = vrjViewMatrix;
    m_vrjViewMatrixOSG.set( m_vrjViewMatrix.mData );
    m_cameraMatrix = cameraMatrix;
    m_cameraMatrixOSG.set( m_cameraMatrix.mData );
    m_viewMatrix = m_vrjViewMatrix * m_cameraMatrix;
    m_viewMatrixOSG.set( m_viewMatrix.mData );
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
void GLTransformInfo::UpdateCenterViewMatrix( const gmtl::Matrix44d& vrjViewMatrix )
{
    m_vrjCenterViewMatrix = vrjViewMatrix;
    m_vrjCenterViewMatrixOSG.set( m_vrjCenterViewMatrix.mData );
    
    m_centerViewMatrix = m_vrjCenterViewMatrix * m_cameraMatrix;
    m_centerViewMatrixOSG.set( m_centerViewMatrix.mData );
}
////////////////////////////////////////////////////////////////////////////////
const double& GLTransformInfo::GetFOVZ()
{
    double topAngle = atan( m_topFrustum / m_nearFrustum );
    double tempDiv = fabs( m_bottomFrustum ) / m_nearFrustum;
    double bottomAngle = atan( tempDiv );
    
    m_fovz = topAngle + bottomAngle;
    return m_fovz;
}
////////////////////////////////////////////////////////////////////////////////
