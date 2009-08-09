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
    const gmtl::Matrix44d& windowMatrix )
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

    m_windowMatrix( windowMatrix ),
    m_osgWindowMatrix( m_windowMatrix.mData )
{
    m_modelViewMatrix.mState = gmtl::Matrix44d::FULL;
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

    m_modelViewMatrix( glTransformInfo.m_modelViewMatrix ),
    m_projectionMatrix( glTransformInfo.m_projectionMatrix ),
    m_windowMatrix( glTransformInfo.m_windowMatrix ),
    m_osgWindowMatrix( glTransformInfo.m_osgWindowMatrix )
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
const gmtl::Matrix44d& GLTransformInfo::GetModelViewMatrix() const
{
    return m_modelViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& GLTransformInfo::GetOSGModelViewMatrix() const
{
    return m_osgModelViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetProjectionMatrix() const
{
    return m_projectionMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& GLTransformInfo::GetOSGProjectionMatrix() const
{
    return m_osgProjectionMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetWindowMatrix() const
{
    return m_windowMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& GLTransformInfo::GetOSGWindowMatrix() const
{
    return m_osgWindowMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d GLTransformInfo::GetMVPWMatrix() const
{
    return m_windowMatrix * m_projectionMatrix * m_modelViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd GLTransformInfo::GetOSGMVPWMatrix() const
{
    return m_osgModelViewMatrix * m_osgProjectionMatrix * m_osgWindowMatrix;
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
void GLTransformInfo::UpdateModelViewMatrix(
    const gmtl::Matrix44d& modelViewMatrix )
{
    m_modelViewMatrix = modelViewMatrix;
    m_osgModelViewMatrix.set( m_modelViewMatrix.mData );
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

    m_osgProjectionMatrix.set( m_projectionMatrix.mData );
}
////////////////////////////////////////////////////////////////////////////////
