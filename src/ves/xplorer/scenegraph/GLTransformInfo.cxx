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

// --- OSG Includes --- //

// --- vrJuggler Includes --- //
#include <gmtl/Vec.h>
#include <gmtl/Generate.h>
#include <gmtl/AxisAngle.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
GLTransformInfo::GLTransformInfo(
    const unsigned int& viewportOriginX, const unsigned int& viewportOriginY,
    const unsigned int& viewportWidth, const unsigned int& viewportHeight,
    const gmtl::Matrix44d& windowMatrix )
    :
    m_viewportOriginX( viewportOriginX ),
    m_viewportOriginY( viewportOriginY ),
    m_viewportWidth( viewportWidth ),
    m_viewportHeight( viewportHeight ),

    m_leftFrustum( 0.0 ),
    m_rightFrustum( 0.0 ),
    m_bottomFrustum( 0.0 ),
    m_topFrustum( 0.0 ),
    m_nearFrustum( 0.0 ),
    m_farFrustum( 0.0 ),

    m_windowMatrix( windowMatrix )
{
    m_modelViewMatrix.mState =
        gmtl::Matrix44d::AFFINE | gmtl::Matrix44d::NON_UNISCALE;
    m_projectionMatrix.mState =
        gmtl::Matrix44d::AFFINE | gmtl::Matrix44d::NON_UNISCALE;
}
////////////////////////////////////////////////////////////////////////////////
GLTransformInfo::GLTransformInfo( const GLTransformInfo& glTransformInfo )
    :
    m_viewportOriginX( glTransformInfo.m_viewportOriginX ),
    m_viewportOriginY( glTransformInfo.m_viewportOriginY ),
    m_viewportWidth( glTransformInfo.m_viewportWidth ),
    m_viewportHeight( glTransformInfo.m_viewportHeight ),

    m_leftFrustum( glTransformInfo.m_leftFrustum ),
    m_rightFrustum( glTransformInfo.m_rightFrustum ),
    m_bottomFrustum( glTransformInfo.m_bottomFrustum ),
    m_topFrustum( glTransformInfo.m_topFrustum ),
    m_nearFrustum( glTransformInfo.m_nearFrustum ),
    m_farFrustum( glTransformInfo.m_farFrustum ),

    m_modelViewMatrix( glTransformInfo.m_modelViewMatrix ),
    m_projectionMatrix( glTransformInfo.m_projectionMatrix ),
    m_windowMatrix( glTransformInfo.m_windowMatrix )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
GLTransformInfo::~GLTransformInfo()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
const unsigned int& GLTransformInfo::GetViewportOriginX() const
{
    return m_viewportOriginX;
}
////////////////////////////////////////////////////////////////////////////////
const unsigned int& GLTransformInfo::GetViewportOriginY() const
{
    return m_viewportOriginY;
}
////////////////////////////////////////////////////////////////////////////////
const unsigned int& GLTransformInfo::GetViewportWidth() const
{
    return m_viewportWidth;
}
////////////////////////////////////////////////////////////////////////////////
const unsigned int& GLTransformInfo::GetViewportHeight() const
{
    return m_viewportHeight;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetModelViewMatrix() const
{
    return m_modelViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetProjectionMatrix() const
{
    return m_projectionMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& GLTransformInfo::GetWindowMatrix() const
{
    return m_windowMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d GLTransformInfo::GetMVPWMatrix() const
{
    return m_windowMatrix * m_projectionMatrix * m_modelViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
