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

#ifndef GL_TRANSFORM_INFO_H
#define GL_TRANSFORM_INFO_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include "GLTransformInfoPtr.h"

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Matrixd>

// --- VR Juggler Includes --- //
#include <gmtl/Matrix.h>

// --- C/C++ Libraries --- //
#include <string>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

/*!\file GLTransformInfo.h
 */

/*!\class ves::xplorer::scenegraph::GLTransformInfo
 *
 */

/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS GLTransformInfo
{
public:
    ///Constructor
    GLTransformInfo(
        const int& viewportOriginX, const int& viewportOriginY,
        const int& viewportWidth, const int& viewportHeight,
        const int& windowOriginX, const int& windowOriginY,
        const int& windowWidth, const int& windowHeight,
        const gmtl::Matrix44d& windowMatrix );

    ///Copy Constructor
    GLTransformInfo( const GLTransformInfo& glTransformInfo );

    ///Destructor
    ~GLTransformInfo();

    ///
    const int& GetViewportOriginX() const;

    ///
    const int& GetViewportOriginY() const;

    ///
    const int& GetViewportWidth() const;

    ///
    const int& GetViewportHeight() const;

    ///
    const int& GetWindowOriginX() const;

    ///
    const int& GetWindowOriginY() const;

    ///
    const int& GetWindowWidth() const;

    ///
    const int& GetWindowHeight() const;

    ///
    const double& GetLeftFrustum() const;

    ///
    const double& GetRightFrustum() const;

    ///
    const double& GetBottomFrustum() const;

    ///
    const double& GetTopFrustum() const;

    ///
    const double& GetNearFrustum() const;

    ///
    const double& GetFarFrustum() const;

    ///
    const gmtl::Matrix44d& GetModelViewMatrix() const;

    ///
    const osg::Matrixd& GetOSGModelViewMatrix() const;

    ///
    const gmtl::Matrix44d& GetProjectionMatrix() const;

    ///
    const osg::Matrixd& GetOSGProjectionMatrix() const;

    ///
    const gmtl::Matrix44d& GetWindowMatrix() const;

    ///
    const osg::Matrixd& GetOSGWindowMatrix() const;

    ///
    const gmtl::Matrix44d GetMVPWMatrix() const;

    ///
    const osg::Matrixd GetOSGMVPWMatrix() const;

    ///
    void UpdateFrustumValues(
        const double& l, const double& r,
        const double& b, const double& t,
        const double& n, const double& f );

    ///
    void UpdateModelViewMatrix( const gmtl::Matrix44d& modelViewMatrix );

protected:

private:
    ///
    void UpdateProjectionMatrix();

    ///
    const int m_viewportOriginX;
    ///
    const int m_viewportOriginY;
    ///
    const int m_viewportWidth;
    ///
    const int m_viewportHeight;

    ///
    const int m_windowOriginX;
    ///
    const int m_windowOriginY;
    ///
    const int m_windowWidth;
    ///
    const int m_windowHeight;

    ///
    double m_leftFrustum;
    ///
    double m_rightFrustum;
    ///
    double m_bottomFrustum;
    ///
    double m_topFrustum;
    ///
    double m_nearFrustum;
    ///
    double m_farFrustum;

    ///
    gmtl::Matrix44d m_modelViewMatrix;

    ///
    osg::Matrixd m_osgModelViewMatrix;

    ///
    gmtl::Matrix44d m_projectionMatrix;

    ///
    osg::Matrixd m_osgProjectionMatrix;

    ///
    const gmtl::Matrix44d m_windowMatrix;

    ///
    const osg::Matrixd m_osgWindowMatrix;

};
} //end scenegraph
} //end xplorer
} //end ves

#endif // end GL_TRANSFORM_INFO_H
