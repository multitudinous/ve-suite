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
 * 1) Object Coordinates - [ vertex ]
 * 2) World Coordinates - [ vertex ] [ M ]
 * 3) Eye Coordinates - [ vertex ] [ M ] [ V ]
 * 4) Clip Coordinates - [ vertex ] [ M ] [ V ] [ P ]
 * 5) Normalized Device Coordinates - Perspective Division
 *    Perspective division performed on the Clip Coordinates produces
 *    Normalized Device Coordinates, ranging from -1 to 1 in all three axes
 * 6) Window Coordinates - [ vertex ] [ M ] [ V ] [ P ] [ W ]
 *    Normalized Device Coordinates are scaled and translated
 *    by the viewport parameters to produce Window Coordinates
 *    x: 0 to viewport width
 *    y: 0 to viewport height
 *    z: 0 to 1
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
        const gmtl::Matrix44d& windowMatrix, const bool inStereo );

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
    const gmtl::Matrix44d& GetVrjViewMatrix() const;

    ///
    const osg::Matrixd& GetVrjViewMatrixOSG() const;

    ///
    const gmtl::Matrix44d& GetVrjCenterViewMatrix() const;

    ///
    const osg::Matrixd& GetVrjCenterViewMatrixOSG() const;
    
    ///
    const gmtl::Matrix44d& GetCameraMatrix() const;

    ///
    const osg::Matrixd& GetCameraMatrixOSG() const;

    ///
    const gmtl::Matrix44d& GetViewMatrix() const;

    ///
    const osg::Matrixd& GetViewMatrixOSG() const;

    ///
    const gmtl::Matrix44d& GetCenterViewMatrix() const;
    
    ///
    const osg::Matrixd& GetCenterViewMatrixOSG() const;
    
    ///
    const gmtl::Matrix44d& GetProjectionMatrix() const;

    ///
    const osg::Matrixd& GetProjectionMatrixOSG() const;

    ///
    const gmtl::Matrix44d& GetWindowMatrix() const;

    ///
    const osg::Matrixd& GetWindowMatrixOSG() const;

    ///
    const gmtl::Matrix44d GetVPWMatrix() const;

    ///
    const osg::Matrixd GetVPWMatrixOSG() const;

    ///
    const gmtl::Matrix44d GetCenterVPWMatrix() const;
    
    ///
    const osg::Matrixd GetCenterVPWMatrixOSG() const;
    
    ///
    void UpdateFrustumValues(
        const double& l, const double& r,
        const double& b, const double& t,
        const double& n, const double& f );

    ///
    void UpdateViewMatrix(
        const gmtl::Matrix44d& vrjViewMatrix,
        const gmtl::Matrix44d& cameraMatrix );

    ///Update the center view matrix from VR Juggler
    ///Note: This must be called AFTER UpdateViewMatrix
    void UpdateCenterViewMatrix( const gmtl::Matrix44d& vrjViewMatrix );
    
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
    gmtl::Matrix44d m_vrjViewMatrix;

    ///
    osg::Matrixd m_vrjViewMatrixOSG;

    ///
    gmtl::Matrix44d m_cameraMatrix;

    ///
    osg::Matrixd m_cameraMatrixOSG;

    ///
    gmtl::Matrix44d m_viewMatrix;

    ///
    osg::Matrixd m_viewMatrixOSG;

    ///
    gmtl::Matrix44d m_projectionMatrix;

    ///
    osg::Matrixd m_projectionMatrixOSG;

    ///
    const gmtl::Matrix44d m_windowMatrix;

    ///
    const osg::Matrixd m_windowMatrixOSG;

    ///
    gmtl::Matrix44d m_vrjCenterViewMatrix;
    
    ///
    osg::Matrixd m_vrjCenterViewMatrixOSG;
    
    ///
    gmtl::Matrix44d m_centerViewMatrix;
    
    ///
    osg::Matrixd m_centerViewMatrixOSG;
    
    ///Flag to tell if the viewport associated with this matrix stack is for a
    ///stereo viewport
    bool m_inStereo;
    
};
} //end scenegraph
} //end xplorer
} //end ves

#endif // end GL_TRANSFORM_INFO_H
