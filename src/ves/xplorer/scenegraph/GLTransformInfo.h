/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

// --- BackdropFX Includes --- //
namespace backdropFX
{
class RTTViewport;
}

// --- STL Includes --- //
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
 *
 * \class ves::xplorer::scenegraph::GLTransformInfo
 * \namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS GLTransformInfo
{
public:
    ///Constructor
    GLTransformInfo( bool const& inStereo );

    ///Copy Constructor
    GLTransformInfo( GLTransformInfo const& glTransformInfo );

    ///Destructor
    ~GLTransformInfo();

    ///
    int const& GetViewportOriginX() const;

    ///
    int const& GetViewportOriginY() const;

    ///
    int const& GetViewportWidth() const;

    ///
    int const& GetViewportHeight() const;

    ///This can't be const because osg::Camera::setViewport modifies it
    backdropFX::RTTViewport& GetBdfxRTTViewport() const;

    ///
    //int const& GetWindowOriginX() const;

    ///
    //int const& GetWindowOriginY() const;

    ///
    int const& GetWindowWidth() const;

    ///
    int const& GetWindowHeight() const;

    ///
    double const& GetLeftFrustum() const;

    ///
    double const& GetRightFrustum() const;

    ///
    double const& GetBottomFrustum() const;

    ///
    double const& GetTopFrustum() const;

    ///
    double const& GetNearFrustum() const;

    ///
    double const& GetFarFrustum() const;

    ///Calculates and returns the FoVZ
    double const& GetFOVZ();

    ///
    double const& GetAspectRatio() const;

    ///
    gmtl::Matrix44d const& GetVrjViewMatrix() const;

    ///
    osg::Matrixd const& GetVrjViewMatrixOSG() const;

    ///
    gmtl::Matrix44d const& GetVrjCenterViewMatrix() const;

    ///
    osg::Matrixd const& GetVrjCenterViewMatrixOSG() const;

    ///
    gmtl::Matrix44d const& GetCameraMatrix() const;

    ///
    osg::Matrixd const& GetCameraMatrixOSG() const;

    ///
    gmtl::Matrix44d const& GetViewMatrix() const;

    ///
    osg::Matrixd const& GetViewMatrixOSG() const;

    ///
    gmtl::Matrix44d const& GetCenterViewMatrix() const;

    ///
    osg::Matrixd const& GetCenterViewMatrixOSG() const;

    ///
    gmtl::Matrix44d const& GetProjectionMatrix() const;

    ///
    osg::Matrixd const& GetProjectionMatrixOSG() const;

    ///
    gmtl::Matrix44d const& GetWindowMatrix() const;

    ///
    gmtl::Matrix44d const& GetInvertedWindowMatrix() const;

    ///
    osg::Matrixd const& GetWindowMatrixOSG() const;

    ///
    osg::Matrixd const& GetInvertedWindowMatrixOSG() const;

    ///
    gmtl::Matrix44d const GetVPWMatrix() const;

    ///
    osg::Matrixd const GetVPWMatrixOSG() const;

    ///
    gmtl::Matrix44d const GetCenterVPWMatrix() const;

    ///
    osg::Matrixd const GetCenterVPWMatrixOSG() const;

    ///
    void UpdateFrustumValues(
        double const& l, double const& r,
        double const& b, double const& t,
        double const& n, double const& f );

    ///
    void UpdateViewMatrix(
        gmtl::Matrix44d const& vrjViewMatrix,
        gmtl::Matrix44d const& cameraMatrix );

    ///Update the center view matrix from VR Juggler
    ///Note: This must be called AFTER UpdateViewMatrix
    void UpdateCenterViewMatrix( gmtl::Matrix44d const& vrjViewMatrix );

    ///
    void UpdateViewportValues(
        int const& viewportOriginX,
        int const& viewportOriginY,
        int const& viewportWidth,
        int const& viewportHeight );

    ///
    void UpdateWindowValues(
        int const& windowWidth,
        int const& windowHeight );

protected:

private:
    ///
    void UpdateProjectionMatrix();

    ///
    void UpdateWindowMatrix();

    ///Flag to tell if the viewport associated with this matrix stack is for a
    ///stereo viewport
    const bool m_inStereo;

    ///
    int m_viewportOriginX;

    ///
    int m_viewportOriginY;

    ///
    int m_viewportWidth;

    ///
    int m_viewportHeight;

    ///
    osg::ref_ptr< backdropFX::RTTViewport > m_bdfxRTTViewport;

    ///
    //int m_windowOriginX;

    ///
    //int m_windowOriginY;

    ///
    int m_windowWidth;

    ///
    int m_windowHeight;

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
    double m_fovz;

    ///
    double m_aspectRatio;

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
    gmtl::Matrix44d m_windowMatrix;

    ///
    gmtl::Matrix44d m_invertedWindowMatrix;

    ///
    osg::Matrixd m_windowMatrixOSG;

    ///
    osg::Matrixd m_invertedWindowMatrixOSG;

    ///
    gmtl::Matrix44d m_vrjCenterViewMatrix;

    ///
    osg::Matrixd m_vrjCenterViewMatrixOSG;

    ///
    gmtl::Matrix44d m_centerViewMatrix;

    ///
    osg::Matrixd m_centerViewMatrixOSG;

};
} //end scenegraph
} //end xplorer
} //end ves

#endif // end GL_TRANSFORM_INFO_H
