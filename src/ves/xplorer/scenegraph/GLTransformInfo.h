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

// --- VR Juggler Includes --- //
#include <gmtl/Matrix.h>

#if __VJ_version >= 2003000
#include <vrj/Display/ViewportPtr.h>
#else
namespace vrj
{
class Viewport;
}
#endif

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
        const unsigned int& viewportOriginX, const unsigned int& viewportOriginY,
        const unsigned int& viewportWidth, const unsigned int& viewportHeight,
        const gmtl::Matrix44d& windowMatrix );

    ///Copy Constructor
    GLTransformInfo( const GLTransformInfo& glTransformInfo );

    ///Destructor
    ~GLTransformInfo();

    ///
    const unsigned int& GetViewportOriginX() const;

    ///
    const unsigned int& GetViewportOriginY() const;

    ///
    const unsigned int& GetViewportWidth() const;

    ///
    const unsigned int& GetViewportHeight() const;

    ///
    const gmtl::Matrix44d& GetModelViewMatrix() const;

    ///
    const gmtl::Matrix44d& GetProjectionMatrix() const;

    ///
    const gmtl::Matrix44d& GetWindowMatrix() const;

    ///
    const gmtl::Matrix44d GetMVPWMatrix() const;

protected:

private:
    ///
    const unsigned int m_viewportOriginX;
    ///
    const unsigned int m_viewportOriginY;
    ///
    const unsigned int m_viewportWidth;
    ///
    const unsigned int m_viewportHeight;

    ///
    float m_leftFrustum;
    ///
    float m_rightFrustum;
    ///
    float m_bottomFrustum;
    ///
    float m_topFrustum;
    ///
    float m_nearFrustum;
    ///
    float m_farFrustum;

    ///
    gmtl::Matrix44d m_modelViewMatrix;

    ///
    gmtl::Matrix44d m_projectionMatrix;

    ///
    const gmtl::Matrix44d m_windowMatrix;

};
} //end scenegraph
} //end xplorer
} //end ves

#endif // end GL_TRANSFORM_INFO_H
