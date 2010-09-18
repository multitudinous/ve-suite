/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef SCENE_GL_TRANSFORM_INFO
#define SCENE_GL_TRANSFORM_INFO

// --- VE-Suite Includes --- //
#include "SceneGLTransformInfoPtr.h"

#include <ves/xplorer/scenegraph/GLTransformInfoPtr.h>

// ---  VR Juggler Includes --- //
#include <vrj/vrjParam.h>

#include <vrj/Draw/OpenGL/ContextData.h>

#include <gmtl/Matrix.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Matrixd>

namespace osg
{
    ;
}

namespace osgUtil
{
class SceneView;
}

// --- C/C++ Libraries --- //
#include <map>
#include <string>

namespace ves
{
namespace xplorer
{

/*!\file SceneGLTransformInfo.h
 *
 */

/*!\class ves::xplorer::SceneGLTransformInfo
 *
 */

/*!\namespace ves::xplorer
 *
 */
class SceneGLTransformInfo
{
public:
    ///Constructor
    SceneGLTransformInfo(
        gmtl::Matrix44d const& ortho2DMatrix,
        gmtl::Matrix44d const& identityMatrix,
        gmtl::Matrix44d const& zUpMatrix );

    ///Destructor
    ~SceneGLTransformInfo();

    ///
    ///\return
    scenegraph::GLTransformInfoPtr const GetGLTransformInfo(
        vrj::ViewportPtr const viewport );

    ///
    gmtl::Matrix44d const& GetOrtho2DMatrix() const;

    ///
    osg::Matrixd const& GetOrtho2DMatrixOSG() const;

    ///
    gmtl::Matrix44d const& GetIdentityMatrix() const;

    ///
    osg::Matrixd const& GetIdentityMatrixOSG() const;

    ///
    gmtl::Matrix44d const& GetZUpMatrix() const;

    ///
    osg::Matrixd const& GetZUpMatrixOSG() const;

    ///Initialize transform info for the scene w.r.t each viewport
    ///NOTE: MUST be called AFTER EnvironmentHandler::InitScene
    void Initialize();
    ///Calculate the projection for the center view of the of the view matrix
    ///This is needed because vr juggler is always calculating a 
    ///Left/Right combo for the view
    void CalculateCenterViewMatrix( vrj::ProjectionPtr const projection );
    
protected:

private:    
    ///
    const gmtl::Matrix44d m_ortho2DMatrix;

    ///
    const osg::Matrixd m_ortho2DMatrixOSG;

    ///
    const gmtl::Matrix44d m_identityMatrix;

    ///
    const osg::Matrixd m_identityMatrixOSG;

    ///
    const gmtl::Matrix44d m_zUpMatrix;

    ///
    const osg::Matrixd m_zUpMatrixOSG;

    ///
    typedef std::map<
        vrj::ViewportPtr, scenegraph::GLTransformInfoPtr > GLTransformInfoMap;
    ///
    vrj::opengl::ContextData< GLTransformInfoMap > m_glTransformInfoMap;

};
} //end xplorer
} //end ves

#endif //SCENE_GL_TRANSFORM_INFO
