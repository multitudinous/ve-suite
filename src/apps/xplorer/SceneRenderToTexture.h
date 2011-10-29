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

#ifndef SCENE_RENDER_TO_TEXTURE
#define SCENE_RENDER_TO_TEXTURE

// --- VES Includes --- //
#include "SceneRenderToTexturePtr.h"

#include <ves/open/xml/CommandPtr.h>

// ---  VR Juggler Includes --- //
#include <vrj/vrjParam.h>
#include <vrj/Draw/OpenGL/ContextData.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Matrixd>
#include <osg/Texture2D>

namespace osg
{
class Group;
class Geode;
class Camera;
class Light;
class LightSource;
class LightModel;
}

namespace osgUtil
{
class NodeVisitor;
class SceneView;
}

namespace osgwTools
{
class ScreenCapture;
}

// --- STL Includes --- //
#include <map>
#include <string>

namespace ves
{
namespace xplorer
{

namespace scenegraph
{
namespace rtt
{
class Processor;
}
}

/*!\file SceneRenderToTexture.h
 *
 */

/*!\class ves::xplorer::SceneRenderToTexture
 *
 */

/*!\namespace ves::xplorer
 *
 */
class SceneRenderToTexture
{
public:
    ///Constructor
    SceneRenderToTexture( bool const& enableRTT );

    ///Destructor
    ~SceneRenderToTexture();

    ///
    bool CameraConfigured();

    ///Initialized
    void InitializeRTT();

    ///
    osg::Light* GetLight0() const;

    ///Get the root node for all children in the scene to be added to
    ///\return The root osg::Group node
    osg::Group* GetRootGroup() const;

    ///Get the post process camera for this context
    ///\return The post process camera for this context
    osg::Camera* GetPostProcessCamera();

    ///Initialize correct screen info for the texture and quad
    ///NOTE: MUST be called AFTER EnvironmentHandler::InitScene
    void InitScene( osg::Camera* const svCamera );

    ///Take a high resolution screen capture of the render window for SceneView
    ///\param root The osg::Group to be rendered
    ///\param sv The osgUtil::SceneView to provide the context for the render
    ///\param filename The file name to be used for the screen capture
    void WriteImageFileForWeb(
        osg::Group* root, osgUtil::SceneView* sv, std::string& filename );

    ///Take a low resolution screen capture of the render window for SceneView
    ///\param root The osg::Group to be rendered
    ///\param sv The osgUtil::SceneView to provide the context for the render
    ///\param filename The file name to be used for the screen capture
    void WriteLowResImageFile(
        osg::Group* root, osgUtil::SceneView* sv, std::string& filename );

    ///Update function to traverse
    void Update(
        osg::NodeVisitor* updateVisitor,
        ves::open::xml::CommandPtr tempCommand );

    ///capture image callback
    void SetImageCameraCallback( bool capture, const std::string& filename );

protected:

private:
    ///
    void InitRootGroup();

    ///
    void InitRTTCamera(
        osg::Camera* const rttCamera,
        std::pair< int, int > const& viewportDimensions );

    ///
    scenegraph::rtt::Processor* CreatePostProcessPipeline(
        osg::Camera* rttCamera,
        std::pair< int, int > const& viewportDimensions );

    ///
    osg::Camera* CreatePostProcessCamera();

    ///
    osg::Texture2D* CreateViewportTexture(
        GLenum internalFormat,
        GLenum sourceFormat,
        GLenum sourceType,
        osg::Texture2D::FilterMode filterMode,
        osg::Texture2D::WrapMode wrapMode,
        std::pair< int, int > const& viewportDimensions );

    ///
    osg::Geode* CreateClearColorQuad( unsigned int const& numViewports );

    ///
    osg::Geode* CreateRTTQuad( osg::Texture2D* texture );

    ///
    void SetupDefaultLighting();

    ///
    bool m_enableRTT;

    ///
    std::vector< osg::Camera* > m_updateList;

    ///
    std::map< osg::Camera*,
              osg::ref_ptr< osgwTools::ScreenCapture > > m_captureTools;

    ///The root group that everything gets added to
    ///Is the same for all contexts
    osg::ref_ptr< osg::Group > m_rootGroup;

    ///Shader pointers
    osg::ref_ptr< osg::Shader > m_1dxVP;
    osg::ref_ptr< osg::Shader > m_1dxFP;
    osg::ref_ptr< osg::Shader > m_1dyVP;
    osg::ref_ptr< osg::Shader > m_1dyFP;
    osg::ref_ptr< osg::Shader > m_finalShader;

    ///Let the object know all cameras are configured
    vrj::opengl::ContextData< bool > m_camerasConfigured;

    ///
    vrj::opengl::ContextData< osg::ref_ptr< osg::Camera > > m_postProcessCamera;

    ///Light for the scene
    osg::ref_ptr< osg::Light > m_light0;

    ///Light source for the scene
    osg::ref_ptr< osg::LightSource > m_lightSource0;

    ///Light model for the scene
    osg::ref_ptr< osg::LightModel > m_lightModel0;
    
    ///Add the ui group to the cameras
    bool m_isUIAdded;

};
} //end xplorer
} //end ves

#endif //SCENE_RENDER_TO_TEXTURE
