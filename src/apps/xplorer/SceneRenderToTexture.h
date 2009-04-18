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

#ifndef SCENE_RENDER_TO_TEXTURE
#define SCENE_RENDER_TO_TEXTURE

// --- VE-Suite Includes --- //
#include "SceneRenderToTexturePtr.h"

// ---  VR Juggler Includes --- //
#include <vrj/vrjParam.h>
#if __VJ_version >= 2003000
#include <vrj/Draw/OpenGL/ContextData.h>
#else
#include <vrj/Draw/OGL/GlContextData.h>
#endif

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Matrixd>
#include <osg/Texture2D>

namespace osg
{
class Group;
class Geode;
class Camera;
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

// --- VE-Suite Includes --- //
namespace scenegraph
{
namespace rtt
{
class Processor;
}
}

/*!\file SceneRenderToTexture.h
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
    SceneRenderToTexture();

    ///Destructor
    ~SceneRenderToTexture();

    ///Initialize correct screen info for the texture and quad
    ///NOTE: MUST be called AFTER EnvironmentHandler::InitScene
    void InitScene( osg::Camera* const sceneViewCamera );

    ///Get the root node for all children in the scene to be added to
    ///\return The root osg::Group node
    osg::Group* const GetGroup() const;

    ///Get the camera with specified vrj::Viewport
    ///\return The camera with specified vrj::Viewport
//#if __VJ_version >= 2003000    
//    osg::Camera* GetCamera( vrj::ViewportPtr viewport );
//#else    
//    osg::Camera* GetCamera( vrj::Viewport* viewport );
//#endif
    ///Update something
    ///NOTE: Must have an active context to call
    void UpdateRTTQuadAndViewport();
    
    void ConfigureRTTCameras();

    ///Take a high resolution screen capture of the render window for SceneView
    ///\param root The osg::Group to be rendered
    ///\param sv The osgUtil::SceneView to provide the context for the render
    ///\param filename The file name to be used for the screen capture
    void WriteImageFileForWeb(
        osg::Group* root, osgUtil::SceneView* sv, std::string& filename );

protected:

private:
    ///
    osg::Camera* CreatePipelineCamera( osg::Viewport* viewport );

    ///
#if __VJ_version >= 2003000
    scenegraph::rtt::Processor* CreatePipelineProcessor(
        vrj::ViewportPtr viewport, osg::Camera* camera, osg::Camera* svCamera  );
#else
    scenegraph::rtt::Processor* CreatePipelineProcessor(
        vrj::Viewport* viewport, osg::Camera* camera, osg::Camera* svCamera  );
#endif
    
    ///
    osg::Texture2D* CreateViewportTexture(
        GLenum internalFormat,
        GLenum sourceFormat,
        GLenum sourceType,
        osg::Texture2D::FilterMode filterMode,
        osg::Texture2D::WrapMode wrapMode,
        std::pair< int, int >& viewportDimensions );
    
    ///
#if __VJ_version >= 2003000
    osg::Geode* CreateTexturedQuad(
        vrj::ViewportPtr viewport, osg::Texture2D* texture );
#else
    osg::Geode* CreateTexturedQuad(
        vrj::Viewport* viewport, osg::Texture2D* texture );
#endif

    ///Set the number of super samples
    int mScaleFactor;

    ///Let the object know all cameras are configured
#if __VJ_version >= 2003000
    vrj::opengl::ContextData< bool > mCamerasConfigured;
#else
    vrj::GlContextData< bool > mCamerasConfigured;
#endif

    ///A typedef to make it easier to define iterators
    typedef std::pair<
        osg::ref_ptr< osg::Camera >,
        osg::ref_ptr< scenegraph::rtt::Processor > > PipelinePair;

    ///The render to texture cameras
    ///A context locked map to hold post-process pipelines for each viewport per context
#if __VJ_version >= 2003000
    typedef std::map< vrj::ViewportPtr, PipelinePair > PipelineMap;
    vrj::opengl::ContextData< PipelineMap > mPipelines;
#else
    typedef std::map< vrj::Viewport*, PipelinePair > PipelineMap;
    vrj::GlContextData< PipelineMap > mPipelines;
#endif
    ///The root group that everything gets added to
    ///Is the same for all contexts
    osg::ref_ptr< osg::Group > mRootGroup;
};
} //end xplorer
} //end ves

#endif //SCENE_RENDER_TO_TEXTURE
