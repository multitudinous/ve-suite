/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <vrj/Draw/OGL/GlContextData.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Matrixd>

namespace osg
{
class Group;
class Camera;
class Texture2D;
class RenderInfo;
class Geode;
class MatrixTransform;
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
/*!\file SceneRenderToTexture.h
 */

/*!\class ves::xplorer::SceneRenderToTexture
 *
 */

/*!\namespace ves::xplorer
 *
 */

/*
struct StencilImage : public osg::Camera::DrawCallback
{
    StencilImage();

    virtual void operator () ( osg::RenderInfo& renderInfo ) const;

    osg::ref_ptr< osg::Image > _image;
};
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

    ///Return the camera being used to render the ves scenegraph 
    ///to texture. This is the root node for the scenegraph
    ///\return The osg::Camera being used to render to the FBO
    osg::Camera* const GetCamera();

    ///Get the root node for all children in the scene to be added to
    ///\return The root osg::Group node
    osg::Group* const GetGroup() const;

    ///Return the texture that is being rendered for the desktop display
    ///\return The osg::Texture2D for the display
    osg::Texture2D* const GetColorMap();

    ///
    ///\return
    //osg::MatrixTransform* const GetQuad();

    ///Update something
    ////NOTE: Must have an active context to call
    void UpdateRTTQuadAndViewportMatrix( osgUtil::SceneView* sceneView, 
                                        osg::Matrixd quadTransform );

    ///Update the projection and viewport information for the rtt's cameras
    ///NOTE: Must have an active context to call
    void UpdateRTTProjectionAndViewportMatrix( osgUtil::SceneView* sv ){;}
    
    ///Take a high resolution screen capture of the render window for SceneView
    ///\param root The osg::Group to be rendered
    ///\param sv The osgUtil::SceneView to provide the context for the render
    ///\param filename The file name to be used for the screen capture
    void WriteImageFileForWeb(
        osg::Group* root, osgUtil::SceneView* sv, std::string& filename );

protected:

private:
    ///
    osg::Geode* CreateFullScreenTexturedQuad(
                                             std::pair< int, int > screenDims, osg::Texture2D* colorTexture );
    
    ///
    osg::Texture2D* CreateFBOTexture(
                                     std::pair< int, int >& screenDims, float scale = 1.0f );
    
    ///Create the camera with the appropriate settings to render to an FBO
    void InitCamera( std::pair< int, int >& screenDims );

    ///Create the texture of the appropriate size for the FBO to write to
    void InitTextures( std::pair< int, int >& screenDims );
    
    ///The root group that everything gets added to
    osg::ref_ptr< osg::Group > mRootGroup;

    ///Set the number of super samples
    int mScaleFactor;

    ///
    int mAdjustedScreenRes;

    ///Let the object know all cameras are configured
    vrj::GlContextData< bool > mCamerasConfigured;

    ///The render to texture camera
    ///A context locked map to hold cameras
    vrj::GlContextData< osg::ref_ptr< osg::Camera > > mCameraMap;

    ///The texture attached to the color buffer of the camera
    ///A context locked map to hold textures
    vrj::GlContextData< osg::ref_ptr< osg::Texture2D > > mColorMap;

    ///A context locked map to hold textures
    vrj::GlContextData< osg::ref_ptr< osg::Texture2D > > mGlowMap;

    ///A context locked map to hold textures
    vrj::GlContextData< osg::ref_ptr< osg::Texture2D > > mGlowStencil;

    ///The texture attached to the depth and stencil buffer of the camera
    ///A context locked map to hold textures
    vrj::GlContextData< osg::ref_ptr< osg::Texture2D > > mDepthStencilTexture;

    ///
    ///
    vrj::GlContextData< osg::ref_ptr< osg::MatrixTransform > > mQuad;

};
} //end xplorer
} //end ves

#endif //SCENE_RENDER_TO_TEXTURE
