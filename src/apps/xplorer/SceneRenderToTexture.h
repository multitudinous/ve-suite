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

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Array>

namespace osg
{
class Camera;
class Geode;
class Geometry;
class Texture2D;
}

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
class SceneRenderToTexture
{
public:
    ///Constructor
    SceneRenderToTexture();
    ///Destructor
    ~SceneRenderToTexture();
    ///Return the camera being used to render the ves scenegraph 
    ///to texture. This is the root node for the scenegraph
    ///\return The osg::Camera being used to render to the FBO
    osg::Camera* GetRenderToTextureCamera();
    ///Return the texture that is being rendered for the desktop display
    ///\return The osg::Texture2D for the dispaly
    osg::Texture2D* GetRenderToTexture();

protected:

private:
    ///Create the texture of the appropriate size for the FBO to write to
    void CreateTexture();
    ///Create the quad to blit the texture to
    void CreateQuad();
    ///Create the camera wit hthe appropriate settings to render to an FBO
    void CreateCamera();

    ///The texture being used by the camera
    osg::ref_ptr< osg::Texture2D > mTexture;
    ///The render to texture camera
    osg::ref_ptr< osg::Camera > mCamera;
    ///The geode for the quad
    osg::ref_ptr< osg::Geode > mQuadGeode;
    ///The geometry for the quad
    osg::ref_ptr< osg::Geometry > mQuadGeometry;
    ///The verts for the quad
    osg::ref_ptr< osg::Vec3Array > mQuadVertices;
};
} //end xplorer
} //end ves

#endif //SCENE_RENDER_TO_TEXTURE