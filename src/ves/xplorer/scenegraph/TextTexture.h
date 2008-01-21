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
#ifndef TEXT_TEXTURE_H
#define TEXT_TEXTURE_H

/*!\file TextTexture.h
*Text Texture API that renders text offscreen to a texture
*/

/*!\class ves::xplorer::scenegraph::TextTexture
*
*/

/*!\namespace ves::xplorer::scenegraph
*
*/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/Geode.h>

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Version>
#if (OSG_VERSION_MAJOR>=2)
#include <osg/Camera>
#endif

namespace osg
{
class Texture2D;
#if (OSG_VERSION_MAJOR>=2)
class Camera;
#endif
}

namespace osgText
{
class Font;
class Text;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class VE_SCENEGRAPH_EXPORTS TextTexture : public ves::xplorer::scenegraph::Geode
{
public:
    ///Constructor
    ///\param textureResolutionX The X resolution of the output texture in pixels
    ///\param textureResolutionY The Y resolution of the output texture in pixels
    ///\param fontFile The file to load fonts from. See osgText/Font for usage.
    TextTexture( unsigned int textureResolutionX = 1024,
                 unsigned int textureResolutionY = 1024,
                 std::string fontFile = "fonts/arial.ttf" );

    ///Copy constructors for osg
    TextTexture( const TextTexture& ttexture,
                 const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Node( ves::xplorer::scenegraph, TextTexture );

    ///Set the color of the text
    ///\param color Text color
    void SetTextColor( float color[4] );

    ///Set the font
    ///\param fontFile The file containing the font to use
    void SetFont( std::string fontFile );

    ///Update the text
    ///\param newText The new text
    void UpdateText( std::string newText );

    ///Get the texture with the text
    osg::Texture2D* GetTexture();

    ///Get the fbo
#if (OSG_VERSION_MAJOR>=2)
    osg::Camera* GetCameraNode();
#else
    osg::CameraNode* GetCameraNode();
#endif

protected:
    ///Destructor
    virtual ~TextTexture();

    ///Initialize the Frame Buffer Object in the camera node
    void _initializeFBO();

    bool _fboInitialized;///<Flag for camera node state
    float _textColor[4];///<The color of the text, default is black
    unsigned int _textureResolution[2];///<The texture resolution
    std::string _font;///<The font file
    osg::ref_ptr<osgText::Text> _text;///<The text
    osg::ref_ptr<osg::Texture2D> _texture;///<The texture we create
#if (OSG_VERSION_MAJOR>=2)
    osg::ref_ptr<osg::Camera> _fbo;///<The off screen rendering node
#else
    osg::ref_ptr<osg::CameraNode> _fbo;///<The off screen rendering node
#endif
    //osg::ref_ptr<TextUpdateCallback> _ttUpdateCallback;///<The update callback

};
}
}
}

#endif//_OSG

#endif //TEXT_TEXTURE_H
