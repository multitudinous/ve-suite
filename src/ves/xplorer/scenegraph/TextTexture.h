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
#include <ves/VEConfig.h>
#include <ves/xplorer/scenegraph/TextTexturePtr.h>

// --- OSG Includes --- //
#include <osg/Version>
#include <osg/Geode>
#include <osg/Group>
namespace osgBullet
{
    class Chart;
}

namespace osg
{
class Drawable;
class Texture2D;
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
class VE_SCENEGRAPH_EXPORTS TextTexture : public osg::Group
{
public:
    ///Constructor
    ///\param textureResolutionX The X resolution of the output texture in pixels
    ///\param textureResolutionY The Y resolution of the output texture in pixels
    ///\param fontFile The file to load fonts from. See osgText/Font for usage.
    TextTexture( std::string fontFile = "fonts/arial.ttf" );

    ///Copy constructors for osg
    TextTexture( const TextTexture& ttexture,
                 const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph, TextTexture );

    ///Set the color of the text
    ///\param color Text color
    void SetTextColor( float color[ 4 ] );

    ///Set the font
    ///\param fontFile The file containing the font to use
    void SetFont( std::string fontFile );

    ///Update the text
    ///\param newText The new text
    void UpdateText( std::string newText );

    ///Get the texture with the text
    osg::Texture2D* GetTexture();

    ///Create the data display chart
    void CreateChart();

    ///Get the data display chart
    osgBullet::Chart* GetChart();
    
    ///Set the title for the dialog
    void SetTitle( const std::string& title );
    
    ///Get the title for the dialog
    const std::string& GetTitle();
    
protected:
    ///Destructor
    virtual ~TextTexture();

    ///Load the backgroud texture the text will render over
    void LoadBackgroundTexture();

    ///
    void CreateTexturedQuad();

    ///
    void CreateText();

    ///The color of the text, default is black
    float _textColor[ 4 ];

    ///The font file
    std::string _font;

    ///The title for the dialog
    std::string m_title;
    
    ///The body text
    osg::ref_ptr< osgText::Text > m_bodyText;

    ///The title text
    osg::ref_ptr< osgText::Text > m_titleText;
    
    ///The texture we create
    osg::ref_ptr< osg::Texture2D > _texture;

    //The update callback
    //osg::ref_ptr< TextUpdateCallback > _ttUpdateCallback;
    osgBullet::Chart* m_chartSurface;
};
}
}
}
#endif //TEXT_TEXTURE_H
