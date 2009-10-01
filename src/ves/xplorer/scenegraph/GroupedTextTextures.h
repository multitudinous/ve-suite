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
#ifndef GROUPED_TEXT_TEXTURES_H
#define GROUPED_TEXT_TEXTURES_H

/*!\file GroupedTextTextures.h
*Text Texture API that renders text offscreen to a texture
*/

/*!\class ves::xplorer::scenegraph::GroupedTextTextures
*
*/

/*!\namespace ves::xplorer::scenegraph
*
*/

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Version>
#include <osg/Geode>
#include <osg/Group>

#include <list>
#include <map>

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
class TextTexture;

class VE_SCENEGRAPH_EXPORTS GroupedTextTextures : public osg::Group
{
public:
    ///Constructor
    ///\param textureResolutionX The X resolution of the output texture in pixels
    ///\param textureResolutionY The Y resolution of the output texture in pixels
    ///\param fontFile The file to load fonts from. See osgText/Font for usage.
    GroupedTextTextures( std::string fontFile = "fonts/arial.ttf" );

    ///Copy constructors for osg
    GroupedTextTextures( const GroupedTextTextures& ttexture,
                 const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph, GroupedTextTextures );

    ///Set the font
    ///\param fontFile The file containing the font to use
    void SetFont( std::string fontFile );

    ///Get the texture with the text
    void AddTextTexture( const std::string& tempKey, TextTexture* tempTexture );

    void RemoveTextTextures( const std::string& tempKey );

    void UpdateDCSPosition( ves::xplorer::scenegraph::DCS* tempDCS, size_t i );

    void UpdateListPositions();

    void MakeTextureActive( const std::string& tempKey );
    
    void MakeTextureActive( const TextTexture* tempKey );

    void MakeTextureActive( const ves::xplorer::scenegraph::DCS* tempKey );

    const std::string& GetKeyForTexture( const ves::xplorer::scenegraph::DCS* tempKey );

    ///Return if the animation is still being processed
    bool AnimationComplete();
    ///Update position of all of the attached textures
    void UpdateTexturePosition();
    
protected:
    ///Destructor
    virtual ~GroupedTextTextures();
    ///Animate the texture position
    void AnimateTextureMovement();
    
    ///The color of the text, default is black
    float _textColor[ 4 ];

    ///The font file
    std::string _font;

    std::map< std::string, osg::ref_ptr< DCS > > m_groupedTextures;
    
    osg::ref_ptr< DCS > m_activeDCS;
    
    std::list< DCS* > m_transformList;
    ///Start location for lerp animation functions
    double m_yStartLocation;
    ///Selected DCS
    DCS* m_selectedTexture;
    ///Current DCS 
    DCS* m_currentTexture;
    ///Tell if the animation is still running
    bool m_animationComplete;
    ///Determine if this group should animate dialog movement
    bool m_animateTextures;
};
}
}
}
#endif //GROUPED_TEXT_TEXTURES_H
