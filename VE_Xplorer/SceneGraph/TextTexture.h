/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TEXT_TEXTURE_H
#define TEXT_TEXTURE_H
#ifdef _OSG
/*!\file TextTexture.h
* Text Texture API that renders text offscreen to a texture
*/

/*!\class VE_SceneGraph::TextTexture
*
*/
#include <osg/Version>
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2))
#include <osg/CameraNode>
#endif

namespace osg
{
   class Texture2D;
#if ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
   class CameraNode;
#endif
}
namespace osgText
{
   class Font;
   class Text;
}
#include "VE_Xplorer/SceneGraph/Geode.h"

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS TextTexture : public VE_SceneGraph::Geode
{
public:
   ///Constructor
   ///\param textureResolutionX The X resolution of the output texture in pixels
   ///\param textureResolutionY The Y resolution of the output texture in pixels
   ///\param fontFile The file to load fonts from. See osgText/Font for usage.
   TextTexture(unsigned int textureResolutionX=1024,
               unsigned int textureResolutionY=1024,
               std::string fontFile="fonts/arial.ttf");
   ///Copy constructors for osg
   TextTexture( const TextTexture& ttexture, 
                const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY );
   
   META_Node( VE_SceneGraph, TextTexture );
   ///set the color of the text
   ///\param color Text color
   void SetTextColor(float color[4]);

   ///Set font
   ///\param fontFile The file containing the font to use 
   void SetFont(std::string fontFile);

   void UpdateText(std::string newText);

   ///Get the texture with the text
   osg::Texture2D* GetTexture();

   ///Get the fbo
   osg::CameraNode* GetCameraNode();
   
protected:
   ///Destructor
   virtual ~TextTexture();

   ///Initialize the Frame Buffer Object in the camera node
   void _initializeFBO();

   bool _fboInitialized;///<Flag for camera node state
   float _textColor[4];///<The color of the text. Default is black;
   unsigned int _textureResolution[2];///<The texture resolution
   std::string _font;///<The font file
   osg::ref_ptr<osgText::Text> _text;///<The text
   osg::ref_ptr<osg::Texture2D> _texture;///<The texture we create
   osg::ref_ptr<osg::CameraNode> _fbo;///<The off screen rendering node
   //osg::ref_ptr<TextUpdateCallback> _ttUpdateCallback;///<The update callback
};
}
#endif//_OSG
#endif //TEXT_TEXTURE_H
