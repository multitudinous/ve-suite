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
#include <ves/xplorer/scenegraph/GroupedTextTextures.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/BlendFunc>
#include <osg/Depth>

#include <osgText/Text>

#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

#include <osgBullet/Chart.h>

using namespace ves::xplorer::scenegraph;

//#define VES_SRTT_DEBUG

////////////////////////////////////////////////////////////////////////////////
GroupedTextTextures::GroupedTextTextures( std::string fontFile )
    :
    osg::Group()
{
    _font = fontFile;

    LoadBackgroundTexture();
    CreateTexturedQuad();
    CreateText();
}
////////////////////////////////////////////////////////////////////////////////
GroupedTextTextures::GroupedTextTextures(
    const GroupedTextTextures& ttexture,
    const osg::CopyOp& copyop )
    :
    osg::Group( ttexture, copyop )
{
    _font = ttexture._font;
}
////////////////////////////////////////////////////////////////////////////////
GroupedTextTextures::~GroupedTextTextures()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::SetFont( std::string fontFile )
{
    _font = fontFile;
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::UpdateText( std::string newText )
{
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::LoadBackgroundTexture()
{
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::CreateTexturedQuad()
{
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::CreateText()
{
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::AddTextTexture( const std::string tempKey, TextTexturePtr tempTexture )
{
    m_groupedTextures[ tempKey ] = tempTexture;
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::RemoveTextTexture( const std::string tempKey )
{
    //m_groupedTextures[ tempKey ] = tempTexture;
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
