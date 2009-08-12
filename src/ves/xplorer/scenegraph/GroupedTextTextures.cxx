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

#include <ves/xplorer/scenegraph/TextTexture.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/BlendFunc>
#include <osg/Depth>

#include <osgText/Text>

#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

#include <osgBullet/Chart.h>

#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
GroupedTextTextures::GroupedTextTextures( std::string fontFile )
    :
    osg::Group()
{
    _font = fontFile;
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
void GroupedTextTextures::MakeTextureActive( const std::string& tempKey )
{
    std::map< std::string, osg::ref_ptr< DCS > >::iterator iter = m_groupedTextures.find( tempKey );
    
    if( iter != m_groupedTextures.end() )
    {
        MakeTextureActive( static_cast< TextTexture* >( iter->second->getChild( 0 ) ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::MakeTextureActive( const TextTexture* tempKey )
{
    //std::map< std::string, osg::ref_ptr< DCS > >::iterator iter = m_groupedTextures.find( tempKey );
    
    //if( iter != m_groupedTextures.end() )
    {
        //find dcs in list
        std::list< DCS* >::iterator listIter = std::find( m_transformList.begin(), m_transformList.end(), tempKey->getParent( 0 ) );
        //make this dcs the first one in the list
        std::cout << std::distance( m_transformList.begin(), listIter ) << std::endl;
        m_transformList.insert( m_transformList.begin(), listIter, m_transformList.end() );
        std::cout << std::distance( m_transformList.begin(), listIter ) << std::endl;
        m_transformList.erase( listIter, m_transformList.end() );
        //Now update all the positions of the other textures
        UpdateListPositions();
    }
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::AddTextTexture( const std::string& tempKey, TextTexture* tempTexture )
{
    osg::ref_ptr< DCS > tempDCS = new DCS();
    tempDCS->addChild( tempTexture );
    m_groupedTextures[ tempKey ] = tempDCS.get();
    addChild( tempDCS.get() );
    
    m_transformList.push_front( tempDCS.get() );
    m_activeDCS = tempDCS.get();
    
    UpdateListPositions();
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::UpdateListPositions()
{
    for( std::list< DCS* >::iterator iter = m_transformList.begin(); 
        iter != m_transformList.end(); ++iter)
    {
        size_t i = std::distance( m_transformList.begin(), iter );
        UpdateDCSPosition( (*iter), i );
    }
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::UpdateDCSPosition( DCS* tempDCS, size_t i )
{
    //y = ax + b
    double b = 0.0f;
    double a = 0.5f;
    double x = 0.0f;
    double deltaX = 1.0f;
    double y = 0.0f;
    
    x = i * deltaX;
    y = a * x + b;
    
    double pos[ 3 ];
    pos[ 0 ] = 0.0f;
    pos[ 1 ] = x;
    pos[ 2 ] = y;

    tempDCS->SetTranslationArray( pos );
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::RemoveTextTextures( const std::string& tempKey )
{
    if( tempKey.empty() )
    {
        m_transformList.clear();
        m_groupedTextures.clear();
    }
    else
    {
        ;
    }
}
