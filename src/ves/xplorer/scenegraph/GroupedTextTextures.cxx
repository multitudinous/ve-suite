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
#include <osg/Group>

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
    //getOrCreateStateSet()->setAttributeAndModes( 
    //    new osg::Depth( osg::Depth::ALWAYS ), 
    //    osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
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
    //std::map< std::string, osg::ref_ptr< DCS > >::iterator iter = m_groupedTextures.find( tempKey->getParent( 0 ) );
    
    //if( iter != m_groupedTextures.end() )
    {
        //find dcs in list
        osg::Group* tempGroup = const_cast< osg::Group* >( tempKey->getParent( 0 ) );
        ves::xplorer::scenegraph::DCS* tempParent =
            static_cast< ves::xplorer::scenegraph::DCS* >( tempGroup );
        std::list< DCS* >::iterator listIter = 
            std::find( m_transformList.begin(), m_transformList.end(), 
            tempParent );
        for( listIter = m_transformList.begin(); 
            listIter != m_transformList.end(); 
            ++listIter )
        {
            std::cout << tempParent << " " << (*listIter) << std::endl;
            if( tempParent == (*listIter) )
            {
                //std::cout << " found DCS " << std::endl;
                break;
            }
        }

        if( listIter == m_transformList.end() )
        {
            std::cout << "Not in list " << std::endl;
            return;
        }
        //make this dcs the first one in the list
        m_transformList.insert( m_transformList.begin(), listIter, m_transformList.end() );
        m_transformList.erase( listIter, m_transformList.end() );
        //Now update all the positions of the other textures
        UpdateListPositions();
    }
    //else
    //{
    //    std::cout << "No texture found " << std::endl;
    //}
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::MakeTextureActive( const ves::xplorer::scenegraph::DCS* tempKey )
{
    //std::map< std::string, osg::ref_ptr< DCS > >::iterator iter = m_groupedTextures.find( tempKey->getParent( 0 ) );
    
    //if( iter != m_groupedTextures.end() )
    {
        //find dcs in list
        //osg::Group* tempGroup = const_cast< osg::Group* >( tempKey->getParent( 0 ) );
        //ves::xplorer::scenegraph::DCS* tempParent =
        //static_cast< ves::xplorer::scenegraph::DCS* >( tempGroup );
        std::list< DCS* >::iterator listIter = 
        std::find( m_transformList.begin(), m_transformList.end(), 
                  tempKey );
        if( listIter == m_transformList.end() )
        {
            std::cout << "Not in list " << std::endl;
            //return;
        }
        /*for( listIter = m_transformList.begin(); 
            listIter != m_transformList.end(); 
            ++listIter )
        {
            std::cout << tempKey << " " << (*listIter) << " " << (*listIter)->getName() << std::endl;
            if( tempKey == (*listIter) )
            {
                std::cout << " found DCS " << std::endl;
                break;
            }
            if( (*listIter)->getName() == tempKey->getName() )
            {
                std::cout << " found DCS " << std::endl;
                break;
            }
        }*/
        
        if( listIter == m_transformList.end() )
        {
            std::cout << "Not in list " << std::endl;
            return;
        }
        //make this dcs the first one in the list
        m_transformList.insert( m_transformList.begin(), listIter, m_transformList.end() );
        m_transformList.erase( listIter, m_transformList.end() );
        //Now update all the positions of the other textures
        UpdateListPositions();
    }
    //else
    //{
    //    std::cout << "No texture found " << std::endl;
    //}
}
////////////////////////////////////////////////////////////////////////////////
void GroupedTextTextures::AddTextTexture( const std::string& tempKey, TextTexture* tempTexture )
{
    DCS* tempDCS = new DCS();
    tempDCS->addChild( tempTexture );
    std::string nameString = "VES_TextTexture_" + tempKey;
    //tempTexture->setName( nameString );
    tempDCS->setName( nameString );
    m_groupedTextures[ tempKey ] = tempDCS;
    addChild( tempDCS );
    m_transformList.push_front( tempDCS );
    m_activeDCS = tempDCS;

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
    double a = 0.4f;
    double x = 0.0f;
    double deltaX = 1.0f;
    double y = 0.0f;
    double z = 0.0f;
    i+=1;
    x = i * -0.30f;
    y = i * deltaX;
    z = a * y + b;
    
    double pos[ 3 ];
    pos[ 0 ] = x;//0.0f;//-i;// * 0.0f;
    pos[ 1 ] = y;
    pos[ 2 ] = z;

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
