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

// --- My Includes --- //
#include "Block.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/LineWidth>

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>

//C/C++ Libraries
#include <iostream>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
Block::Block()
{
    CreateBlock();
}
////////////////////////////////////////////////////////////////////////////////
Block::Block( const Block& block, const osg::CopyOp& copyop )
:
osg::Geode( block, copyop )
{
    if( &block != this )
    {
        //m_isAttached = block.m_isAttached;
    }
}
////////////////////////////////////////////////////////////////////////////////
Block::~Block()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Block::CreateBlock()
{
	osg::ref_ptr< osg::Image > image = osgDB::readImageFile( "Textures/tile.jpg" );

	if( !image )
	{
        std::cout << "Invalid texture file!" << std::endl << std::endl;
	}

    osg::ref_ptr< osg::Geometry > block = new osg::Geometry();
    m_sideStates.insert( std::make_pair( "Left", new osg::Geometry() ) );
    m_sideStates.insert( std::make_pair( "Near", new osg::Geometry() ) );
    m_sideStates.insert( std::make_pair( "Right", new osg::Geometry() ) );
    m_sideStates.insert( std::make_pair( "Far", new osg::Geometry() ) );

    osg::ref_ptr< osg::Vec3Array > blockVertices = new osg::Vec3Array();
    //Left
    blockVertices->push_back( osg::Vec3( -0.5f,  0.5f,  0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f,  0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f,  0.5f ) );
    //Near
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f,  0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f, -0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f, -0.5f,  0.5f ) );		
    //Right
    blockVertices->push_back( osg::Vec3( 0.5f, -0.5f,  0.5f ) );
    blockVertices->push_back( osg::Vec3( 0.5f, -0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( 0.5f,  0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( 0.5f,  0.5f,  0.5f ) );
    //Far
    blockVertices->push_back( osg::Vec3(  0.5f, 0.5f,  0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f, 0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, 0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, 0.5f,  0.5f ) );	
    //Top
    blockVertices->push_back( osg::Vec3( -0.5f,  0.5f, 0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f, -0.5f, 0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f,  0.5f, 0.5f ) );	
    //Bottom
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f,  0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f,  0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f, -0.5f, -0.5f ) );

    block->setVertexArray( blockVertices.get() );

    osg::ref_ptr< osg::Vec3Array > leftLineVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > nearLineVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > rightLineVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > farLineVertices = new osg::Vec3Array();
    //Left
    leftLineVertices->push_back( osg::Vec3( -0.5f,  0.5f, 0.5f ) );
    leftLineVertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
    m_sideStates[ "Left" ]->setVertexArray( leftLineVertices.get() );
    //Near
    nearLineVertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
    nearLineVertices->push_back( osg::Vec3(  0.5f, -0.5f, 0.5f ) );
    m_sideStates[ "Near" ]->setVertexArray( nearLineVertices.get() );
    //Right
    rightLineVertices->push_back( osg::Vec3( 0.5f, -0.5f, 0.5f ) );
    rightLineVertices->push_back( osg::Vec3( 0.5f,  0.5f, 0.5f ) );
    m_sideStates[ "Right" ]->setVertexArray( rightLineVertices.get() );
    //Far
    farLineVertices->push_back( osg::Vec3(  0.5f, 0.5f, 0.5f ) );
    farLineVertices->push_back( osg::Vec3( -0.5f, 0.5f, 0.5f ) );
    m_sideStates[ "Far" ]->setVertexArray( farLineVertices.get() );

	osg::ref_ptr< osg::Vec4Array > blockColor = new osg::Vec4Array();
	blockColor->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    block->setColorArray( blockColor.get() );
    block->setColorBinding( osg::Geometry::BIND_OVERALL );
	
	osg::ref_ptr< osg::Vec4Array > leftLineColor = new osg::Vec4Array();
    osg::ref_ptr< osg::Vec4Array > nearLineColor = new osg::Vec4Array();
    osg::ref_ptr< osg::Vec4Array > rightLineColor = new osg::Vec4Array();
    osg::ref_ptr< osg::Vec4Array > farLineColor = new osg::Vec4Array();

	leftLineColor->push_back( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );
    nearLineColor->push_back( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );
    rightLineColor->push_back( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );
    farLineColor->push_back( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );

    m_sideStates[ "Left" ]->setColorArray( leftLineColor.get() );
    m_sideStates[ "Near" ]->setColorArray( nearLineColor.get() );
    m_sideStates[ "Right" ]->setColorArray( rightLineColor.get() );
    m_sideStates[ "Far" ]->setColorArray( farLineColor.get() );

    osg::ref_ptr< osg::Vec3Array > blockNormals = new osg::Vec3Array();
    //Left
    blockNormals->push_back( osg::Vec3( -1.0f,  0.0f,  0.0f ) );
    //Near
    blockNormals->push_back( osg::Vec3(  0.0f, -1.0f,  0.0f ) );
    //Right
    blockNormals->push_back( osg::Vec3(  1.0f,  0.0f,  0.0f ) );
    //Far
    blockNormals->push_back( osg::Vec3(  0.0f,  1.0f,  0.0f ) );
    //Top
    blockNormals->push_back( osg::Vec3(  0.0f,  0.0f,  1.0f ) );
    //Bottom
    blockNormals->push_back( osg::Vec3(  0.0f,  0.0f, -1.0f ) );

    block->setNormalArray( blockNormals.get() );
    block->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    block->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, blockVertices.get()->size() ) );
    m_sideStates[ "Left" ]->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, leftLineVertices->size() ) );
    m_sideStates[ "Near" ]->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, nearLineVertices->size() ) );
    m_sideStates[ "Right" ]->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, rightLineVertices->size() ) );
    m_sideStates[ "Far" ]->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, farLineVertices->size() ) );

    osg::ref_ptr< osg::Vec2Array > texCoord = new osg::Vec2Array();
    //Left
    texCoord->push_back( osg::Vec2( 0, 1 ) );
    texCoord->push_back( osg::Vec2( 0, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 1 ) );
    //Near
    texCoord->push_back( osg::Vec2( 0, 1 ) );
    texCoord->push_back( osg::Vec2( 0, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 1 ) );
    //Right
    texCoord->push_back( osg::Vec2( 0, 1 ) );
    texCoord->push_back( osg::Vec2( 0, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 1 ) );
    //Far
    texCoord->push_back( osg::Vec2( 0, 1 ) );
    texCoord->push_back( osg::Vec2( 0, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 1 ) );
    //Top
    texCoord->push_back( osg::Vec2( 0, 1 ) );
    texCoord->push_back( osg::Vec2( 0, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 1 ) );
    //Bottom
    texCoord->push_back( osg::Vec2( 0, 1 ) );
    texCoord->push_back( osg::Vec2( 0, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 0 ) );
    texCoord->push_back( osg::Vec2( 1, 1 ) );

    block->setTexCoordArray( 0, texCoord.get() );

    osg::ref_ptr< osg::StateSet > blockStateSet = new osg::StateSet();
    osg::ref_ptr< osg::Texture2D > texture = new osg::Texture2D();
    texture->setImage( image.get() );
    blockStateSet->setTextureAttributeAndModes( 0, texture.get() );
    //block->setStateSet( blockStateSet.get() );

    osg::ref_ptr< osg::StateSet > lineStateSet = new osg::StateSet();
    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 2.0f );
    lineStateSet->setAttribute( lineWidth.get() );

    std::map< std::string, osg::ref_ptr< osg::Geometry > >::const_iterator itr;
    for( itr = m_sideStates.begin(); itr != m_sideStates.end(); ++itr )
    {
        itr->second->setStateSet( lineStateSet.get() );
    }
    
    addDrawable( block.get() );
}
////////////////////////////////////////////////////////////////////////////////
void Block::SetColor( float r, float g, float b, float a )
{
    osg::ref_ptr< osg::Vec4Array > colorArray = static_cast< osg::Vec4Array* >
        ( getDrawable( 0 )->asGeometry()->getColorArray() );

    if( colorArray.valid() )
    {
        colorArray->at( 0 ).r() = r;
        colorArray->at( 0 ).g() = g;
        colorArray->at( 0 ).b() = b;
        colorArray->at( 0 ).a() = a;
    }
}
////////////////////////////////////////////////////////////////////////////////
