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
#include <string>

using namespace Construction;

////////////////////////////////////////////////////////////////////////////////
Block::Block()
:
m_isAttached( false )
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
        m_isAttached = block.m_isAttached;
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
        exit( 0 );
	}

    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    osg::ref_ptr< osg::Geometry > block = new osg::Geometry();
    osg::ref_ptr< osg::Geometry > lines = new osg::Geometry();

    osg::ref_ptr< osg::Vec3Array > blockVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > lineVertices = new osg::Vec3Array();

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

    lineVertices->push_back( osg::Vec3( -0.5f,  0.5f, 0.5f ) );
    lineVertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );

    lineVertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
    lineVertices->push_back( osg::Vec3(  0.5f, -0.5f, 0.5f ) );

    lineVertices->push_back( osg::Vec3( 0.5f, -0.5f, 0.5f ) );
    lineVertices->push_back( osg::Vec3( 0.5f,  0.5f, 0.5f ) );

    lineVertices->push_back( osg::Vec3(  0.5f, 0.5f, 0.5f ) );
    lineVertices->push_back( osg::Vec3( -0.5f, 0.5f, 0.5f ) );

    block->setVertexArray( blockVertices.get() );
    lines->setVertexArray( lineVertices.get() );

	osg::ref_ptr< osg::Vec4Array > blockColor = new osg::Vec4Array();
	blockColor->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    block->setColorArray( blockColor.get() );
    block->setColorBinding( osg::Geometry::BIND_OVERALL );
	
	osg::ref_ptr< osg::Vec4Array > lineColor = new osg::Vec4Array();
	for( int i = 0; i < 4; ++i )
	{
	    lineColor->push_back( osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );
	}

    lines->setColorArray( lineColor.get() );
    lines->setColorBinding( osg::Geometry::BIND_PER_PRIMITIVE );

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

    osg::ref_ptr< osg::Vec3Array > lineNormals = new osg::Vec3Array();
    lineNormals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
    lines->setNormalArray( lineNormals.get() );
    lines->setNormalBinding( osg::Geometry::BIND_OVERALL );

    block->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, blockVertices.get()->size() ) );
    lines->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, lineVertices.get()->size() ) );

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
    lines->setStateSet( lineStateSet.get() );

    addDrawable( block.get() );
    addDrawable( lines.get() );
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
