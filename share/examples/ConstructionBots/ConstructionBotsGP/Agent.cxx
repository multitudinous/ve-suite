// --- My Includes --- //
#include "Agent.h"

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osgDB/ReadFile>

// --- C/C++ Includes --- //
#include <iostream>

using namespace Construction;

////////////////////////////////////////////////////////////////////////////////
Agent::Agent()
{
    CreateAgent();
}
////////////////////////////////////////////////////////////////////////////////
Agent::Agent( const Agent& agent, const osg::CopyOp& copyop )
:
osg::Geode( agent, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Agent::~Agent()
{

}
////////////////////////////////////////////////////////////////////////////////
void Agent::CreateAgent()
{
    //addChild( osgDB::readNodeFile( "Models/Agent.obj" ) );

    int blockScale = 1;

	osg::ref_ptr< osg::Image > image;

	//if( start )
	//{
		//image = osgDB::readImageFile( "Textures/tile2.jpg" );
	//}

	//else
	//{
		image = osgDB::readImageFile( "Textures/tile.jpg" );
	//}

	if( !image )
	{
        std::cout << "Invalid texture file!" << std::endl;
        std::cout << std::endl;
        exit( 0 );
	}

    osg::ref_ptr< osg::Geode > geode = new osg::Geode;

    osg::ref_ptr< osg::Geometry > block = new osg::Geometry;
    osg::ref_ptr< osg::Geometry > lines = new osg::Geometry;

    osg::ref_ptr< osg::Vec3Array > blockVertices = new osg::Vec3Array;
    osg::ref_ptr< osg::Vec3Array > lineVertices = new osg::Vec3Array;

    //Left
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, blockScale * 0.5f, blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, blockScale * 0.5f, -blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, -blockScale * 0.5f, -blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, -blockScale * 0.5f, blockScale * 0.5f ) );

    //Near
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, -blockScale * 0.5f, blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, -blockScale * 0.5f, -blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, -blockScale * 0.5f, -blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, -blockScale * 0.5f, blockScale * 0.5f ) );
    		
    //Right
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, -blockScale * 0.5f, blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, -blockScale * 0.5f, -blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, blockScale * 0.5f, -blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, blockScale * 0.5f, blockScale * 0.5f ) );

    //Far
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, blockScale * 0.5f, blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, blockScale * 0.5f, -blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, blockScale * 0.5f, -blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, blockScale * 0.5f, blockScale * 0.5f ) );
    		
    //Top
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, blockScale * 0.5f, blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, -blockScale * 0.5f, blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, -blockScale * 0.5f, blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, blockScale * 0.5f, blockScale * 0.5f ) );
    		
    //Bottom
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, -blockScale * 0.5f, -blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( -blockScale * 0.5f, blockScale * 0.5f, -blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, blockScale * 0.5f, -blockScale * 0.5f ) );
    blockVertices->push_back( osg::Vec3( blockScale * 0.5f, -blockScale * 0.5f, -blockScale * 0.5f ) );


    block->setVertexArray( blockVertices.get() );

	osg::ref_ptr< osg::Vec4Array > blockColor = new osg::Vec4Array;
	blockColor->push_back( osg::Vec4( 0.5, 0.5, 0.5, 1.0 ) );
    block->setColorArray( blockColor.get() );
    block->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array > blockNormals = new osg::Vec3Array;
    blockNormals->push_back( osg::Vec3( -1.0f, 0.0f, 0.0f ) );					//Left
    blockNormals->push_back( osg::Vec3( 0.0f, -1.0f, 0.0f ) );					//Near
    blockNormals->push_back( osg::Vec3( 1.0f, 0.0f, 0.0f ) );					//Right
    blockNormals->push_back( osg::Vec3( 0.0f, 1.0f, 0.0f ) );					//Far
    blockNormals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );					//Top
    blockNormals->push_back( osg::Vec3( 0.0f, 0.0f, -1.0f ) );					//Bottom
    block->setNormalArray( blockNormals.get() );
    block->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    block->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, blockVertices.get()->size() ) );

    osg::ref_ptr< osg::Vec2Array > texCoord = new osg::Vec2Array;
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

    osg::ref_ptr< osg::StateSet > blockStateSet = new osg::StateSet;
    osg::ref_ptr< osg::Texture2D > texture = new osg::Texture2D;
    texture->setImage( image.get() );
    blockStateSet->setTextureAttributeAndModes( 0, texture.get() ,osg::StateAttribute::ON );
    //block->setStateSet( blockStateSet.get() );

    addDrawable( block.get() );
}
////////////////////////////////////////////////////////////////////////////////
