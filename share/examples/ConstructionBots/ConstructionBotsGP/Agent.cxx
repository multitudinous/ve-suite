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
    if( &agent != this )
    {

    }
}
////////////////////////////////////////////////////////////////////////////////
Agent::~Agent()
{

}
////////////////////////////////////////////////////////////////////////////////
void Agent::CreateAgent()
{
	osg::ref_ptr< osg::Image > image = osgDB::readImageFile( "Textures/tile.jpg" );

	if( !image )
	{
        std::cout << "Invalid texture file!" << std::endl;
        exit( 0 );
	}

    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    osg::ref_ptr< osg::Geometry > agent = new osg::Geometry();
    osg::ref_ptr< osg::Geometry > lines = new osg::Geometry();

    osg::ref_ptr< osg::Vec3Array > agentVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > lineVertices = new osg::Vec3Array();

    //Left
    agentVertices->push_back( osg::Vec3( -0.5f,  0.5f,  0.5f ) );
    agentVertices->push_back( osg::Vec3( -0.5f,  0.5f, -0.5f ) );
    agentVertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    agentVertices->push_back( osg::Vec3( -0.5f, -0.5f,  0.5f ) );
    //Near
    agentVertices->push_back( osg::Vec3( -0.5f, -0.5f,  0.5f ) );
    agentVertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    agentVertices->push_back( osg::Vec3(  0.5f, -0.5f, -0.5f ) );
    agentVertices->push_back( osg::Vec3(  0.5f, -0.5f,  0.5f ) );	
    //Right
    agentVertices->push_back( osg::Vec3( 0.5f, -0.5f,  0.5f ) );
    agentVertices->push_back( osg::Vec3( 0.5f, -0.5f, -0.5f ) );
    agentVertices->push_back( osg::Vec3( 0.5f,  0.5f, -0.5f ) );
    agentVertices->push_back( osg::Vec3( 0.5f,  0.5f,  0.5f ) );
    //Far
    agentVertices->push_back( osg::Vec3(  0.5f, 0.5f,  0.5f ) );
    agentVertices->push_back( osg::Vec3(  0.5f, 0.5f, -0.5f ) );
    agentVertices->push_back( osg::Vec3( -0.5f, 0.5f, -0.5f ) );
    agentVertices->push_back( osg::Vec3( -0.5f, 0.5f,  0.5f ) );	
    //Top
    agentVertices->push_back( osg::Vec3( -0.5f,  0.5f, 0.5f ) );
    agentVertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
    agentVertices->push_back( osg::Vec3(  0.5f, -0.5f, 0.5f ) );
    agentVertices->push_back( osg::Vec3(  0.5f,  0.5f, 0.5f ) );
    //Bottom
    agentVertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    agentVertices->push_back( osg::Vec3( -0.5f,  0.5f, -0.5f ) );
    agentVertices->push_back( osg::Vec3(  0.5f,  0.5f, -0.5f ) );
    agentVertices->push_back( osg::Vec3(  0.5f, -0.5f, -0.5f ) );

    agent->setVertexArray( agentVertices.get() );

	osg::ref_ptr< osg::Vec4Array > agentColor = new osg::Vec4Array();
	agentColor->push_back( osg::Vec4( 0.9, 0.8, 0.5, 1.0 ) );
    agent->setColorArray( agentColor.get() );
    agent->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array > agentNormals = new osg::Vec3Array();
    agentNormals->push_back( osg::Vec3( -1.0f,  0.0f,  0.0f ) );					//Left
    agentNormals->push_back( osg::Vec3(  0.0f, -1.0f,  0.0f ) );					//Near
    agentNormals->push_back( osg::Vec3(  1.0f,  0.0f,  0.0f ) );					//Right
    agentNormals->push_back( osg::Vec3(  0.0f,  1.0f,  0.0f ) );					//Far
    agentNormals->push_back( osg::Vec3(  0.0f,  0.0f,  1.0f ) );					//Top
    agentNormals->push_back( osg::Vec3(  0.0f,  0.0f, -1.0f ) );					//Bottom
    agent->setNormalArray( agentNormals.get() );
    agent->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    agent->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, agentVertices.get()->size() ) );

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

    agent->setTexCoordArray( 0, texCoord.get() );

    osg::ref_ptr< osg::StateSet > agentStateSet = new osg::StateSet();
    osg::ref_ptr< osg::Texture2D > texture = new osg::Texture2D();
    texture->setImage( image.get() );
    agentStateSet->setTextureAttributeAndModes( 0, texture.get() ,osg::StateAttribute::ON );
    //agent->setStateSet( agentStateSet.get() );

    addDrawable( agent.get() );
}
////////////////////////////////////////////////////////////////////////////////
