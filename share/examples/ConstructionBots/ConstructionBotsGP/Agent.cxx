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
#include "Agent.h"

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osgDB/ReadFile>

// --- C/C++ Includes --- //
#include <iostream>

using namespace bots;

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
	}

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
	agentColor->push_back( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );
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
