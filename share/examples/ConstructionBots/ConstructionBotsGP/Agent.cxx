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

#include <osgDB/ReadFile>

// --- Bullet Includes --- //
#include <BulletCollision/CollisionShapes/btBoxShape.h>
#include <BulletCollision/CollisionShapes/btCompoundShape.h>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
Agent::Agent()
{
    Initialize();
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
btCompoundShape* Agent::CreateCompoundShape()
{
    btScalar boxHalfWidth = 0.5;
    btScalar holderHalfThickness = 0.1;
    btScalar offset = boxHalfWidth + holderHalfThickness;

    btCompoundShape* compoundShape = new btCompoundShape();
    btBoxShape* mainBox = new btBoxShape(
        btVector3( boxHalfWidth, boxHalfWidth, boxHalfWidth ) );
    btBoxShape* leftHolderBox = new btBoxShape(
        btVector3( holderHalfThickness, boxHalfWidth, holderHalfThickness ) );
    btBoxShape* nearHolderBox = new btBoxShape(
        btVector3( boxHalfWidth, holderHalfThickness, holderHalfThickness ) );
    btBoxShape* rightHolderBox = new btBoxShape(
        btVector3( holderHalfThickness, boxHalfWidth, holderHalfThickness ) );
    btBoxShape* farHolderBox = new btBoxShape(
        btVector3( boxHalfWidth, holderHalfThickness, holderHalfThickness ) );

    btTransform transform;
    transform.setIdentity();
    compoundShape->addChildShape( transform, mainBox );
    transform.setOrigin( btVector3( -offset, 0.0, offset ) );
    compoundShape->addChildShape( transform, leftHolderBox );
    transform.setOrigin( btVector3( 0.0, -offset, offset ) );
    compoundShape->addChildShape( transform, nearHolderBox );
    transform.setOrigin( btVector3( offset, 0.0, offset ) );
    compoundShape->addChildShape( transform, rightHolderBox );
    transform.setOrigin( btVector3( 0.0, offset, offset ) );
    compoundShape->addChildShape( transform, farHolderBox );

    return compoundShape;
}
////////////////////////////////////////////////////////////////////////////////
void Agent::Initialize()
{
    double delta = 0.05;
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    setStateSet( stateset.get() );

    osg::ref_ptr< osg::Geometry > agent = new osg::Geometry();

    osg::ref_ptr< osg::Vec3Array > agentVertices = new osg::Vec3Array();
    //Left
    agentVertices->push_back( osg::Vec3( -0.5,  0.5,  0.5 ) );
    agentVertices->push_back( osg::Vec3( -0.5,  0.5, -0.5 ) );
    agentVertices->push_back( osg::Vec3( -0.5, -0.5, -0.5 ) );
    agentVertices->push_back( osg::Vec3( -0.5, -0.5,  0.5 ) );
    //Near
    agentVertices->push_back( osg::Vec3( -0.5, -0.5,  0.5 ) );
    agentVertices->push_back( osg::Vec3( -0.5, -0.5, -0.5 ) );
    agentVertices->push_back( osg::Vec3(  0.5, -0.5, -0.5 ) );
    agentVertices->push_back( osg::Vec3(  0.5, -0.5,  0.5 ) );
    //Right
    agentVertices->push_back( osg::Vec3( 0.5, -0.5,  0.5 ) );
    agentVertices->push_back( osg::Vec3( 0.5, -0.5, -0.5 ) );
    agentVertices->push_back( osg::Vec3( 0.5,  0.5, -0.5 ) );
    agentVertices->push_back( osg::Vec3( 0.5,  0.5,  0.5 ) );
    //Far
    agentVertices->push_back( osg::Vec3(  0.5, 0.5,  0.5 ) );
    agentVertices->push_back( osg::Vec3(  0.5, 0.5, -0.5 ) );
    agentVertices->push_back( osg::Vec3( -0.5, 0.5, -0.5 ) );
    agentVertices->push_back( osg::Vec3( -0.5, 0.5,  0.5 ) );
    //Top
    agentVertices->push_back( osg::Vec3( -0.5,  0.5, 0.5 ) );
    agentVertices->push_back( osg::Vec3( -0.5, -0.5, 0.5 ) );
    agentVertices->push_back( osg::Vec3(  0.5, -0.5, 0.5 ) );
    agentVertices->push_back( osg::Vec3(  0.5,  0.5, 0.5 ) );
    //Bottom
    agentVertices->push_back( osg::Vec3( -0.5, -0.5, -0.5 ) );
    agentVertices->push_back( osg::Vec3( -0.5,  0.5, -0.5 ) );
    agentVertices->push_back( osg::Vec3(  0.5,  0.5, -0.5 ) );
    agentVertices->push_back( osg::Vec3(  0.5, -0.5, -0.5 ) );
    agent->setVertexArray( agentVertices.get() );

	osg::ref_ptr< osg::Vec4Array > agentColor = new osg::Vec4Array();
	agentColor->push_back( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );
    agent->setColorArray( agentColor.get() );
    agent->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array > agentNormals = new osg::Vec3Array();
    //Left
    agentNormals->push_back( osg::Vec3( -1.0,  0.0,  0.0 ) );
    //Near
    agentNormals->push_back( osg::Vec3(  0.0, -1.0,  0.0 ) );
    //Right
    agentNormals->push_back( osg::Vec3(  1.0,  0.0,  0.0 ) );
    //Far
    agentNormals->push_back( osg::Vec3(  0.0,  1.0,  0.0 ) );
    //Top
    agentNormals->push_back( osg::Vec3(  0.0,  0.0,  1.0 ) );
    //Bottom
    agentNormals->push_back( osg::Vec3(  0.0,  0.0, -1.0 ) );
    agent->setNormalArray( agentNormals.get() );
    agent->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    agent->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, agentVertices.get()->size() ) );
    addDrawable( agent.get() );
}
////////////////////////////////////////////////////////////////////////////////
