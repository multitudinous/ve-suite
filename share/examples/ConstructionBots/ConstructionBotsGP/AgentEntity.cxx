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
#include "AgentEntity.h"
#include "Agent.h"
#include "BlockEntity.h"
#include "ObstacleSensor.h"
#include "BlockSensor.h"
#include "SiteSensor.h"
#include "HoldBlockSensor.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

// --- Bullet Includes --- //
#include <BulletDynamics/ConstraintSolver/btGeneric6DofConstraint.h>
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

// --- C/C++ Libraries --- //
#include <sstream>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
AgentEntity::AgentEntity( osg::ref_ptr< bots::Agent > agent,
                          ves::xplorer::scenegraph::DCS* pluginDCS,
                          ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
:
CADEntity( agent.get(), pluginDCS, physicsSimulator ),
m_buildMode( false ),
m_geometry( agent.get() ),
m_pluginDCS( pluginDCS ),
m_targetDCS( 0 ),
m_constraint( 0 )
{
	//These need to be initialized last
    m_obstacleSensor = new bots::ObstacleSensor( this );
    m_blockSensor = new bots::BlockSensor( this );
    m_siteSensor = new bots::SiteSensor( this );
    m_holdBlockSensor = new bots::HoldBlockSensor( this );
}
////////////////////////////////////////////////////////////////////////////////
AgentEntity::~AgentEntity()
{
    if( m_constraint )
    {
        if( m_physicsSimulator )
        {
            m_physicsSimulator->GetDynamicsWorld()->removeConstraint( m_constraint );
        }
        delete m_constraint;
    }

    if( m_obstacleSensor )
    {
        delete m_obstacleSensor;
    }

    if( m_blockSensor )
    {
        delete m_blockSensor;
    }

    if( m_siteSensor )
    {
        delete m_siteSensor;
    }

    if( m_holdBlockSensor )
    {
        delete m_holdBlockSensor;
    }
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::AvoidObstacle()
{
    m_obstacleSensor->CalculateResultantForce( m_buildMode );
    m_physicsRigidBody->setLinearVelocity( m_obstacleSensor->GetResultantForce() );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::WanderAround()
{
    btVector3 velocity = m_physicsRigidBody->getLinearVelocity();

    m_physicsRigidBody->setLinearVelocity( velocity.normalize() );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::GoToBlock()
{
    m_physicsRigidBody->setLinearVelocity( m_blockSensor->GetNormalizedBlockVector() );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::PickUpBlock( bots::BlockEntity* blockEntity )
{
    double* position = m_dcs->GetVETranslationArray();
    double transArray[ 3 ] = { position[ 0 ], position[ 1 ], 1.5 };
    blockEntity->GetDCS()->SetTranslationArray( transArray );
    blockEntity->GetPhysicsRigidBody()->clearForces();
    m_targetDCS = NULL;
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::Build()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::GoToSite()
{
    m_physicsRigidBody->setLinearVelocity( m_siteSensor->GetNormalizedSiteVector() );;
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::SetNameAndDescriptions( int number )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    m_dcs->setDescriptions( descriptorsList );

    std::stringstream ss;
    ss << "Agent" << number;
    std::cout << ss.str() << std::endl;
    m_dcs->setName( ss.str() );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::SetConstraints( int gridSize )
{
    btTransform trans;
    trans.setIdentity();
    trans.setOrigin( btVector3( 0, 0, 0.5 ) );

    //Must disable deactivation so constraint is always applied
    m_physicsRigidBody->setActivationState( DISABLE_DEACTIVATION );
    btRigidBody* fixedBody = m_physicsSimulator->CreateRigidBody( 0, trans, 0 );

    btTransform frameInA, frameInB;
    frameInA = btTransform::getIdentity();
    frameInB = btTransform::getIdentity();

#if ( BULLET_MAJOR_VERSION >= 2 ) && ( BULLET_MINOR_VERSION > 61 )
    m_constraint = new btGeneric6DofConstraint( *m_physicsRigidBody, *fixedBody, frameInA, frameInB, false );
#else
    m_constraint = new btGeneric6DofConstraint( *m_physicsRigidBody, *fixedBody, frameInA, frameInB );
#endif

    //Fix the translation range for the agents
    //Give 0.5 units extra in xy plane for agent/wall collision
    //Give small z-range for agent/grid collision
    m_constraint->setLinearLowerLimit( btVector3( -gridSize * 0.5, -gridSize * 0.5, 0.0 ) );
    m_constraint->setLinearUpperLimit( btVector3( gridSize * 0.5, gridSize * 0.5, 0.1 ) );

    //Remove rotation from agents
    //Range should be small, otherwise singularities will 'explode' the constraint
    m_constraint->setAngularLowerLimit( btVector3( 0, 0, 0 ) );
    m_constraint->setAngularUpperLimit( btVector3( 0, 0, 0 ) );

    m_physicsSimulator->GetDynamicsWorld()->addConstraint( m_constraint );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::SetTargetDCS( ves::xplorer::scenegraph::DCS* targetDCS )
{
    m_targetDCS = targetDCS;
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::SetBuildMode( bool buildMode )
{
    m_buildMode = buildMode;
}
////////////////////////////////////////////////////////////////////////////////
bool AgentEntity::IsBuilding()
{
    return m_buildMode;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* AgentEntity::GetPluginDCS()
{
	return m_pluginDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* AgentEntity::GetTargetDCS()
{
	return m_targetDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
bots::ObstacleSensor* AgentEntity::GetObstacleSensor()
{
    return m_obstacleSensor;
}
////////////////////////////////////////////////////////////////////////////////
bots::BlockSensor* AgentEntity::GetBlockSensor()
{
    return m_blockSensor;
}
////////////////////////////////////////////////////////////////////////////////
bots::SiteSensor* AgentEntity::GetSiteSensor()
{
    return m_siteSensor;
}
////////////////////////////////////////////////////////////////////////////////
bots::HoldBlockSensor* AgentEntity::GetHoldBlockSensor()
{
    return m_holdBlockSensor;
}
////////////////////////////////////////////////////////////////////////////////
