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

using namespace Construction;

////////////////////////////////////////////////////////////////////////////////
AgentEntity::AgentEntity( Construction::Agent* agent,
                          ves::xplorer::scenegraph::DCS* pluginDCS,
                          ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
:
CADEntity( agent, pluginDCS, physicsSimulator ),
m_geometry( agent ),
m_pluginDCS( pluginDCS ),
m_targetDCS( 0 ),
m_constraint( 0 )
{
	//These need to be initialized last
    m_obstacleSensor = new Construction::ObstacleSensor( this );
    m_blockSensor = new Construction::BlockSensor( this );
    m_siteSensor = new Construction::SiteSensor( this );
    m_holdBlockSensor = new Construction::HoldBlockSensor( this );
}
////////////////////////////////////////////////////////////////////////////////
AgentEntity::~AgentEntity()
{
    if( m_constraint )
    {
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
    m_obstacleSensor->CalculateResultantForce();
    GetPhysicsRigidBody()->setLinearVelocity( m_obstacleSensor->GetResultantForce() );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::WanderAround()
{
    btVector3 velocity = GetPhysicsRigidBody()->getLinearVelocity();

    GetPhysicsRigidBody()->setLinearVelocity( velocity.normalize() );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::GoToBlock()
{
    GetPhysicsRigidBody()->setLinearVelocity( m_blockSensor->GetNormalizedBlockVector() );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::PickUpBlock( Construction::BlockEntity* blockEntity )
{
    double* position = GetDCS()->GetVETranslationArray();
    double transArray[ 3 ] = { position[ 0 ], position[ 1 ], 1.5 };
    blockEntity->GetDCS()->SetTranslationArray( transArray );
    m_targetDCS = NULL;
    blockEntity->GetPhysicsRigidBody()->clearForces();
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::Build()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::GoToSite()
{
    GetPhysicsRigidBody()->setLinearVelocity( m_siteSensor->GetNormalizedSiteVector() );;
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::SetNameAndDescriptions( int number )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );

    std::stringstream ss;
    ss << "Agent" << number;
    std::cout << ss.str() << std::endl;
    GetDCS()->setName( ss.str() );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::SetConstraints( int gridSize )
{
    btTransform trans;
    trans.setIdentity();
    trans.setOrigin( btVector3( 0, 0, 0.5 ) );

    //Must disable deactivation so constraint is always applied
    GetPhysicsRigidBody()->setActivationState( DISABLE_DEACTIVATION );
    btRigidBody* fixedBody = ves::xplorer::scenegraph::PhysicsSimulator::instance()->CreateRigidBody( 0, trans, 0 );

    btTransform frameInA, frameInB;
    frameInA = btTransform::getIdentity();
    frameInB = btTransform::getIdentity();

#if ( BULLET_MAJOR_VERSION >= 2 ) && ( BULLET_MINOR_VERSION > 63 )
    m_constraint = new btGeneric6DofConstraint( *GetPhysicsRigidBody(), *fixedBody, frameInA, frameInB, false );
#else
    m_constraint = new btGeneric6DofConstraint( *GetPhysicsRigidBody(), *fixedBody, frameInA, frameInB );
#endif

    //Fix the translation range for the agents
    //Give 0.5 units extra in xy plane for agent/wall collision
    //Give small z-range for agent/grid collision
    m_constraint->setLinearLowerLimit( btVector3( -gridSize * 0.5, -gridSize * 0.5, -0.1 ) );
    m_constraint->setLinearUpperLimit( btVector3( gridSize * 0.5, gridSize * 0.5, 0.0 ) );

    //Remove rotation from agents
    //Range should be small, otherwise singularities will 'explode' the constraint
    m_constraint->setAngularLowerLimit( btVector3( 0, 0, 0 ) );
    m_constraint->setAngularUpperLimit( btVector3( 0, 0, 0 ) );

    ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld()->addConstraint( m_constraint );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::SetTargetDCS( ves::xplorer::scenegraph::DCS* targetDCS )
{
    m_targetDCS = targetDCS;
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
Construction::ObstacleSensor* AgentEntity::GetObstacleSensor()
{
    return m_obstacleSensor;
}
////////////////////////////////////////////////////////////////////////////////
Construction::BlockSensor* AgentEntity::GetBlockSensor()
{
    return m_blockSensor;
}
////////////////////////////////////////////////////////////////////////////////
Construction::SiteSensor* AgentEntity::GetSiteSensor()
{
    return m_siteSensor;
}
////////////////////////////////////////////////////////////////////////////////
Construction::HoldBlockSensor* AgentEntity::GetHoldBlockSensor()
{
    return m_holdBlockSensor;
}
////////////////////////////////////////////////////////////////////////////////
