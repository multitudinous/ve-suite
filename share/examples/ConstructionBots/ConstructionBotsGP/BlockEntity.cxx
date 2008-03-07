// --- My Includes --- //
#include "BlockEntity.h"
#include "Block.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/LineWidth>

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>

// --- Bullet Includes --- //
#include <BulletDynamics/ConstraintSolver/btGeneric6DofConstraint.h>
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

// --- C/C++ Libraries --- //
#include <sstream>

using namespace Construction;

////////////////////////////////////////////////////////////////////////////////
BlockEntity::BlockEntity( osg::ref_ptr< Construction::Block > block,
                          ves::xplorer::scenegraph::DCS* pluginDCS,
                          ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
:
CADEntity( block.get(), pluginDCS, physicsSimulator ),
m_block( block.get() ),
m_constraint( 0 ),
m_location( 0, 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
BlockEntity::~BlockEntity()
{
    if( m_constraint )
    {
        if( m_physicsSimulator )
        {
            m_physicsSimulator->GetDynamicsWorld()->removeConstraint( m_constraint );
        }
        delete m_constraint;
    }
}
////////////////////////////////////////////////////////////////////////////////
Construction::Block* BlockEntity::GetGeometry()
{
    return m_block.get();
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::pair< int, int >, bool > BlockEntity::GetOccMatrix()
{
	return m_occMatrix;
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetNameAndDescriptions( int number )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    m_dcs->setDescriptions( descriptorsList );

    std::stringstream ss;
    ss << "Block" << number;
    std::cout << ss.str() << std::endl;
    m_dcs->setName( ss.str() );
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetConstraints( int gridSize )
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
    m_constraint->setLinearLowerLimit( btVector3( -gridSize * 0.5, -gridSize * 0.5, -5.0 ) );
    m_constraint->setLinearUpperLimit( btVector3( gridSize * 0.5, gridSize * 0.5, 5.0 ) );

    //Remove rotation from agents
    //Range should be small, otherwise singularities will 'explode' the constraint
    m_constraint->setAngularLowerLimit( btVector3( 0, 0, 0 ) );
    m_constraint->setAngularUpperLimit( btVector3( 0, 0, 0 ) );

    m_physicsSimulator->GetDynamicsWorld()->addConstraint( m_constraint );
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetOccMatrix( std::map< std::pair< int, int >, bool > occMatrix )
{
    m_occMatrix = occMatrix;
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::UpdateSideStates()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
