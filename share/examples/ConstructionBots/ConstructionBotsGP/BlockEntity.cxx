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
m_geometry( block.get() ),
m_constraint( 0 )
//start( false )
{
	//location[0] = location[1] = location[2] = 0.0f;
	//sideState[0] = sideState[1] = sideState[2] = sideState[3] = false;

    //set the uuid on the osg node so that we can get back to vexml
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
    return m_geometry.get();
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

#if ( BULLET_MAJOR_VERSION >= 2 ) && ( BULLET_MINOR_VERSION > 63 )
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
/*
void BlockEntity::SetOccMatrix( std::vector< bool > temp )
{
	for( int i=0; i < (int)temp.size(); i++ )
	{
		occMatrix.push_back( temp.at( i ) );
	}
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetLocation( osg::Vec3 temp )
{
	location.set( temp );
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetSideStates( bool* temp )
{
	for(int i = 0; i < 4; i++ )
	{
		sideState[i] = temp[i];
	}
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetStartBlock( bool temp )
{
   start = temp;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::CADEntity* BlockEntity::GetEntity()
{
	return entity;
}
////////////////////////////////////////////////////////////////////////////////
std::vector< bool > BlockEntity::GetOccMatrix()
{
	return occMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Vec3 BlockEntity::GetLocation()
{
	return location;
}
////////////////////////////////////////////////////////////////////////////////
bool* BlockEntity::GetSideStates()
{
	return sideState;
}
////////////////////////////////////////////////////////////////////////////////
*/
