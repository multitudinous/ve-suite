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
// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/vesMotionState.h>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDiscreteDynamicsWorld.h>

#include <BulletCollision/CollisionShapes/btCompoundShape.h>
#include <BulletCollision/CollisionShapes/btCollisionShape.h>
#include <BulletCollision/CollisionShapes/btBoxShape.h>
#include <BulletCollision/CollisionShapes/btSphereShape.h>
#include <BulletCollision/CollisionShapes/btTriangleMesh.h>
#include <BulletCollision/CollisionShapes/btBvhTriangleMeshShape.h>
#include <BulletCollision/CollisionShapes/btConvexTriangleMeshShape.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
PhysicsRigidBody::PhysicsRigidBody( osg::Node* node,
                                    PhysicsSimulator* physicsSimulator )
        :
        m_storeCollisions( false ),
        m_mass( 1.0 ),
        mPhysicsSimulator( physicsSimulator ),
        m_osgToBullet( new osgToBullet( node ) ),
#if ( BULLET_MAJOR_VERSION >= 2 ) && ( BULLET_MINOR_VERSION > 65 )
        btRigidBody( btRigidBody::btRigidBodyConstructionInfo(
                     btScalar( m_mass ),                         //mass
                     m_vesMotionState = new vesMotionState(),    //motionState
                     0,                                          //collisionShape
                     btVector3( 0.0f, 0.0f, 0.0f ) ) )           //localInertia
#else
        btRigidBody( btScalar( m_mass ),                         //mass
                     m_vesMotionState = new vesMotionState(),    //motionState
                     0,                                          //collisionShape
                     btVector3( 0.0f, 0.0f, 0.0f ),              //localInertia
                     btScalar( 0.0f ),                           //linearDamping
                     btScalar( 0.0f ),                           //angularDamping
                     btScalar( 0.5f ),                           //friction
                     btScalar( 0.0f ) )                          //restitution
#endif
{
    BoundingBoxShape();
}
////////////////////////////////////////////////////////////////////////////////
PhysicsRigidBody::~PhysicsRigidBody()
{
    ClearCollisions();

    delete m_vesMotionState;
    delete m_collisionShape;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetMass( float mass )
{
    m_mass = mass;
    SetMassProps();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetStoreCollisions( bool storeCollisions )
{
    if( storeCollisions == false )
    {
        ClearCollisions();
    }

    m_storeCollisions = storeCollisions;
}
////////////////////////////////////////////////////////////////////////////////
bool PhysicsRigidBody::IsStoringCollisions()
{
    return m_storeCollisions;
}
////////////////////////////////////////////////////////////////////////////////
bool PhysicsRigidBody::CollisionInquiry( PhysicsRigidBody* physicsRigidBody )
{
    int count = m_collisions.count( physicsRigidBody );
    if( count > 0 )
    {
        return true;
    }
    else
    {
        return false;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetMassProps()
{
    if( m_collisionShape )
    {
        //btRigidBody* is dynamic if and only if mass is non zero, otherwise static
        bool dynamic = ( m_mass != 0.0f );

        btVector3 localInertia( 0, 0, 0 );
        if( dynamic )
        {
            m_collisionShape->calculateLocalInertia( m_mass, localInertia );
        }

        setMassProps( m_mass, localInertia );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::PushBackCollision( PhysicsRigidBody* physicsRigidBody, btVector3 location )
{
    m_collisions.insert( std::make_pair( physicsRigidBody, location ) );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::ClearCollisions()
{
    m_collisions.clear();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::BoundingBoxShape()
{
    if( this )
    {
        mPhysicsSimulator->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    osg::BoundingBox bb = m_osgToBullet->GetBoundingBox();
    m_collisionShape = new btBoxShape(
                           btVector3( ( bb.xMax() - bb.xMin() ) * 0.5f,
                                      ( bb.yMax() - bb.yMin() ) * 0.5f,
                                      ( bb.zMax() - bb.zMin() ) * 0.5f ) );

    SetMassProps();

    if( m_mass != 0 )
    {
        setActivationState( DISABLE_DEACTIVATION );
    }

    mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( this );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SphereShape( double radius )
{
    if( this )
    {
        mPhysicsSimulator->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    if( radius == 0 )
    {
        m_collisionShape = new btSphereShape( m_osgToBullet->GetBoundingSphere().radius() );
    }
    else
    {
        m_collisionShape = new btSphereShape( radius );
    }

    SetMassProps();

    if( m_mass != 0 )
    {
        setActivationState( DISABLE_DEACTIVATION );
    }

    mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( this );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::UserDefinedShape( btCollisionShape* collisionShape )
{
    if( this )
    {
        mPhysicsSimulator->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    m_collisionShape = collisionShape;

    SetMassProps();

    if( m_mass != 0 )
    {
        setActivationState( DISABLE_DEACTIVATION );
    }

    mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( this );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::StaticConcaveShape()
{
    if( this )
    {
        mPhysicsSimulator->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    m_collisionShape = new btBvhTriangleMeshShape( m_osgToBullet->GetTriangleMesh(), false );

    SetMass( 0 );

    mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( this );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::ConvexShape()
{
    if( this )
    {
        mPhysicsSimulator->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    m_collisionShape = new btConvexTriangleMeshShape( m_osgToBullet->GetTriangleMesh() );

    SetMassProps();

    if( m_mass != 0 )
    {
        setActivationState( DISABLE_DEACTIVATION );
    }

    mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( this );
}
////////////////////////////////////////////////////////////////////////////////
