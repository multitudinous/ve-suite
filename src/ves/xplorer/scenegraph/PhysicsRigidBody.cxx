/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/vesMotionState.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/TriangleIndexFunctor>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDiscreteDynamicsWorld.h>
#include <BulletDynamics/Dynamics/btRigidBody.h>

#include <BulletCollision/CollisionShapes/btCompoundShape.h>
#include <BulletCollision/CollisionShapes/btCollisionShape.h>
#include <BulletCollision/CollisionShapes/btBoxShape.h>
#include <BulletCollision/CollisionShapes/btSphereShape.h>
#include <BulletCollision/CollisionShapes/btTriangleMesh.h>
#include <BulletCollision/CollisionShapes/btBvhTriangleMeshShape.h>
#include <BulletCollision/CollisionShapes/btConvexTriangleMeshShape.h>

// --- C/C++ Libraries --- //
#include <iostream>

class TriIndexFunc
{
public:
    TriIndexFunc(){;}
    ~TriIndexFunc(){;}

    void inline operator()( unsigned int pos1, unsigned int pos2, unsigned int pos3 )
    {
        m_triangleIndex.push_back( pos1 );
        m_triangleIndex.push_back( pos2 );
        m_triangleIndex.push_back( pos3 );
    }

    std::vector< unsigned int > m_triangleIndex;
};

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
PhysicsRigidBody::PhysicsRigidBody( osg::Node* node )
:
m_traversed( false ),
m_numVertices( 0 ),
m_mass( 1.0 ),
m_friction( 0.5 ),
m_restitution( 0.0 ),
m_rigidBody( 0 ),
m_vesMotionState( new vesMotionState() ),
m_compoundShape( 0 ),
m_collisionShape( 0 ),
m_triangleMesh( 0 ),
NodeVisitor( TRAVERSE_ALL_CHILDREN )
{
    node->accept( *this );

    BoundingBoxShape();
}
////////////////////////////////////////////////////////////////////////////////
PhysicsRigidBody::~PhysicsRigidBody()
{
    if( m_rigidBody )
    {
        delete m_rigidBody;
    }

    if( m_vesMotionState )
    {
        delete m_vesMotionState;
    }

    if( m_compoundShape )
    {
        delete m_compoundShape;
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
    }

    if( m_triangleMesh )
    {
        delete m_triangleMesh;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::apply( osg::Geode& geode )
{
    m_triangleMesh = new btTriangleMesh();

    for( size_t i = 0; i < geode.getNumDrawables(); ++i )
    {
        osg::TriangleIndexFunctor< TriIndexFunc > tif;
        osg::ref_ptr< osg::Drawable > drawable = geode.getDrawable( i );
        drawable->accept( tif );
        m_boundingBox.expandBy( drawable->getBound() );
        m_boundingSphere.expandBy( drawable->getBound() );

        osg::ref_ptr< osg::Vec3Array > vertexArray;
        vertexArray = static_cast< osg::Vec3Array* >( drawable->asGeometry()->getVertexArray() );

        for( size_t j = 0; j < tif.m_triangleIndex.size() / 3; ++j )
        {
            unsigned int index1, index2, index3;
            index1 = tif.m_triangleIndex.at( j * 3     );
            index2 = tif.m_triangleIndex.at( j * 3 + 1 );
            index3 = tif.m_triangleIndex.at( j * 3 + 2 );

            osg::Vec3d point1, point2, point3;
            point1 = vertexArray->at( index1 );
            point2 = vertexArray->at( index2 );
            point3 = vertexArray->at( index3 );

            m_triangleMesh->addTriangle( btVector3( point1.x(), point1.y(), point1.z() ),
                                         btVector3( point2.x(), point2.y(), point2.z() ),
                                         btVector3( point3.x(), point3.y(), point3.z() ) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
btRigidBody* PhysicsRigidBody::GetRigidBody()
{
    return m_rigidBody;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetMass( float mass )
{
    m_mass = mass;
    SetMassProps();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetFriction( float friction )
{
    m_friction = friction;
    m_rigidBody->setFriction( m_friction );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetRestitution( float restitution )
{
    m_restitution = restitution;
    m_rigidBody->setRestitution( m_restitution );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetMassProps()
{
    if( m_compoundShape && m_collisionShape )
    {
        //btRigidBody* is dynamic if and only if mass is non zero, otherwise static
        bool dynamic = ( m_mass != 0.0f );

        btVector3 localInertia( 0, 0, 0 );
        if( dynamic )
        {
            m_compoundShape->calculateLocalInertia( m_mass, localInertia );
        }

        m_rigidBody->setMassProps( m_mass, localInertia );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::BoundingBoxShape()
{
    if( m_rigidBody )
    {
        ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld()->removeRigidBody( m_rigidBody );
        delete m_rigidBody;
        m_rigidBody = 0;
    }

    if( m_compoundShape )
    {
        delete m_compoundShape;
        m_compoundShape = 0;
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    m_compoundShape = new btCompoundShape();
    m_collisionShape = new btBoxShape( btVector3( ( m_boundingBox.xMax() - m_boundingBox.xMin() ) * 0.5f,
                                                  ( m_boundingBox.yMax() - m_boundingBox.yMin() ) * 0.5f,
                                                  ( m_boundingBox.zMax() - m_boundingBox.zMin() ) * 0.5f ) );

    m_compoundShape->addChildShape( btTransform::getIdentity(), m_collisionShape );

    btVector3 localInertia( 0, 0, 0 );
    if( m_mass != 0 )
    {
        m_compoundShape->calculateLocalInertia( m_mass, localInertia );
    }

    m_rigidBody = new btRigidBody( m_mass,
                                   m_vesMotionState,
                                   m_compoundShape,
                                   localInertia );

    if( m_mass != 0 )
    {
        m_rigidBody->setActivationState( DISABLE_DEACTIVATION );
    }

    ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld()->addRigidBody( m_rigidBody );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SphereShape( double radius )
{
    if( m_rigidBody )
    {
        ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld()->removeRigidBody( m_rigidBody );
        delete m_rigidBody;
        m_rigidBody = 0;
    }

    if( m_compoundShape )
    {
        delete m_compoundShape;
        m_compoundShape = 0;
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    m_compoundShape = new btCompoundShape();
    if( radius == 0 )
    {
        m_collisionShape = new btSphereShape( m_boundingSphere.radius() );
    }
    else
    {
        m_collisionShape = new btSphereShape( radius );
    }

    m_compoundShape->addChildShape( btTransform::getIdentity(), m_collisionShape );

    btVector3 localInertia( 0, 0, 0 );
    if( m_mass != 0 )
    {
        m_compoundShape->calculateLocalInertia( m_mass, localInertia );
    }

    m_rigidBody = new btRigidBody( m_mass,
                                   m_vesMotionState,
                                   m_compoundShape,
                                   localInertia );

    if( m_mass != 0 )
    {
        m_rigidBody->setActivationState( DISABLE_DEACTIVATION );
    }

    ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld()->addRigidBody( m_rigidBody );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::StaticConcaveShape()
{
    if( m_rigidBody )
    {
        ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld()->removeRigidBody( m_rigidBody );
        delete m_rigidBody;
        m_rigidBody = 0;
    }

    if( m_compoundShape )
    {
        delete m_compoundShape;
        m_compoundShape = 0;
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    m_collisionShape = new btBvhTriangleMeshShape( m_triangleMesh, false );

    btVector3 localInertia( 0, 0, 0 );

    m_rigidBody = new btRigidBody( 0,
                                   m_vesMotionState,
                                   m_collisionShape,
                                   localInertia );

    ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld()->addRigidBody( m_rigidBody );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::ConvexShape()
{
    if( m_rigidBody )
    {
        ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld()->removeRigidBody( m_rigidBody );
        delete m_rigidBody;
        m_rigidBody = 0;
    }

    if( m_compoundShape )
    {
        delete m_compoundShape;
        m_compoundShape = 0;
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    m_compoundShape = new btCompoundShape();
    m_collisionShape = new btConvexTriangleMeshShape( m_triangleMesh );
    btTransform comOffset = btTransform::getIdentity();
    osg::Vec3d center = m_boundingBox.center();
    comOffset.setOrigin( btVector3( -center.x(), -center.y(), -center.z() ) );
    m_compoundShape->addChildShape( comOffset, m_collisionShape );

    btVector3 localInertia( 0, 0, 0 );
    if( m_mass != 0 )
    {
        m_compoundShape->calculateLocalInertia( m_mass, localInertia );
    }

    m_rigidBody = new btRigidBody( m_mass,
                                   m_vesMotionState,
                                   m_compoundShape,
                                   localInertia );

    if( m_mass != 0 )
    {
        m_rigidBody->setActivationState( DISABLE_DEACTIVATION );
    }

    ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld()->addRigidBody( m_rigidBody );
}
////////////////////////////////////////////////////////////////////////////////
