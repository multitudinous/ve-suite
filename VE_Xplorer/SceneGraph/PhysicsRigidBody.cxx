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
#include "VE_Xplorer/SceneGraph/PhysicsRigidBody.h"
#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/TriangleIndexFunctor>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDiscreteDynamicsWorld.h>

#include <BulletCollision/CollisionShapes/btCollisionShape.h>
#include <BulletCollision/CollisionShapes/btBoxShape.h>
#include <BulletCollision/CollisionShapes/btTriangleMesh.h>
#include <BulletCollision/CollisionShapes/btBvhTriangleMeshShape.h>
#include <BulletCollision/CollisionShapes/btConvexTriangleMeshShape.h>

#include <LinearMath/btDefaultMotionState.h>

// --- C/C++ Libraries --- //
#include <iostream>

class TriIndexFunc
{
public:
    TriIndexFunc(){;}
    ~TriIndexFunc(){;}

    void inline operator()( unsigned int pos1, unsigned int pos2, unsigned int pos3 )
    {
        triangleIndex.push_back( pos1 );
        triangleIndex.push_back( pos2 );
        triangleIndex.push_back( pos3 );
    }

    std::vector< unsigned int > triangleIndex;
};

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
PhysicsRigidBody::PhysicsRigidBody( osg::Node* node, const btTransform& startTransform )
:
m_mass( 1.0f ),
tri_mesh( 0 ),
btRigidBody( btScalar( m_mass ),                                        //mass
             new btDefaultMotionState( startTransform ),                //motionState
             0,                                                         //collisionShape
             btVector3( 0.0f, 0.0f, 0.0f ),                             //localInertia
             btScalar( 0.0f ),                                          //linearDamping
             btScalar( 0.0f ),                                          //angularDamping
             btScalar( 1.0f ),                                          //friction
             btScalar( 0.0f ) ),                                        //restitution

NodeVisitor( TRAVERSE_ALL_CHILDREN )
{
    node->accept( *this );

    BoundingBoxShape();
}
////////////////////////////////////////////////////////////////////////////////
PhysicsRigidBody::~PhysicsRigidBody()
{
    if( tri_mesh )
    {
       delete tri_mesh;
    }

    if( m_collisionShape )
    {
       delete m_collisionShape;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::apply( osg::Geode& geode )
{ 
    tri_mesh = new btTriangleMesh;

    for( size_t i = 0; i < geode.getNumDrawables(); i++ )
    {
        osg::TriangleIndexFunctor< TriIndexFunc > TIF;
        osg::ref_ptr< osg::Vec3Array > vertex_array;

        geode.getDrawable( i )->accept( TIF );
        vertex_array = static_cast< osg::Vec3Array* >( geode.getDrawable( i )->asGeometry()->getVertexArray() );

        bb.expandBy( geode.getDrawable( i )->getBound() );

        btVector3 v1, v2, v3;

        for( size_t j = 0; j < TIF.triangleIndex.size() / 3; j++ )
        {
            tri_mesh->addTriangle( btVector3( vertex_array->at( TIF.triangleIndex.at( j * 3     ) ).x(),
										      vertex_array->at( TIF.triangleIndex.at( j * 3     ) ).y(),
										      vertex_array->at( TIF.triangleIndex.at( j * 3     ) ).z() ),
						           btVector3( vertex_array->at( TIF.triangleIndex.at( j * 3 + 1 ) ).x(),
										      vertex_array->at( TIF.triangleIndex.at( j * 3 + 1 ) ).y(),
										      vertex_array->at( TIF.triangleIndex.at( j * 3 + 1 ) ).z() ),
						           btVector3( vertex_array->at( TIF.triangleIndex.at( j * 3 + 2 ) ).x(),
										      vertex_array->at( TIF.triangleIndex.at( j * 3 + 2 ) ).y(),
										      vertex_array->at( TIF.triangleIndex.at( j * 3 + 2 ) ).z() ) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetMass( float mass )
{
    m_mass = mass;

    if( this && m_collisionShape )
    {
        SetMassProps();
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

        this->setMassProps( m_mass, localInertia );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::BoundingBoxShape()
{
    if( this )
    {
        VE_SceneGraph::PhysicsSimulator::instance()->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;

        m_collisionShape = 0;
    }

    m_collisionShape = new btBoxShape( btVector3( ( bb.xMax() - bb.xMin() ) * 0.5f,
                                                  ( bb.yMax() - bb.yMin() ) * 0.5f,
                                                  ( bb.zMax() - bb.zMin() ) * 0.5f ) );

    SetMassProps();

    VE_SceneGraph::PhysicsSimulator::instance()->GetDynamicsWorld()->addRigidBody( this );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::StaticConcaveShape()
{
    if( this )
    {
        VE_SceneGraph::PhysicsSimulator::instance()->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;

        m_collisionShape = 0;
    }

    m_collisionShape = new btBvhTriangleMeshShape( tri_mesh, false );

    SetMass( 0 );

    VE_SceneGraph::PhysicsSimulator::instance()->GetDynamicsWorld()->addRigidBody( this );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::ConvexShape()
{
    if( this )
    {
        VE_SceneGraph::PhysicsSimulator::instance()->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;

        m_collisionShape = 0;
    }

    m_collisionShape = new btConvexTriangleMeshShape( tri_mesh );

    SetMassProps();

    VE_SceneGraph::PhysicsSimulator::instance()->GetDynamicsWorld()->addRigidBody( this );
}
////////////////////////////////////////////////////////////////////////////////
