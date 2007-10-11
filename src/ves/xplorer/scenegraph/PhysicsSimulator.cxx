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
#include <ves/xplorer/scenegraph/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgDB/ReaderWriter>
#include <osgDB/Registry>
#include <osgDB/FileUtils>

// --- Bullet Includes --- //
#include <btBulletDynamicsCommon.h>
#include <btBulletCollisionCommon.h>
//#include <BulletCollision/CollisionDispatch/btSimulationIslandManager.h>

//#include "BulletCollision/CollisionDispatch/btSphereSphereCollisionAlgorithm.h>
//#include "../Extras/AlternativeCollisionAlgorithms/BoxBoxCollisionAlgorithm.h>
//#include "BulletCollision/CollisionDispatch/btSphereTriangleCollisionAlgorithm.h>

// --- C/C++ Libraries --- //
#include <sstream>
#include <ostream>
#include <string>

using namespace VE_SceneGraph;

const int maxProxies = 32766;

vprSingletonImp( PhysicsSimulator );

//#define PRINT_CONTACT_STATISTICS 1
//#define SHOW_NUM_DEEP_PENETRATIONS 1
//#define USE_KINEMATIC_GROUND 1
//#define USER_DEFINED_FRICTION_MODEL 1

//#define USE_CUSTOM_NEAR_CALLBACK 1
//#define REGISTER_CUSTOM_COLLISION_ALGORITHM 1

////////////////////////////////////////////////////////////////////////////////
PhysicsSimulator::PhysicsSimulator()
:
m_dynamicsWorld( 0 ),
m_dispatcher( 0 ),
m_broadphase( 0 ),
m_solver( 0 ),
m_debugMode( 0 ),
m_idle( true ),
shoot_speed( 50.0f )
{
    head.init( "VJHead" );

    InitializePhysicsSimulation();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::ExitPhysics()
{
    if( m_dynamicsWorld )
    {
        //Remove the rigidbodies from the dynamics world and delete them
        for( int i = 0; i < m_dynamicsWorld->getNumCollisionObjects(); ++i )
        {
            btCollisionObject* obj = m_dynamicsWorld->getCollisionObjectArray()[ i ];
            m_dynamicsWorld->removeCollisionObject( obj );

            delete obj;
        }

        //Delete dynamics world
        delete m_dynamicsWorld;
    }

    //*************************************************************************//
    //Don't know if btDynamicsWorld's destructor will clean these up in the future
    //But, it looks like they are still hanging around, so delete them for now

    //Delete m_dispatcher
    if( m_dispatcher )
    {
        delete m_dispatcher;
    }
   
    //Delete m_broadphase
    if( m_broadphase )
    {
        delete m_broadphase;
    }

    //Delete m_solver
    if( m_solver )
    {
        delete m_solver;
    }
    //*************************************************************************//

    for( size_t i = 0; i < box_vector.size(); ++i )
    {
        delete box_vector.at( i );
    }

    box_vector.clear();
}
////////////////////////////////////////////////////////////////////////////////
//By default, Bullet will use its own nearcallback, but you can override it using m_dispatcher->setNearCallback()
void customNearCallback( btBroadphasePair& collisionPair, btCollisionDispatcher& m_dispatcher, btDispatcherInfo& dispatchInfo )
{
    btCollisionObject* colObj0 = ( btCollisionObject* )collisionPair.m_pProxy0->m_clientObject;
    btCollisionObject* colObj1 = ( btCollisionObject* )collisionPair.m_pProxy1->m_clientObject;

    if( m_dispatcher.needsCollision( colObj0, colObj1 ) )
    {
        //Dispatcher will keep algorithms persistent in the collision pair
        if( !collisionPair.m_algorithm )
        {
            collisionPair.m_algorithm = m_dispatcher.findAlgorithm( colObj0, colObj1 );
        }

        if( collisionPair.m_algorithm )
        {
            btManifoldResult contactPointResult( colObj0, colObj1 );

            if( dispatchInfo.m_dispatchFunc == btDispatcherInfo::DISPATCH_DISCRETE )
            {
                //Discrete collision detection query
                collisionPair.m_algorithm->processCollision( colObj0, colObj1, dispatchInfo, &contactPointResult );
            }

            else
            {
                //Continuous collision detection query, time of impact ( toi )
                float toi = collisionPair.m_algorithm->calculateTimeOfImpact( colObj0, colObj1, dispatchInfo, &contactPointResult );

                if( dispatchInfo.m_timeOfImpact > toi )
                {
                    dispatchInfo.m_timeOfImpact = toi;
                }
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::InitializePhysicsSimulation()
{
    btDefaultCollisionConfiguration* collisionConfiguration = new btDefaultCollisionConfiguration();
    m_dispatcher = new btCollisionDispatcher( collisionConfiguration );

#ifdef USE_CUSTOM_NEAR_CALLBACK
    m_dispatcher->setNearCallback( customNearCallback );
#else
#endif

    btVector3 worldAabbMin( -10000, -10000, -10000 );
    btVector3 worldAabbMax( 10000, 10000, 10000 );

    m_broadphase = new btAxisSweep3( worldAabbMin, worldAabbMax, maxProxies );

#ifdef REGISTER_CUSTOM_COLLISION_ALGORITHM
#else
    //Default constraint solver
    m_solver = new btSequentialImpulseConstraintSolver();
#endif

    m_dynamicsWorld = new btDiscreteDynamicsWorld( m_dispatcher, m_broadphase, m_solver );
    //m_dynamicsWorld->getDispatchInfo().m_enableSPU = true;
    m_dynamicsWorld->setGravity( btVector3( 0, 0, -10 ) );

    //m_dynamicsWorld->setDebugDrawer( &debugDrawer );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::UpdatePhysics( float dt )
{
    if( m_dynamicsWorld && !m_idle )
    {
        m_dynamicsWorld->stepSimulation( dt );
        /*
        printf( "dt = %f: ", dt );

        if( m_dynamicsWorld )
        {
            //During m_idle mode, just run 1 simulation step maximum
            int maxSimSubSteps = m_idle ? 1 : 1;
            if( m_idle )
            {
                dt = 1.0/420.f;
            }

            int numSimSteps = m_dynamicsWorld->stepSimulation( dt, maxSimSubSteps );
            if( !numSimSteps )
            {
                printf( "Interpolated transforms\n" );
            }

            else
            {
                if( numSimSteps > maxSimSubSteps )
                {
                    //Detect dropping frames
                    printf( "Dropped (%i) simulation steps out of %i\n", numSimSteps - maxSimSubSteps, numSimSteps );
                }

                else
                {
                    printf( "Simulated (%i) steps\n", numSimSteps );
                }
            }
        }
        */
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::StepSimulation()
{
    if( m_idle )
    {
        m_dynamicsWorld->stepSimulation(  1.0f / 60.0f, 0  );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::ResetScene()
{
    /*
#ifdef SHOW_NUM_DEEP_PENETRATIONS
    gNumDeepPenetrationChecks = 0;
    gNumGjkChecks = 0;
#endif
    */

    if( m_dynamicsWorld )
    {
        m_dynamicsWorld->stepSimulation( 1.0f / 60.0f, 0 );
    }

    int numObjects = m_dynamicsWorld->getNumCollisionObjects();

    for( int i = 0; i < numObjects; ++i )
    {
        btCollisionObject* colObj = m_dynamicsWorld->getCollisionObjectArray()[ i ];
        btRigidBody* body = btRigidBody::upcast( colObj );

        if( body && body->getMotionState() )
        {
            btDefaultMotionState* myMotionState = ( btDefaultMotionState* )body->getMotionState();
            myMotionState->m_graphicsWorldTrans = myMotionState->m_startWorldTrans;

            colObj->setWorldTransform( myMotionState->m_graphicsWorldTrans );
            colObj->setInterpolationWorldTransform( myMotionState->m_startWorldTrans );
            colObj->activate();

            //Removed cached contact points
				m_dynamicsWorld->getBroadphase()->getOverlappingPairCache()->cleanProxyFromPairs( colObj->getBroadphaseHandle(), m_dynamicsWorld->getDispatcher() );

            btRigidBody* body = btRigidBody::upcast( colObj );

            if( body && !body->isStaticObject() )
            {
                btRigidBody::upcast( colObj )->setLinearVelocity( btVector3( 0, 0, 0 ) );
                btRigidBody::upcast( colObj )->setAngularVelocity( btVector3( 0, 0, 0 ) );
            }
        }

        /*
        //Quickly search some issue at a certain simulation frame, pressing space to reset
        int fixed = 18;
        for( int i = 0; i < fixed; ++i )
        {
            getDynamicsWorld()->stepSimulation( 1.0f / 60.0f, 1 );
        }
        */
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::ShootBox( const btVector3& destination )
{
    if( m_dynamicsWorld )
    {
        //Create osg::Box to visually represent rigid body
        osg::ref_ptr< osg::Geode > geode = new osg::Geode;

        osg::ref_ptr< osg::Geometry > box = new osg::Geometry;

        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array;

        //Left
        vertices->push_back( osg::Vec3( -0.5f, 0.5f, 0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, 0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );

        //Near
        vertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f, -0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f, -0.5f, 0.5f ) );

        //Right
        vertices->push_back( osg::Vec3( 0.5f, -0.5f, 0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f, -0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f, 0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f, 0.5f, 0.5f ) );

        //Far
        vertices->push_back( osg::Vec3( 0.5f, 0.5f, 0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f, 0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, 0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, 0.5f, 0.5f ) );

        //Top
        vertices->push_back( osg::Vec3( -0.5f, 0.5f, 0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f, -0.5f, 0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f, 0.5f, 0.5f ) );

        //Bottom
        vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, 0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f, 0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f, -0.5f, -0.5f ) );

        box->setVertexArray( vertices.get() );

        osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array;
        colors->push_back( osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );
        box->setColorArray( colors.get() );
        box->setColorBinding( osg::Geometry::BIND_OVERALL );

        osg::ref_ptr< osg::Vec3Array > normals = new osg::Vec3Array;
        normals->push_back( osg::Vec3( -1.0f, 0.0f, 0.0f ) );					//Left
        normals->push_back( osg::Vec3( 0.0f, -1.0f, 0.0f ) );					//Near
        normals->push_back( osg::Vec3( 1.0f, 0.0f, 0.0f ) );					//Right
        normals->push_back( osg::Vec3( 0.0f, 1.0f, 0.0f ) );					//Far
        normals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );					//Top
        normals->push_back( osg::Vec3( 0.0f, 0.0f, -1.0f ) );					//Bottom
        box->setNormalArray( normals.get() );
        box->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

        box->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, vertices.get()->size() ) );

        geode->addDrawable( box.get() );

        std::ostringstream box_ss;

        osgDB::writeNodeFile( *geode, "C:/Users/JK/Desktop/Models/box.osg" );

        osgDB::Registry::instance()->getReaderWriterForExtension("osg")->writeNode( *geode, box_ss );

        box_vector.push_back( new VE_SceneGraph::CADEntity( "C:/Users/JK/Desktop/Models/box.osg", VE_SceneGraph::SceneManager::instance()->GetWorldDCS(), false, false ) );

        float mass = 1.0f;
        btTransform transform;
        transform.setIdentity();
        gadget::PositionData* head_pos;
        head_pos = head->getPositionData();
        btVector3 position;
        position.setValue( head_pos->mPosData[ 0 ][ 3 ], head_pos->mPosData[ 1 ][ 3 ], head_pos->mPosData[ 2 ][ 3 ] );
        transform.setOrigin( position );

        btCollisionShape* box_shape = new btBoxShape( btVector3( 1.0f, 1.0f, 1.0f ) );
        btRigidBody* body = CreateRigidBody( mass, transform, box_shape );

        btVector3 lin_vel( destination[ 0 ] + position[ 0 ], destination[ 1 ] + position[ 1 ], destination[ 2 ] + position[ 2 ] );
        lin_vel.normalize();
        lin_vel *= shoot_speed;

        body->getWorldTransform().setOrigin( position );
        body->getWorldTransform().setRotation( btQuaternion( 0, 0, 0, 1 ) );
        body->setLinearVelocity( lin_vel );
        body->setAngularVelocity( btVector3( 0, 0, 0 ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetDebugMode( int mode )
{
    m_debugMode = mode;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetIdle( bool state )
{
    m_idle = state;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetShootSpeed( float speed )
{
    shoot_speed = speed;
}
////////////////////////////////////////////////////////////////////////////////
bool PhysicsSimulator::GetIdle()
{
    return m_idle;
}
////////////////////////////////////////////////////////////////////////////////
int PhysicsSimulator::GetDebugMode()
{
    return m_debugMode;
}
////////////////////////////////////////////////////////////////////////////////
btRigidBody* PhysicsSimulator::CreateRigidBody( float mass, const btTransform& startTransform, btCollisionShape* shape )
{
    //RigidBody is dynamic if and only if mass is non zero, otherwise static
    bool dynamic = ( mass != 0.0f );

    btVector3 localInertia( 0 ,0, 0 );
    if( dynamic )
    {
        shape->calculateLocalInertia( mass, localInertia );
    }

    //Using motionstate is recommended, it provides interpolation capabilities, and only synchronizes 'active' objects
    btDefaultMotionState* myMotionState = new btDefaultMotionState( startTransform );
    btRigidBody* body = new btRigidBody( mass, myMotionState, shape, localInertia );

    m_dynamicsWorld->addRigidBody( body );

    return body;
}
////////////////////////////////////////////////////////////////////////////////
btDynamicsWorld* PhysicsSimulator::GetDynamicsWorld()
{
    return m_dynamicsWorld;
}
////////////////////////////////////////////////////////////////////////////////
