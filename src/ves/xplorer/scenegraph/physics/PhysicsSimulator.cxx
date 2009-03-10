/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/vesMotionState.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/MatrixTransform>
#include <osg/ShapeDrawable>

// --- Bullet Includes --- //
#include <btBulletDynamicsCommon.h>
#include <btBulletCollisionCommon.h>

#include <osgBullet/RigidBody.h>
#include <osgBullet/OSGToCollada.h>
#include <osgBullet/MotionState.h>
#include <osgBullet/Utils.h>
#include <osgBullet/DebugBullet.h>
#include <osgBullet/GLDebugDrawer.h>

// --- C/C++ Libraries --- //
#include <sstream>
#include <string>

const int maxProxies = 32766;

//#define PRINT_CONTACT_STATISTICS 1
//#define SHOW_NUM_DEEP_PENETRATIONS 1
//#define USE_KINEMATIC_GROUND 1
//#define USER_DEFINED_FRICTION_MODEL 1

//#define USE_CUSTOM_NEAR_CALLBACK 1
//#define REGISTER_CUSTOM_COLLISION_ALGORITHM 1

using namespace ves::xplorer::scenegraph;
namespace vx = ves::xplorer;
namespace vxs = ves::xplorer::scenegraph;

//vprSingletonImp( PhysicsSimulator );
vprSingletonImpLifetime( PhysicsSimulator, 25 );

////////////////////////////////////////////////////////////////////////////////
PhysicsSimulator::PhysicsSimulator()
    :
    mDynamicsWorld( 0 ),
    mCollisionConfiguration( 0 ),
    mDispatcher( 0 ),
    mBroadphase( 0 ),
    mSolver( 0 ),
    mDebugMode( 0 ),
    mIdle( true ),
    mCollisionInformation( false ),
    shoot_speed( 50.0f ),
    mCreatedGroundPlane( false ),
    mDebugBullet( new osgBullet::DebugBullet ),
    mDebugBulletFlag( false )
{
    head.init( "VJHead" );

    InitializePhysicsSimulation();
    if( mDebugBulletFlag )
    {
        SceneManager::instance()->GetRootNode()->addChild( mDebugBullet->getRoot() );
    }
}
////////////////////////////////////////////////////////////////////////////////
PhysicsSimulator::~PhysicsSimulator()
{
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::ExitPhysics()
{
    if( mDebugBulletFlag )
    {
        SceneManager::instance()->GetRootNode()->removeChild( mDebugBullet->getRoot() );
    }
    delete mDebugBullet;
    
    for( size_t i = 0; i < mBoxVector.size(); ++i )
    {
        delete mBoxVector.at( i );
    }

    mBoxVector.clear();

    if( mDynamicsWorld )
    {
        //Remove the rigidbodies from the dynamics world and delete them
        /*for( int i = 0; i < mDynamicsWorld->getNumCollisionObjects(); ++i )
        {
            btCollisionObject* obj =
                mDynamicsWorld->getCollisionObjectArray()[ i ];
            mDynamicsWorld->removeCollisionObject( obj );

            delete obj;
        }*/

        //Delete dynamics world
        delete mDynamicsWorld;
    }

    //************************************************************************//
    //Don't know if btDynamicsWorld's destructor will clean these up
    //But, it looks like they are still hanging around, so delete them for now

    if( mCollisionConfiguration )
    {
        delete mCollisionConfiguration;
    }

    //Delete mDispatcher
    if( mDispatcher )
    {
        delete mDispatcher;
    }

    //Delete mBroadphase
    if( mBroadphase )
    {
        delete mBroadphase;
    }

    //Delete mSolver
    if( mSolver )
    {
        delete mSolver;
    }
    //************************************************************************//
}
////////////////////////////////////////////////////////////////////////////////
//By default, Bullet will use its own nearcallback
//But, you can override it using mDispatcher->setNearCallback()
void customNearCallback( btBroadphasePair& collisionPair,
                         btCollisionDispatcher& mDispatcher,
                         btDispatcherInfo& dispatchInfo )
{
    btCollisionObject* colObj0 =
        ( btCollisionObject* )collisionPair.m_pProxy0->m_clientObject;
    btCollisionObject* colObj1 =
        ( btCollisionObject* )collisionPair.m_pProxy1->m_clientObject;

    if( mDispatcher.needsCollision( colObj0, colObj1 ) )
    {
        //Dispatcher will keep algorithms persistent in the collision pair
        if( !collisionPair.m_algorithm )
        {
            collisionPair.m_algorithm =
                mDispatcher.findAlgorithm( colObj0, colObj1 );
        }

        if( collisionPair.m_algorithm )
        {
            btManifoldResult contactPointResult( colObj0, colObj1 );

            if( dispatchInfo.m_dispatchFunc ==
                btDispatcherInfo::DISPATCH_DISCRETE )
            {
                //Discrete collision detection query
                collisionPair.m_algorithm->processCollision(
                    colObj0, colObj1, dispatchInfo, &contactPointResult );
            }

            else
            {
                //Continuous collision detection query, time of impact( toi )
                float toi = collisionPair.m_algorithm->calculateTimeOfImpact(
                    colObj0, colObj1, dispatchInfo, &contactPointResult );

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
	///collision configuration contains default setup for memory, collision setup
    mCollisionConfiguration = new btDefaultCollisionConfiguration();
	///use the default collision dispatcher. For parallel processing 
    ///you can use a diffent dispatcher (see Extras/BulletMultiThreaded)
    mDispatcher = new btCollisionDispatcher( mCollisionConfiguration );

#ifdef USE_CUSTOM_NEAR_CALLBACK
    mDispatcher->setNearCallback( customNearCallback );
#else
#endif

    btVector3 worldAabbMin( -10000, -10000, -10000 );
    btVector3 worldAabbMax( 10000, 10000, 10000 );

    mBroadphase = new btAxisSweep3( worldAabbMin, worldAabbMax, maxProxies );
    //mBroadphase = new btDbvtBroadphase();

#ifdef REGISTER_CUSTOM_COLLISION_ALGORITHM
#else
    //Default constraint solver
    mSolver = new btSequentialImpulseConstraintSolver();
    mSolver->setRandSeed( 20090108 );
#endif

#if (BULLET_MAJOR_VERSION >= 2) && (BULLET_MINOR_VERSION > 63)
    mDynamicsWorld = new btDiscreteDynamicsWorld(
        mDispatcher, mBroadphase, mSolver, mCollisionConfiguration );
#else
    mDynamicsWorld = new btDiscreteDynamicsWorld(
        mDispatcher, mBroadphase, mSolver );
#endif
    //mDynamicsWorld->getDispatchInfo().m_enableSPU = true;
    mDynamicsWorld->setGravity( btVector3( 0, 0, -32.2 ) );

    if( mDebugBulletFlag )
    {
        mDynamicsWorld->setDebugDrawer( new GLDebugDrawer( SceneManager::instance()->GetRootNode() ) );
        mDynamicsWorld->getDebugDrawer()->setDebugMode( btIDebugDraw::DBG_MAX_DEBUG_DRAW_MODE );
    }
    //CreateGroundPlane();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::UpdatePhysics( float dt )
{
    if( !mDynamicsWorld || mIdle )
    {
        return;
    }

    mDynamicsWorld->stepSimulation( dt );

    //Sample debug code
    /*for( int i = 0; i < mDynamicsWorld->getNumCollisionObjects(); ++i )
    {
        btCollisionObject* temp = mDynamicsWorld->getCollisionObjectArray()[ i ];
        btVector3 bbMin = temp->getBroadphaseHandle()->m_aabbMin;
        btVector3 bbMax = temp->getBroadphaseHandle()->m_aabbMax;
        std::cout << "Min = " << bbMin.x() << " " << bbMin.y() << " " << bbMin.z() << std::endl;
        std::cout << "Max = " << bbMax.x() << " " << bbMax.y() << " " << bbMax.z() << std::endl;
    }
    
    int numManifolds1 = mDispatcher->getNumManifolds();
    for( int i = 0; i < numManifolds1; ++i )
    {
        btPersistentManifold* contactManifold =
        mDispatcher->getManifoldByIndexInternal( i );
        //contactManifold->refreshContactPoints(
        //bodyA->getWorldTransform(), bodyB->getWorldTransform() );
        
        int numContacts = contactManifold->getNumContacts();
        for (int p=0;p<contactManifold->getNumContacts();p++)
        {
            const btManifoldPoint& pt = contactManifold->getContactPoint(p);
            
            btVector3 posWorldB = pt.getPositionWorldOnB();
            btVector3 posWorldA = pt.m_normalWorldOnB;
            std::cout << "Position = " << posWorldB.x() << " " << posWorldB.y() << " " << posWorldB.z() << std::endl;
            std::cout << "Normal = " << posWorldA.x() << " " << posWorldA.y() << " " << posWorldA.z() << std::endl;
            std::cout << "Distance = " << pt.getDistance() << std::endl;
            std::cout << "Lifetime = " << pt.getLifeTime() << std::endl;
        }
    }*/
    
    if( mDebugBulletFlag )
    {
        dynamic_cast< GLDebugDrawer* >( mDynamicsWorld->getDebugDrawer() )->BeginDraw();
        mDynamicsWorld->debugDrawWorld();
        dynamic_cast< GLDebugDrawer* >( mDynamicsWorld->getDebugDrawer() )->EndDraw();
    }

    if( !mCollisionInformation )
    {
        return;
    }

    for( int i = 0; i < mDynamicsWorld->getNumCollisionObjects(); ++i )
    {
        void* tempUserData = mDynamicsWorld->getCollisionObjectArray()[ i ]->getUserPointer();
        if( tempUserData )
        {
            PhysicsRigidBody* obj = static_cast< PhysicsRigidBody* >( tempUserData );
            obj->ClearCollisions();
        }
    }
        
    int numManifolds = mDispatcher->getNumManifolds();
    for( int i = 0; i < numManifolds; ++i )
    {
        btPersistentManifold* contactManifold =
            mDispatcher->getManifoldByIndexInternal( i );
        //contactManifold->refreshContactPoints(
            //bodyA->getWorldTransform(), bodyB->getWorldTransform() );
        
        int numContacts = contactManifold->getNumContacts();
        for( int j = 0; j < numContacts; ++j )
        {
            btManifoldPoint& pt = contactManifold->getContactPoint( j );
            PhysicsRigidBody* bodyA = 0;
            PhysicsRigidBody* bodyB = 0;
            
            void* tempBodyA = static_cast< btRigidBody* >( 
                contactManifold->getBody0() )->getUserPointer();
            if( tempBodyA )
            {
                bodyA = static_cast< PhysicsRigidBody* >( tempBodyA );
                continue;
            }

            void* tempBodyB = static_cast< btRigidBody* >( 
                contactManifold->getBody1() )->getUserPointer();
            if( tempBodyB )
            {
                bodyB = static_cast< PhysicsRigidBody* >( tempBodyB );
                continue;
            }
            if( bodyA->IsStoringCollisions() )
            {
                btVector3 ptA = pt.getPositionWorldOnA();
                bodyA->PushBackCollision( bodyB, ptA );
            }
            if( bodyB->IsStoringCollisions() )
            {
                btVector3 ptB = pt.getPositionWorldOnB();
                bodyB->PushBackCollision( bodyA, ptB );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::StepSimulation()
{
    if( mIdle )
    {
        mDynamicsWorld->stepSimulation( 1.0f / 60.0f, 0 );
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

    //This code is take from Bullet in
    //Demos/OpenGL/DemoApplication.cpp
    if( mDynamicsWorld )
    {
        mDynamicsWorld->stepSimulation( 1.0f / 60.0f, 0 );
    }

    int numObjects = mDynamicsWorld->getNumCollisionObjects();

    btTransform tempTransform;
    tempTransform.setIdentity();

    for( int i = 0; i < numObjects; ++i )
    {
        btCollisionObject* colObj =
            mDynamicsWorld->getCollisionObjectArray()[ i ];
        btRigidBody* body = btRigidBody::upcast( colObj );

        if( body && body->isStaticObject() )
        {
            continue;
        }

        if( body && body->getMotionState() )
        {
            osgBullet::MotionState* motionState =
                static_cast< osgBullet::MotionState* >(
                    body->getMotionState() );
            motionState->resetTransform();
            motionState->getWorldTransform( tempTransform );

            colObj->setWorldTransform( tempTransform );
            colObj->setInterpolationWorldTransform( tempTransform );
            colObj->activate();

            //Removed cached contact points
            mDynamicsWorld->getBroadphase()->getOverlappingPairCache()->
                cleanProxyFromPairs( colObj->getBroadphaseHandle(),
                                     mDynamicsWorld->getDispatcher() );

            if( body && !body->isStaticObject() )
            {
                body->setLinearVelocity( btVector3( 0, 0, 0 ) );
                body->setAngularVelocity( btVector3( 0, 0, 0 ) );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::ShootBox( const btVector3& destination )
{
    if( mDynamicsWorld )
    {
        //Create osg::Box to visually represent rigid body
        osg::ref_ptr< osg::Geode > geode = new osg::Geode;
        osg::ref_ptr< osg::Geometry > box = new osg::Geometry;
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array;

        //Left
        vertices->push_back( osg::Vec3( -0.5f,  0.5f,  0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f,  0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, -0.5f,  0.5f ) );
        //Near
        vertices->push_back( osg::Vec3( -0.5f, -0.5f,  0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3(  0.5f, -0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3(  0.5f, -0.5f,  0.5f ) );
        //Right
        vertices->push_back( osg::Vec3( 0.5f, -0.5f,  0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f, -0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f,  0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( 0.5f,  0.5f,  0.5f ) );
        //Far
        vertices->push_back( osg::Vec3(  0.5f, 0.5f,  0.5f ) );
        vertices->push_back( osg::Vec3(  0.5f, 0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, 0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, 0.5f,  0.5f ) );
        //Top
        vertices->push_back( osg::Vec3( -0.5f,  0.5f, 0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
        vertices->push_back( osg::Vec3(  0.5f, -0.5f, 0.5f ) );
        vertices->push_back( osg::Vec3(  0.5f,  0.5f, 0.5f ) );
        //Bottom
        vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3( -0.5f,  0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3(  0.5f,  0.5f, -0.5f ) );
        vertices->push_back( osg::Vec3(  0.5f, -0.5f, -0.5f ) );

        box->setVertexArray( vertices.get() );

        osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array;
        colors->push_back( osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );
        box->setColorArray( colors.get() );
        box->setColorBinding( osg::Geometry::BIND_OVERALL );

        osg::ref_ptr< osg::Vec3Array > normals = new osg::Vec3Array;
        normals->push_back( osg::Vec3( -1.0f,  0.0f,  0.0f ) );
        normals->push_back( osg::Vec3(  0.0f, -1.0f,  0.0f ) );
        normals->push_back( osg::Vec3(  1.0f,  0.0f,  0.0f ) );
        normals->push_back( osg::Vec3(  0.0f,  1.0f,  0.0f ) );
        normals->push_back( osg::Vec3(  0.0f,  0.0f,  1.0f ) );
        normals->push_back( osg::Vec3(  0.0f,  0.0f, -1.0f ) );
        box->setNormalArray( normals.get() );
        box->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

        box->addPrimitiveSet( new osg::DrawArrays(
            osg::PrimitiveSet::QUADS, 0, vertices.get()->size() ) );

        geode->addDrawable( box.get() );

        ves::xplorer::scenegraph::CADEntity* boxEntity =
            new ves::xplorer::scenegraph::CADEntity( geode.get(),
                ves::xplorer::scenegraph::SceneManager::instance()->
                GetWorldDCS(), this );

        osg::Node::DescriptionList descriptorsList;
        descriptorsList.push_back( "VE_XML_ID" );
        descriptorsList.push_back( "" );

        boxEntity->GetDCS()->setDescriptions( descriptorsList );
        //boxEntity->GetDCS()->setName(  );
        //boxEntity->GetDCS()->SetTranslationArray(  );
        boxEntity->InitPhysics();
        boxEntity->GetPhysicsRigidBody()->GetbtRigidBody()->setFriction( 1.0 );
        boxEntity->GetPhysicsRigidBody()->BoundingBoxShape();

        mBoxVector.push_back( boxEntity );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetDebugMode( int mode )
{
    mDebugMode = mode;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetIdle( bool state )
{
    mIdle = state;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetCollisionInformation( bool collisionInformation )
{
    mCollisionInformation = collisionInformation;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetShootSpeed( float speed )
{
    shoot_speed = speed;
}
////////////////////////////////////////////////////////////////////////////////
bool PhysicsSimulator::GetIdle()
{
    return mIdle;
}
////////////////////////////////////////////////////////////////////////////////
int PhysicsSimulator::GetDebugMode()
{
    return mDebugMode;
}
////////////////////////////////////////////////////////////////////////////////
btRigidBody* PhysicsSimulator::CreateRigidBody(
    float mass, const btTransform& startTransform, btCollisionShape* shape )
{
    //RigidBody is dynamic if and only if mass is non zero, otherwise static
    bool dynamic = ( mass != 0.0f );

    btVector3 localInertia( 0 , 0, 0 );
    if( dynamic )
    {
        shape->calculateLocalInertia( mass, localInertia );
    }

    //Using motionstate is recommended, it provides interpolation capabilities
    //and only synchronizes 'active' objects
    btDefaultMotionState* myMotionState =
        new btDefaultMotionState( startTransform );
    btRigidBody* body = new btRigidBody(
        mass, myMotionState, shape, localInertia );

    mDynamicsWorld->addRigidBody( body );

    return body;
}
////////////////////////////////////////////////////////////////////////////////
btDynamicsWorld* const PhysicsSimulator::GetDynamicsWorld() const
{
    return mDynamicsWorld;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::CreateGroundPlane()
{
    if( mCreatedGroundPlane )
    {
        return;
    }
    mCreatedGroundPlane = true;
    // Compute a reasonable ground plane size based on the bounding sphere radius.
    //float dim = loadedModel->getBound()._radius * 1.5;
    //osg::Vec3 cen = loadedModel->getBound()._center;
    float dim = 150;
    osg::Vec3 cen( 0,0,0);
    //cen[ 2 ] -= dim;
    cen[ 2 ] -= 1.5;
    osg::ref_ptr< osg::Node > ground = CreateGround( dim, dim, cen );
    ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot()->addChild( ground.get() );
    osgBullet::RigidBody* body = dynamic_cast< osgBullet::RigidBody* >( ground->getUserData() );
    mDynamicsWorld->addRigidBody( body->getRigidBody() );    
}
////////////////////////////////////////////////////////////////////////////////
osg::Transform* PhysicsSimulator::CreateOSGBox( osg::Vec3 size )
{
    osg::Box* box = new osg::Box();
    box->setHalfLengths( size );
    
    osg::ShapeDrawable* shape = new osg::ShapeDrawable( box );
    shape->setColor( osg::Vec4( 1., 1., 1., 1. ) );
    osg::Geode* geode = new osg::Geode();
    geode->addDrawable( shape );
    
    osg::MatrixTransform* mt = new osg::MatrixTransform();
    mt->addChild( geode );
    
    return( mt );
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* PhysicsSimulator::CreateGround( float w, float h, const osg::Vec3& center )
{
    osg::Transform* ground = CreateOSGBox( osg::Vec3( w, h, 1.01 ) );
    
    //TRIANGLE_MESH_SHAPE_PROXYTYPE
    osgBullet::OSGToCollada converter;
    converter.setSceneGraph( ground );
    converter.setShapeType( BOX_SHAPE_PROXYTYPE );
    converter.setMass( 0.f );
    
    btRigidBody* body = converter.getRigidBody();
    
    // OSGToCollada flattens transformation to transform all
    // verts, but that doesn't work with ShapeDrawables, so we must
    // transform the box explicitly.
    osgBullet::MotionState* motion = dynamic_cast< osgBullet::MotionState* >( body->getMotionState() );
    osg::Matrix m( osg::Matrix::translate( center ) );
    motion->setParentTransform( m );
    body->setWorldTransform( osgBullet::asBtTransform( m ) );
    
    ground->setUserData( new osgBullet::RigidBody( body ) );
    
    return ground;
}
////////////////////////////////////////////////////////////////////////////////
osgBullet::DebugBullet* PhysicsSimulator::GetDebugBullet()
{
    return mDebugBullet;
}
