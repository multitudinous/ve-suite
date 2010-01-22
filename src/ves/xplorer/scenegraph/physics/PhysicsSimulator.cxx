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
#include <ves/xplorer/scenegraph/physics/CharacterController.h>

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
#include <LinearMath/btQuickprof.h>

#include <osgbBullet/RefRigidBody.h>
#include <osgbBullet/OSGToCollada.h>
#include <osgbBullet/MotionState.h>
#include <osgbBullet/Utils.h>
#include <osgbBullet/GLDebugDrawer.h>
#include <osgbBullet/PhysicsThread.h>

// --- STL Includes --- //
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

#define MULTITHREADED_OSGBULLET 0
//vprSingletonImp( PhysicsSimulator );
vprSingletonImpLifetime( PhysicsSimulator, 13 );

////////////////////////////////////////////////////////////////////////////////
PhysicsSimulator::PhysicsSimulator()
    :
    mDynamicsWorld( NULL ),
    mCollisionConfiguration( NULL ),
    mDispatcher( NULL ),
    mBroadphase( NULL ),
    mSolver( NULL ),
    mDebugMode( 0 ),
    mIdle( true ),
    mCollisionInformation( false ),
    shoot_speed( 50.0 ),
    mCreatedGroundPlane( false ),
    mDebugBulletFlag( false ),
    m_debugDrawer( 0 ),
    m_physicsThread( 0 )
{
    head = new gadget::DeviceInterface<class gadget::PositionProxy>;
    head->init( "VJHead" );

    InitializePhysicsSimulation();
}
////////////////////////////////////////////////////////////////////////////////
PhysicsSimulator::~PhysicsSimulator()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::ExitPhysics()
{    
#if MULTITHREADED_OSGBULLET
    if( m_physicsThread )
    {
        m_physicsThread->stopPhysics();
        m_physicsThread->join();
    }
#endif
    delete head;
    head = 0;

    delete m_debugDrawer;
    m_debugDrawer = 0;
    
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
                         const btDispatcherInfo& dispatchInfo )
{
    BT_PROFILE("::customNearCallback");

    btCollisionObject* colObj0 =
        ( btCollisionObject* )collisionPair.m_pProxy0->m_clientObject;
    btCollisionObject* colObj1 =
        ( btCollisionObject* )collisionPair.m_pProxy1->m_clientObject;
    
    if( mDispatcher.needsCollision( colObj0, colObj1 ) )
    {
        /*if( colObj0->getCollisionShape()->isCompound() && !colObj0->isStaticObject() && (static_cast< btRigidBody* >( colObj0 )->getInvMass() < 1.0f) )
        {
            std::cout << "have a compound shape" << std::endl;
            btTransform t;
            btVector3 aabbMin;
            btVector3 aabbMax;
            colObj0->getCollisionShape()->getAabb( t, aabbMin, aabbMax );
            //std::cout << aabbMin.x() << " " << aabbMin.y() << " "<< aabbMin.z() << std::endl 
            //    << aabbMax.x() << " " << aabbMax.y() << " " << aabbMax.z() << std::endl;
            //colObj0->setActivationState( DISABLE_SIMULATION );
            btScalar diag = aabbMin.distance( aabbMax );
            int collisionFlags = colObj0->getCollisionFlags();

            if( diag > 10.0f )
            {
                std::cout << "have a large object " << std::endl;
                
                //collisionFlags &= 
                collisionFlags = btCollisionObject::CF_STATIC_OBJECT;
                colObj0->setCollisionFlags( collisionFlags );
                btVector3 localInertia( 0, 0, 0 );
                static_cast< btRigidBody* >( colObj0 )->setMassProps( 0.0f, localInertia );
                mDynamicsWorld->removeCollisionObject( colObj0 );
                //return;
            }
        }*/
        
        //Dispatcher will keep algorithms persistent in the collision pair
        if( !collisionPair.m_algorithm )
        {
            BT_PROFILE("dispatcher.findAlgorithm");
            collisionPair.m_algorithm =
                mDispatcher.findAlgorithm( colObj0, colObj1 );
        }

        if( collisionPair.m_algorithm )
        {
            BT_PROFILE("collisionPair.m_algorithm");

            btManifoldResult contactPointResult( colObj0, colObj1 );

            if( dispatchInfo.m_dispatchFunc ==
                btDispatcherInfo::DISPATCH_DISCRETE )
            {
                //Discrete collision detection query
                BT_PROFILE("collisionPair.m_algorithm->processCollision");
                collisionPair.m_algorithm->processCollision(
                    colObj0, colObj1, dispatchInfo, &contactPointResult );
            }

            else
            {
                BT_PROFILE("collisionPair.m_algorithm->calculateTimeOfImpact");
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
struct YourOwnFilterCallback : public btOverlapFilterCallback
{
    btDynamicsWorld* mDynamicsWorld;
    
    void SetDynamicsWorld( btDynamicsWorld* dynamicsWorld )
    {
        mDynamicsWorld = dynamicsWorld;
    }
	// return true when pairs need collision
	virtual bool needBroadphaseCollision(btBroadphaseProxy* proxy0,btBroadphaseProxy* proxy1) const
	{
		bool collides = (proxy0->m_collisionFilterGroup & proxy1->m_collisionFilterMask) != 0;
		collides = collides && (proxy1->m_collisionFilterGroup & proxy0->m_collisionFilterMask);
        
        /*btCollisionObject* colObj0 =
            ( btCollisionObject* )( proxy0->m_clientObject );
        btCollisionObject* colObj1 =
            (btCollisionObject* )( proxy1->m_clientObject );
        
        if( colObj0 )
        {
        btRigidBody* tempRB = btRigidBody::upcast(colObj0);

        if( colObj0->getCollisionShape()->isCompound() && !colObj0->isStaticObject() && (tempRB->getInvMass() < 1.0f) )
        {
            //if( colObj0->getCollisionShape()->isCompound() && !colObj0->isStaticObject() && (static_cast< btRigidBody* >( colObj0 )->getInvMass() < 1.0f) )
            {
                btTransform t;
                btVector3 aabbMin;
                btVector3 aabbMax;
                colObj0->getCollisionShape()->getAabb( t, aabbMin, aabbMax );
                //std::cout << aabbMin.x() << " " << aabbMin.y() << " "<< aabbMin.z() << std::endl 
                //    << aabbMax.x() << " " << aabbMax.y() << " " << aabbMax.z() << std::endl;
                //colObj0->setActivationState( DISABLE_SIMULATION );
                btScalar diag = aabbMin.distance( aabbMax );
                //int collisionFlags = colObj0->getCollisionFlags();
                
                if( diag > 10.0f )
                {
                    std::cout << "have a large object " << std::endl;
                    mDynamicsWorld->removeRigidBody( tempRB );

                    //collisionFlags &= 
                    //collisionFlags = btCollisionObject::CF_STATIC_OBJECT;
                    colObj0->setCollisionFlags( btCollisionObject::CF_STATIC_OBJECT );
                    btVector3 localInertia( 0, 0, 0 );
                    tempRB->setMassProps( 0.0f, localInertia );
                    mDynamicsWorld->addRigidBody( tempRB );
                    //return;
                }
            }
        }
            
        }

        if( colObj1 )
        {         
        btRigidBody* tempRB1 = btRigidBody::upcast(colObj1);

        if( colObj1->getCollisionShape()->isCompound() && !colObj1->isStaticObject() && (tempRB1->getInvMass() < 1.0f) )
        {
            //if( colObj0->getCollisionShape()->isCompound() && !colObj0->isStaticObject() && (static_cast< btRigidBody* >( colObj0 )->getInvMass() < 1.0f) )
            {
                btTransform t;
                btVector3 aabbMin;
                btVector3 aabbMax;
                colObj1->getCollisionShape()->getAabb( t, aabbMin, aabbMax );
                //std::cout << aabbMin.x() << " " << aabbMin.y() << " "<< aabbMin.z() << std::endl 
                //    << aabbMax.x() << " " << aabbMax.y() << " " << aabbMax.z() << std::endl;
                //colObj0->setActivationState( DISABLE_SIMULATION );
                btScalar diag = aabbMin.distance( aabbMax );
                //int collisionFlags = colObj0->getCollisionFlags();
                
                if( diag > 15.0f )
                {
                    std::cout << "have a large object " << std::endl;
                    mDynamicsWorld->removeRigidBody( tempRB1 );
                    
                    //collisionFlags &= 
                    //collisionFlags = btCollisionObject::CF_STATIC_OBJECT;
                    colObj1->setCollisionFlags( btCollisionObject::CF_STATIC_OBJECT );
                    btVector3 localInertia( 0, 0, 0 );
                    tempRB1->setMassProps( 0.0f, localInertia );
                    mDynamicsWorld->addRigidBody( tempRB1 );
                    //return;
                }
            }
        }
        }*/
        
		//add some additional logic here that modified 'collides'
		return collides;
	}
};
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

    mDynamicsWorld = new btDiscreteDynamicsWorld(
        mDispatcher, mBroadphase, mSolver, mCollisionConfiguration );
    //mDynamicsWorld->getDispatchInfo().m_useConvexConservativeDistanceUtil = true;
    //mDynamicsWorld->getDispatchInfo().m_convexConservativeDistanceThreshold = 0.0001f;

    //YourOwnFilterCallback* filterCallback = new YourOwnFilterCallback();
    //filterCallback->SetDynamicsWorld( mDynamicsWorld );
    //mDynamicsWorld->getPairCache()->setOverlapFilterCallback(filterCallback);

    //mDynamicsWorld->getDispatchInfo().m_enableSPU = true;
    mDynamicsWorld->setGravity( btVector3( 0, 0, -32.174 ) );

    m_debugDrawerGroup = new osg::Group();
    m_debugDrawerGroup->setName( "osgBullet::DebugDrawer Root" );
    SceneManager::instance()->GetRootNode()->
        addChild( m_debugDrawerGroup.get() );
    m_debugDrawer = new osgbBullet::GLDebugDrawer();
    m_debugDrawerGroup->addChild( m_debugDrawer->getSceneGraph() );
    m_debugDrawerGroup->setNodeMask( 0 );
    //CreateGroundPlane();

#if MULTITHREADED_OSGBULLET
    //Setup multi threaded work
    m_physicsThread = new osgbBullet::PhysicsThread( mDynamicsWorld, &m_tripleDataBuffer );
    m_physicsThread->setProcessorAffinity( 0 );
    m_physicsThread->start();
    m_physicsThread->pause( true );
#endif

    //This is the default value for the island deactivation time. The smaller this
    //value is the sooner the island will be put to sleep. I believe this number is
    //in seconds.
    //btScalar	gDeactivationTime = btScalar(2.);
    //For more information on how the islands work please see these links
    //http://bulletphysics.org/Bullet/phpBB3/viewtopic.php?f=9&t=16
    //http://bulletphysics.org/Bullet/phpBB3/viewtopic.php?f=4&t=2124
    //http://www.cs.cornell.edu/Courses/cs211/2006sp/Lectures/L26-MoreGraphs/lec26.html
    gDeactivationTime = btScalar(0.5);
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::UpdatePhysics( float dt )
{
    if( !mDynamicsWorld || mIdle )
    {
        return;
    }

#if MULTITHREADED_OSGBULLET
    SetIdle( true );
#endif

    if( mDebugBulletFlag )
    {
        static_cast< osgbBullet::GLDebugDrawer* >(
            mDynamicsWorld->getDebugDrawer() )->BeginDraw();
    }

    //Now update the simulation by all bullet objects new positions
#if !MULTITHREADED_OSGBULLET
    mDynamicsWorld->stepSimulation( dt );
#else
    osgbBullet::TripleBufferMotionStateUpdate( m_motionStateList, 
                                             &m_tripleDataBuffer );
#endif

#ifndef BT_NO_PROFILE
    CProfileManager::dumpAll();
#endif

    //Sample debug code
    /*
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
        mDynamicsWorld->debugDrawWorld();
        dynamic_cast< osgbBullet::GLDebugDrawer* >(
            mDynamicsWorld->getDebugDrawer() )->EndDraw();
    }

    if( !mCollisionInformation )
    {
#if MULTITHREADED_OSGBULLET
        SetIdle( false );
#endif
        return;
    }

    for( int i = 0; i < mDynamicsWorld->getNumCollisionObjects(); ++i )
    {
        void* tempUserData =
            mDynamicsWorld->getCollisionObjectArray()[ i ]->getUserPointer();
        if( tempUserData )
        {
            PhysicsRigidBody* obj =
                static_cast< PhysicsRigidBody* >( tempUserData );
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
#if MULTITHREADED_OSGBULLET
    SetIdle( false );
#endif
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::StepSimulation()
{
    if( mIdle )
    {
        //no need to pause simulation since the simulation is alreayd paused
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
    SetIdle( true );

    //This code is take from Bullet in
    //Demos/OpenGL/DemoApplication.cpp
    if( mDynamicsWorld )
    {
        mDynamicsWorld->stepSimulation( 1.0 / 60.0, 0 );
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
            osgbBullet::MotionState* motionState =
                static_cast< osgbBullet::MotionState* >(
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

    CharacterController* characterController =
        SceneManager::instance()->GetCharacterController();
    if( characterController->IsEnabled() )
    {
        characterController->Reset();
    }
    SetIdle( false );
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
#if MULTITHREADED_OSGBULLET
    if( mIdle )
    {
        m_physicsThread->pause( true );
    }
    else
    {
        m_physicsThread->pause( false );
    }
#endif
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
///Adds a rigid body to the physics simulator
btRigidBody* PhysicsSimulator::CreateRigidBody(
    float mass, const btTransform& startTransform, btCollisionShape* shape )
{
    //RigidBody is dynamic if and only if mass is non zero, otherwise static
    bool dynamic = ( mass != 0.0 );

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
    osgbBullet::RefRigidBody* body = dynamic_cast< osgbBullet::RefRigidBody* >( ground->getUserData() );
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
    osgbBullet::OSGToCollada converter;
    converter.setSceneGraph( ground );
    converter.setShapeType( BOX_SHAPE_PROXYTYPE );
    converter.setMass( 0.f );
    
    btRigidBody* body = converter.getRigidBody();
    
    // OSGToCollada flattens transformation to transform all
    // verts, but that doesn't work with ShapeDrawables, so we must
    // transform the box explicitly.
    osgbBullet::MotionState* motion = dynamic_cast< osgbBullet::MotionState* >( body->getMotionState() );
    osg::Matrix m( osg::Matrix::translate( center ) );
    motion->setParentTransform( m );
    body->setWorldTransform( osgbBullet::asBtTransform( m ) );
    
    ground->setUserData( new osgbBullet::RefRigidBody( body ) );
    
    return ground;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetDebuggingOn( bool toggle )
{
    SetIdle( true );
    mDebugBulletFlag = toggle;
    
    if( mDebugBulletFlag )
    {
        m_debugDrawerGroup->setNodeMask( 1 );
        mDynamicsWorld->setDebugDrawer( m_debugDrawer );
        //mDynamicsWorld->getDebugDrawer()->
        //    setDebugMode( btIDebugDraw::DBG_MAX_DEBUG_DRAW_MODE );
    }
    else
    {
        mDynamicsWorld->setDebugDrawer( 0 );
        m_debugDrawer->BeginDraw();
        m_debugDrawerGroup->setNodeMask( 0 );
    }
    SetIdle( false );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::RegisterMotionState( osgbBullet::MotionState* motionState )
{
    SetIdle( true );
    m_motionStateList.push_back( motionState );
    SetIdle( false );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::UnregisterMotionState( osgbBullet::MotionState* motionState )
{
    SetIdle( true );
    std::vector< osgbBullet::MotionState* >::iterator iter = 
        std::find( m_motionStateList.begin(), m_motionStateList.end(), motionState );
    m_motionStateList.erase( iter );
    SetIdle( false );
}
////////////////////////////////////////////////////////////////////////////////
