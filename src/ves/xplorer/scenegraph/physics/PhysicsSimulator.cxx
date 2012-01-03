/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/scenegraph/physics/DiscreteDynamicsWorld.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/MatrixTransform>
#include <osg/ShapeDrawable>

// --- Bullet Includes --- //
#include <btBulletDynamicsCommon.h>
#include <btBulletCollisionCommon.h>

///Disable bullet profiling
#define BT_NO_PROFILE 1

#include <LinearMath/btQuickprof.h>

#include <BulletCollision/CollisionDispatch/btGhostObject.h>
#include <BulletCollision/CollisionDispatch/btInternalEdgeUtility.h>

// --- osgBullet Includes --- //
#include <osgbCollision/CollisionShapes.h>
#include <osgbCollision/RefBulletObject.h>
#include <osgbCollision/Utils.h>
#include <osgbCollision/GLDebugDrawer.h>
#include <osgbCollision/Version.h>

#include <osgbDynamics/RigidBody.h>
#include <osgbDynamics/PhysicsThread.h>

// --- STL Includes --- //
#include <sstream>
#include <string>

#include <boost/concept_check.hpp>

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
vprSingletonImpLifetime( PhysicsSimulator, 1 );

////////////////////////////////////////////////////////////////////////////////
PhysicsSimulator::PhysicsSimulator()
    :
    mIdle( true ),
    mCreatedGroundPlane( false ),
    mDebugBulletFlag( false ),
    mDebugMode( 0 ),
    shoot_speed( 50.0 ),
    mDynamicsWorld( NULL ),
    mCollisionConfiguration( NULL ),
    mDispatcher( NULL ),
    mBroadphase( NULL ),
    mSolver( NULL ),
    m_debugDrawer( NULL ),
    m_debugDrawerGroup( NULL ),
    m_tripleDataBuffer(),
    m_motionStateList(),
    m_physicsThread( NULL )
{
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
}
////////////////////////////////////////////////////////////////////////////////
//By default, Bullet will use its own nearcallback
//But, you can override it using mDispatcher->setNearCallback()
void customNearCallback(
    btBroadphasePair& collisionPair,
    btCollisionDispatcher& mDispatcher,
    const btDispatcherInfo& dispatchInfo )
{
    BT_PROFILE( "::customNearCallback" );

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
static bool CustomMaterialCombinerCallback(
    btManifoldPoint& cp,
    const btCollisionObject* colObj0, int partId0, int index0,
    const btCollisionObject* colObj1, int partId1, int index1 )
{
    boost::ignore_unused_variable_warning( partId0 );
    boost::ignore_unused_variable_warning( index0 );

    btAdjustInternalEdgeContacts( cp,colObj1, colObj0, partId1, index1 );
    //btAdjustInternalEdgeContacts(
        //cp, colObj1, colObj0, partId1, index1, BT_TRIANGLE_CONVEX_BACKFACE_MODE );
    //btAdjustInternalEdgeContacts(
        //cp, colObj1, colObj0, partId1, index1, BT_TRIANGLE_CONVEX_DOUBLE_SIDED+BT_TRIANGLE_CONCAVE_DOUBLE_SIDED );

    return false;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::InitializePhysicsSimulation()
{
    ///collision configuration contains default setup for memory, collision setup
    mCollisionConfiguration = new btDefaultCollisionConfiguration();
    //mCollisionConfiguration->setConvexConvexMultipointIterations( 10, 5 );
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
    mBroadphase->getOverlappingPairCache()->setInternalGhostPairCallback(
        new btGhostPairCallback() );

#ifdef REGISTER_CUSTOM_COLLISION_ALGORITHM
#else
    //Default constraint solver
    mSolver = new btSequentialImpulseConstraintSolver();
    mSolver->setRandSeed( 20090108 );
#endif

    mDynamicsWorld = new DiscreteDynamicsWorld(
        mDispatcher, mBroadphase, mSolver, mCollisionConfiguration );
    //mDynamicsWorld->getDispatchInfo().m_useConvexConservativeDistanceUtil = true;
    //mDynamicsWorld->getDispatchInfo().m_convexConservativeDistanceThreshold = 0.0001;

    //http://bulletphysics.org/mediawiki-1.5.8/index.php/BtContactSolverInfo
    //mDynamicsWorld->getSolverInfo().m_splitImpulse = true;
    //mDynamicsWorld->getSolverInfo().m_splitImpulsePenetrationThreshold = 1e30;
    //mDynamicsWorld->getSolverInfo().m_maxErrorReduction = 1e30;
    //mDynamicsWorld->getSolverInfo().m_erp = 1.0;
    //mDynamicsWorld->getSolverInfo().m_erp2 = 1.0;

    //YourOwnFilterCallback* filterCallback = new YourOwnFilterCallback();
    //filterCallback->SetDynamicsWorld( mDynamicsWorld );
    //mDynamicsWorld->getPairCache()->setOverlapFilterCallback(filterCallback);

    //mDynamicsWorld->getDispatchInfo().m_enableSPU = true;
    mDynamicsWorld->setGravity( btVector3( 0, 0, -32.174 ) );

    m_debugDrawerGroup = new osg::Group();
    m_debugDrawerGroup->setName( "osgBullet::DebugDrawer Root" );
    SceneManager::instance()->GetRootNode()->addChild(
        m_debugDrawerGroup.get() );
    m_debugDrawer = new osgbCollision::GLDebugDrawer();
    m_debugDrawerGroup->addChild( m_debugDrawer->getSceneGraph() );
    m_debugDrawer->setEnabled( false );
    m_debugDrawerGroup->setNodeMask( 0 );
    //CreateGroundPlane();

#if MULTITHREADED_OSGBULLET
    //Setup multi threaded work
    m_physicsThread = new osgbDynamics::PhysicsThread( mDynamicsWorld, &m_tripleDataBuffer );
    m_physicsThread->setProcessorAffinity( 0 );
    m_physicsThread->start();
    m_physicsThread->pause( true );
#endif

    //This is the default value for the island deactivation time. The smaller this
    //value is the sooner the island will be put to sleep. I believe this number is
    //in seconds.
    //btScalar gDeactivationTime = btScalar(2.);
    //For more information on how the islands work please see these links
    //http://bulletphysics.org/Bullet/phpBB3/viewtopic.php?f=9&t=16
    //http://bulletphysics.org/Bullet/phpBB3/viewtopic.php?f=4&t=2124
    //http://www.cs.cornell.edu/Courses/cs211/2006sp/Lectures/L26-MoreGraphs/lec26.html
    gDeactivationTime = btScalar( 0.7 );

    //Provide solution to filter out unwanted collisions with internal edges of a triangle mesh
    //http://code.google.com/p/bullet/issues/detail?id=27#c8
    gContactAddedCallback = CustomMaterialCombinerCallback;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::UpdatePhysics( float dt )
{
    if( !mDynamicsWorld || mIdle )
    {
        return;
    }

#if MULTITHREADED_OSGBULLET
    bool currentIdle = GetIdle();
    SetIdle( true );
#endif
    vprDEBUG( vesDBG, 3 ) << "|\tPhysicsSimulator::UpdatePhysics " 
        << std::endl << vprDEBUG_FLUSH;
    mDebugBulletFlag = m_debugDrawer->getEnabled();
    
    if( mDebugBulletFlag )
    {
        m_debugDrawer->BeginDraw();
    }

    //Now update the simulation by all bullet objects new positions
#if !MULTITHREADED_OSGBULLET
    //Setting max substeps to 10 gives us frame independent physics simulation
    //for frame rates above 6fps, anything below this will result in slowed
    //physics simulation clamped to 6fps
    mDynamicsWorld->stepSimulation( dt, 10 );//, btScalar( 1.0 ) / btScalar( 120.0 ) );
#else
    osgbDynamics::TripleBufferMotionStateUpdate(
        m_motionStateList, &m_tripleDataBuffer );
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
    }
    */

    if( mDebugBulletFlag )
    {
        mDynamicsWorld->debugDrawWorld();
        m_debugDrawer->EndDraw();
    }

#if MULTITHREADED_OSGBULLET
    SetIdle( currentIdle );
#endif
    vprDEBUG( vesDBG, 3 ) << "|\tEnd PhysicsSimulator::UpdatePhysics " 
        << std::endl << vprDEBUG_FLUSH;
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
    //osgAudio::SoundManager::instance()->stopAllSources();
    //osgAudio::SoundManager::instance()->clearSampleCache();
    //osgAudio::SoundManager::instance()->update();

    bool currentIdle = GetIdle();
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

        //if( body && body->isStaticObject() )
        //{
        //    continue;
        //}

        if( body && body->getMotionState() )
        {
            if( !body->isStaticObject() )
            {
                osgbDynamics::MotionState* motionState =
                    static_cast< osgbDynamics::MotionState* >(
                        body->getMotionState() );
                motionState->resetTransform();
                motionState->getWorldTransform( tempTransform );

                colObj->setWorldTransform( tempTransform );
                colObj->setInterpolationWorldTransform( tempTransform );
                colObj->activate();

                body->setLinearVelocity( btVector3( 0, 0, 0 ) );
                body->setAngularVelocity( btVector3( 0, 0, 0 ) );
            }
            //Removed cached contact points
            mDynamicsWorld->getBroadphase()->getOverlappingPairCache()->
                cleanProxyFromPairs( colObj->getBroadphaseHandle(),
                                mDynamicsWorld->getDispatcher() );
        }
    }
    
    ///reset some internal cached data in the broadphase
    mDynamicsWorld->getBroadphase()->resetPool(mDynamicsWorld->getDispatcher());
    mDynamicsWorld->getConstraintSolver()->reset();

    CharacterController& characterController =
        SceneManager::instance()->GetCharacterController();
    if( characterController.IsEnabled() )
    {
        characterController.Reset();
    }
    SetIdle( currentIdle );
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
btDynamicsWorld* PhysicsSimulator::GetDynamicsWorld() const
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
    SceneManager::instance()->GetModelRoot()->addChild( ground.get() );
    osgbCollision::RefRigidBody* body =
        dynamic_cast< osgbCollision::RefRigidBody* >( ground->getUserData() );
    mDynamicsWorld->addRigidBody( body->get() );
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
    geode->setName( "Ground Plane" );

    osg::MatrixTransform* mt = new osg::MatrixTransform();
    mt->setName( "Physics Ground Plane Transform" );

    mt->addChild( geode );
    
    return( mt );
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* PhysicsSimulator::CreateGround(
    float w, float h, const osg::Vec3& center )
{
    osg::Transform* ground = CreateOSGBox( osg::Vec3( w, h, 1.01 ) );

    osg::ref_ptr< osgbDynamics::CreationRecord > cr = new osgbDynamics::CreationRecord();
    cr->_sceneGraph = ground;
    cr->setCenterOfMass( center );
    cr->_shapeType = BOX_SHAPE_PROXYTYPE;
    cr->_restitution = 0.5f;
    cr->_friction = 1.0f;
    cr->_mass = 0.0f;
    cr->_reductionLevel = osgbDynamics::CreationRecord::NONE;
    cr->_overall = true;
    
    btRigidBody* body = osgbDynamics::createRigidBody( cr.get() );
    
    // OSGToCollada flattens transformation to transform all
    // verts, but that doesn't work with ShapeDrawables, so we must
    // transform the box explicitly.
    osgbDynamics::MotionState* motion =
        dynamic_cast< osgbDynamics::MotionState* >( body->getMotionState() );
    osg::Matrix m( osg::Matrix::translate( center ) );
    motion->setParentTransform( m );
    body->setWorldTransform( osgbCollision::asBtTransform( m ) );
    
    ground->setUserData( new osgbCollision::RefRigidBody( body ) );
    
    return ground;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetDebuggingOn( bool toggle )
{
    bool currentIdle = GetIdle();
    SetIdle( true );
    //std::cout << "here 5 " << std::endl << std::flush;
    //mDebugBulletFlag = toggle;
    m_debugDrawer->setEnabled( toggle );

    if( toggle )
    {
        m_debugDrawerGroup->setNodeMask( 1 );
        mDynamicsWorld->setDebugDrawer( m_debugDrawer );
        //mDynamicsWorld->getDebugDrawer()->
        //    setDebugMode( btIDebugDraw::DBG_MAX_DEBUG_DRAW_MODE );
    }
    else
    {
        //std::cout << "here 1 " << std::endl << std::flush;
        mDynamicsWorld->setDebugDrawer( 0 );
        //std::cout << "here 2 " << std::endl << std::flush;
        m_debugDrawerGroup->setNodeMask( 0 );
        //std::cout << "here 3 " << std::endl << std::flush;
    }

    SetIdle( currentIdle );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::RegisterMotionState(
    osgbDynamics::MotionState* motionState )
{
    bool currentIdle = GetIdle();
    SetIdle( true );
#if OSGBCOLLISION_VERSION > 10901
    m_motionStateList.insert( motionState );
#else
    m_motionStateList.push_back( motionState );
#endif
    SetIdle( currentIdle );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::UnregisterMotionState(
    osgbDynamics::MotionState* motionState )
{
    bool currentIdle = GetIdle();
    SetIdle( true );
#if OSGBCOLLISION_VERSION > 10901
    std::set< osgbDynamics::MotionState* >::iterator iter = 
        std::find( m_motionStateList.begin(), m_motionStateList.end(), motionState );
#else
    std::vector< osgbDynamics::MotionState* >::iterator iter = 
        std::find( m_motionStateList.begin(), m_motionStateList.end(), motionState );
#endif
    m_motionStateList.erase( iter );
    SetIdle( currentIdle );
}
////////////////////////////////////////////////////////////////////////////////
