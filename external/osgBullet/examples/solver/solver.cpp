// Copyright (c) 2009 Skew Matrix  Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgViewer/Viewer>
#include <osgViewer/ViewerEventHandlers>
#include <osgGA/TrackballManipulator>
#include <osg/MatrixTransform>
#include <osg/ShapeDrawable>
#include <osg/Geode>

#include <osgBullet/CollisionShape.h>
#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/GLDebugDrawer.h>
#include <osgBullet/Utils.h>

#include <btBulletDynamicsCommon.h>

#include <BulletMultiThreaded/SpuGatheringCollisionDispatcher.h>

#define USE_MULITHREADED_CD
#ifdef USE_MULITHREADED_CD
#ifdef WIN32
#include "BulletMultiThreaded/Win32ThreadSupport.h"
#else
#include "BulletMultiThreaded/PosixThreadSupport.h"
#endif
#include "BulletMultiThreaded/SpuNarrowPhaseCollisionTask/SpuGatheringCollisionTask.h"
#endif

#include <osg/io_utils>
#include <string>
#include <map>



class InteractionManipulator : public osgGA::GUIEventHandler
{
public:
    InteractionManipulator( btDynamicsWorld* world, osg::Group* sg )
      : _world( world ),
        _sg( sg )
    {}

    void setInitialTransform( btRigidBody* rb, osg::Matrix m )
    {
        _posMap[ rb ] = m;
    }

    void updateView( osg::Camera* camera )
    {
        osg::Vec3 center, up;
        camera->getViewMatrixAsLookAt( _viewPos, center, up );
        _viewDir = center - _viewPos;
    }

    bool handle( const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& )
    {
        switch( ea.getEventType() )
        {
            case osgGA::GUIEventAdapter::KEYUP:
            {
                if (ea.getKey()==osgGA::GUIEventAdapter::KEY_BackSpace)
                {
                    reset();
                    return true;
                }
                if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Return)
                {
                    fire();
                    return true;
                }

                return false;
            }

            default:
            break;
        }
        return false;
    }

protected:
    btDynamicsWorld* _world;
    osg::ref_ptr< osg::Group > _sg;

    osg::Vec3 _viewPos, _viewDir;

    typedef std::map< btRigidBody*, osg::Matrix > PosMap;
    PosMap _posMap;

    typedef std::list< osg::ref_ptr< osg::Node > > NodeList;
    NodeList _nodeList;

    void reset()
    {
        PosMap::iterator it;
        for( it=_posMap.begin(); it!=_posMap.end(); it++ )
        {
            btRigidBody* rb = it->first;
            btTransform t = osgBullet::asBtTransform( it->second );
            rb->setWorldTransform( t );
        }
    }

    void fire()
    {
        osg::Sphere* sp = new osg::Sphere( osg::Vec3( 0., 0., 0. ), .5 );
        osg::ShapeDrawable* shape = new osg::ShapeDrawable( sp );
        osg::Geode* geode = new osg::Geode();
        geode->addDrawable( shape );
        osg::ref_ptr< osgBullet::AbsoluteModelTransform > amt = new osgBullet::AbsoluteModelTransform;
        amt->addChild( geode );
        _sg->addChild( amt.get() );

        btSphereShape* collision = new btSphereShape( .5 );

        osgBullet::MotionState* motion = new osgBullet::MotionState;
        motion->setTransform( amt.get() );

        motion->setParentTransform( osg::Matrix::translate( _viewPos ) );

        btScalar mass( 1. );
        btVector3 inertia( btVector3( 0., 0., 0. ) );//osgBullet::asBtVector3( _viewDir ) );
        collision->calculateLocalInertia( mass, inertia );
        btRigidBody::btRigidBodyConstructionInfo rbinfo( mass, motion, collision, inertia );
        btRigidBody* body = new btRigidBody( rbinfo );
        body->setLinearVelocity( osgBullet::asBtVector3( _viewDir * 50. ) );
        _world->addRigidBody( body );
    }
};


osg::ref_ptr< osg::Node > diceNode( NULL );

osg::MatrixTransform*
makeDie( btDynamicsWorld* bw, osg::Vec3 pos, InteractionManipulator* im )
{
    osg::Matrix m( osg::Matrix::translate( pos ) );
    osg::MatrixTransform* root = new osg::MatrixTransform( m );
    osgBullet::AbsoluteModelTransform* amt = new osgBullet::AbsoluteModelTransform;
    amt->setDataVariance( osg::Object::DYNAMIC );
    root->addChild( amt );

    if( !diceNode.valid() )
        diceNode = osgDB::readNodeFile( "dice.osg" );
    amt->addChild( diceNode.get() );

    btCollisionShape* cs = osgBullet::btBoxCollisionShapeFromOSG( diceNode.get() );
    osgBullet::MotionState* motion = new osgBullet::MotionState();
    motion->setTransform( amt );
    motion->setParentTransform( m );
    btScalar mass( .1 );
    btVector3 inertia( 0, 0, 0 );
    cs->calculateLocalInertia( mass, inertia );
    btRigidBody::btRigidBodyConstructionInfo rb( mass, motion, cs, inertia );
    btRigidBody* body = new btRigidBody( rb );
    body->setActivationState( DISABLE_DEACTIVATION );
    im->setInitialTransform( body, m );
    bw->addRigidBody( body );

    return( root );
}

osg::MatrixTransform*
makeCow( btDynamicsWorld* bw, osg::Vec3 pos, InteractionManipulator* im )
{
    osg::Matrix m( osg::Matrix::translate( pos ) );
    osg::MatrixTransform* root = new osg::MatrixTransform( m );
    osgBullet::AbsoluteModelTransform* amt = new osgBullet::AbsoluteModelTransform;
    amt->setDataVariance( osg::Object::DYNAMIC );
    root->addChild( amt );

    osg::Node* node = osgDB::readNodeFile( "cow.osg" );
    amt->addChild( node );

    btCollisionShape* cs = osgBullet::btConvexTriMeshCollisionShapeFromOSG( node );
    osgBullet::MotionState* motion = new osgBullet::MotionState();
    motion->setTransform( amt );
    motion->setParentTransform( m );
    btScalar mass( 2. );
    btVector3 inertia( 0, 0, 0 );
    cs->calculateLocalInertia( mass, inertia );
    btRigidBody::btRigidBodyConstructionInfo rb( mass, motion, cs, inertia );
    btRigidBody* body = new btRigidBody( rb );
    body->setActivationState( DISABLE_DEACTIVATION );
    im->setInitialTransform( body, m );
    bw->addRigidBody( body );

    return( root );
}


btDynamicsWorld*
initPhysics()
{
    // Collision configuration, of type btCollisionConfiguration
    btDefaultCollisionConfiguration* collisionConfiguration = new btDefaultCollisionConfiguration();


    // Collision dispatcher, of type btDispatcher.
#ifdef USE_MULITHREADED_CD
#ifdef WIN32
    Win32ThreadSupport::Win32ThreadConstructionInfo ci(
								"collision",
								processCollisionTask,
								createCollisionLocalStoreMemory,
								2 );
    btThreadSupportInterface* threadSupportCollision = new Win32ThreadSupport( ci );
#else
    PosixThreadSupport::ThreadConstructionInfo ci(
								"collision",
								processCollisionTask,
								createCollisionLocalStoreMemory,
								6 );
    btThreadSupportInterface* threadSupportCollision = new PosixThreadSupport( ci );
#endif
    SpuGatheringCollisionDispatcher* spud = new SpuGatheringCollisionDispatcher( threadSupportCollision, 2, collisionConfiguration );
    btCollisionDispatcher* dispatcher = spud;
#else
    btCollisionDispatcher* dispatcher = new btCollisionDispatcher( collisionConfiguration );
#endif


    // Broadphase interface, of type btBroadphaseInterface.
    btVector3 worldAabbMin( -10000, -10000, -10000 );
    btVector3 worldAabbMax( 10000, 10000, 10000 );
    btBroadphaseInterface* inter = new btAxisSweep3( worldAabbMin, worldAabbMax, 1000 );


    // Constraint solver, of type btConstraintSolver
    btConstraintSolver* solver = new btSequentialImpulseConstraintSolver;


    // Discrete (non-continuous) is the only supported dynamics world object at this time (Bullet 2.74).
    btDynamicsWorld * dynamicsWorld = new btDiscreteDynamicsWorld( dispatcher, inter, solver, collisionConfiguration );
    dynamicsWorld->setGravity( btVector3( 0, 0, -9.8 ) );

    return( dynamicsWorld );
}


int
main( int argc,
      char ** argv )
{
    btDynamicsWorld* bulletWorld = initPhysics();
    osg::Group* root = new osg::Group;

    InteractionManipulator* im = new InteractionManipulator( bulletWorld, root );

    // Make dice pyramid.
    int xCount( 7 );
    int yCount( 7 );
    float xStart( -4. );
    float yStart( -3. );
    const float zInc( 2.5 );
    float z( 1.75 );
    while( xCount && yCount )
    {
        float x, y;
        int xIdx, yIdx;
        for( y=yStart, yIdx=0; yIdx<yCount; y+=2.25, yIdx++ )
        {
            for( x=xStart, xIdx=0; xIdx<xCount; x+=2.25, xIdx++ )
            {
                osg::Vec3 pos( x, y, z );
                root->addChild( makeDie( bulletWorld, pos, im ) );
            }
        }
        xStart += 1.25;
        yStart += 1.25;
        xCount--;
        yCount--;
        z += zInc;
    }

    // Add a cow
    root->addChild( makeCow( bulletWorld, osg::Vec3( -11., 6., 4. ), im ) );

    // Make ground.
    {
        btCollisionShape* groundShape = new btStaticPlaneShape( btVector3( 0, 0, 1 ), 0 );
	    btRigidBody::btRigidBodyConstructionInfo rbInfo( 0., NULL, groundShape, btVector3(0,0,0) );
	    btRigidBody* ground = new btRigidBody(rbInfo);
	    bulletWorld->addRigidBody( ground );
    }



    osgViewer::Viewer viewer;
    viewer.setUpViewInWindow( 50, 200, 800, 600 );
    viewer.setSceneData( root );
    viewer.addEventHandler( im );

    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
    tb->setHomePosition( osg::Vec3( 0., -26., 12. ), osg::Vec3( 0., 0., 2. ), osg::Vec3( 0., 0., 1. ) ); 
    viewer.setCameraManipulator( tb );

    viewer.addEventHandler( new osgViewer::StatsHandler );

    viewer.realize();
    double prevSimTime = 0.;
    while( !viewer.done() )
    {
        const double currSimTime = viewer.getFrameStamp()->getSimulationTime();
        double elapsed( currSimTime - prevSimTime );
        bulletWorld->stepSimulation( elapsed );
        prevSimTime = currSimTime;
        viewer.frame();

        im->updateView( viewer.getCamera() );
    }

    return( 0 );
}
