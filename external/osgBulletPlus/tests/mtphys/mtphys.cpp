// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgViewer/Viewer>
#include <osgGA/TrackballManipulator>
#include <osg/MatrixTransform>
#include <osg/ShapeDrawable>
#include <osg/Geode>

#include <osgbBullet/MotionState.h>
#include <osgbBullet/CollisionShapes.h>
#include <osgbBullet/GLDebugDrawer.h>
#include <osgbBullet/ColladaUtils.h>
#include <osgbBullet/Utils.h>

#include <osgbBullet/TripleBuffer.h>
#include <osgbBullet/PhysicsThread.h>

#include <btBulletDynamicsCommon.h>
#include <LinearMath/btIDebugDraw.h>
#include <stdio.h> //printf debugging

#include <osg/io_utils>
#include <string>
#include <map>


osgbBullet::TripleBuffer tBuf;
osgbBullet::MotionStateList msl;


class InteractionManipulator : public osgGA::GUIEventHandler
{
public:
    InteractionManipulator( btDiscreteDynamicsWorld* world, osg::Group* sg, osgbBullet::PhysicsThread* pt=NULL )
      : _world( world ),
        _sg( sg ),
        _pt( pt )
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
    btDiscreteDynamicsWorld* _world;
    osg::ref_ptr< osg::Group > _sg;
    osgbBullet::PhysicsThread* _pt;

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
            btTransform t = osgbBullet::asBtTransform( it->second );
            rb->setWorldTransform( t );
        }
    }

    void fire()
    {
        osg::Sphere* sp = new osg::Sphere( osg::Vec3( 0., 0., 0. ), .5 );
        osg::ShapeDrawable* shape = new osg::ShapeDrawable( sp );
        osg::Geode* geode = new osg::Geode();
        geode->addDrawable( shape );
        osg::ref_ptr< osgwTools::AbsoluteModelTransform > amt = new osgwTools::AbsoluteModelTransform;
        amt->addChild( geode );
        _sg->addChild( amt.get() );

        btSphereShape* collision = new btSphereShape( .5 );

        // Block physics
        if( _pt != NULL )
            // Blocks until thread is paused.
            _pt->pause( true );

        osgbBullet::MotionState* motion = new osgbBullet::MotionState;
        motion->setTransform( amt.get() );
        motion->setParentTransform( osg::Matrix::translate( _viewPos ) );

        btScalar mass( 0.2 );
        btVector3 inertia( btVector3( 0., 0., 0. ) );//osgbBullet::asBtVector3( _viewDir ) );
        collision->calculateLocalInertia( mass, inertia );
        btRigidBody::btRigidBodyConstructionInfo rbinfo( mass, motion, collision, inertia );
        btRigidBody* body = new btRigidBody( rbinfo );
        body->setLinearVelocity( osgbBullet::asBtVector3( _viewDir * 50. ) );
        _world->addRigidBody( body );

        // Set up for triple buffering.
        motion->registerTripleBuffer( &tBuf );
        msl.push_back( motion );

        // Unblock physics
        if( _pt != NULL )
            _pt->pause( false );
    }
};


btRigidBody* doorFrame;
osg::BoundingSphere doorFrameBB;

osg::Transform*
makeDoorFrame( btDiscreteDynamicsWorld* bw, InteractionManipulator* im )
{
    // Create the door frame scene graph, rooted at an AMT.
    osgwTools::AbsoluteModelTransform* amt = new osgwTools::AbsoluteModelTransform;

    osg::Node* node = osgDB::readNodeFile( "USMC23_1019.ASM.ive" );
    amt->addChild( node );

    // We'll use this later to position the debug axes.
    doorFrameBB = node->getBound();


    // Create matrix transform to simulate an accumulated transformation in the hierarchy.
    // Create a NodePath from it.
    // In a real app, NodePath would come from visiting the parents.
    osg::Matrix m( osg::Matrix::rotate( osg::PI, osg::Vec3( 1., 0., 0. ) ) *
            osg::Matrix::translate( osg::Vec3( 0., 0., 7.1 ) ) );
    osg::MatrixTransform* mt = new osg::MatrixTransform( m );
    osg::NodePath np;
    np.push_back( mt );

    btRigidBody* rb = osgbBullet::loadDae( amt, np, "USMC23_1019.ASM1.dae" );

    bw->addRigidBody( rb );
    rb->setActivationState( DISABLE_DEACTIVATION );

    // Save RB in global, and also record its initial position in the InteractionManipulator (for reset)
    doorFrame=rb;

    return( amt );
}

btRigidBody* door;
osg::BoundingSphere doorBB;

osg::Transform*
makeDoor( btDiscreteDynamicsWorld* bw, InteractionManipulator* im )
{
    // Create the door scene graph, rooted at an AMY.
    osgwTools::AbsoluteModelTransform* amt = new osgwTools::AbsoluteModelTransform;
    amt->setDataVariance( osg::Object::DYNAMIC );

    osg::Node* node = osgDB::readNodeFile( "USMC23_1020.ASM.ive" );
    amt->addChild( node );

    // We'll use this later to position the debug axes.
    doorBB = node->getBound();


    // Create matrix transform to simulate an accumulated transformation in the hierarchy.
    // Create a NodePath from it.
    // In a real app, NodePath would come from visiting the parents.
    osg::Matrix m( osg::Matrix::rotate( osg::PI_2, osg::Vec3( 0., 1., 0. ) ) * 
        osg::Matrix::translate( osg::Vec3( -.26, -3.14, .2 ) ) );
    osg::MatrixTransform* mt = new osg::MatrixTransform( m );
    osg::NodePath np;
    np.push_back( mt );

    btRigidBody* rb = osgbBullet::loadDae( amt, np, "USMC23_1020.ASM1.dae" );
    osgbBullet::MotionState* ms = static_cast< osgbBullet::MotionState* >( rb->getMotionState() );
    ms->registerTripleBuffer( &tBuf );
    msl.push_back( ms );

    bw->addRigidBody( rb );
    rb->setActivationState( DISABLE_DEACTIVATION );

    // Save RB in global, and also record its initial position in the InteractionManipulator (for reset)
    door=rb;
    im->setInitialTransform( rb, m );

    return( amt );
}

btDiscreteDynamicsWorld*
initPhysics()
{
    btDefaultCollisionConfiguration * collisionConfiguration = new btDefaultCollisionConfiguration();
    btCollisionDispatcher * dispatcher = new btCollisionDispatcher( collisionConfiguration );
    btConstraintSolver * solver = new btSequentialImpulseConstraintSolver;

    btVector3 worldAabbMin( -10000, -10000, -10000 );
    btVector3 worldAabbMax( 10000, 10000, 10000 );
    btBroadphaseInterface * inter = new btAxisSweep3( worldAabbMin, worldAabbMax, 1000 );

    btDiscreteDynamicsWorld * dynamicsWorld = new btDiscreteDynamicsWorld( dispatcher, inter, solver, collisionConfiguration );

    dynamicsWorld->setGravity( btVector3( 0, 0, -9.81 ) );

    return( dynamicsWorld );
}


int
main( int argc,
      char ** argv )
{
    btDiscreteDynamicsWorld* bulletWorld = initPhysics();
    osg::Group* root = new osg::Group;
    osgbBullet::PhysicsThread pt( bulletWorld, &tBuf );
    InteractionManipulator* im = new InteractionManipulator( bulletWorld, root, &pt );

    osg::ref_ptr< osg::Node > axes = osgDB::readNodeFile( "axes.osg" );


    // Add doorframe (constraint body A)
    osg::Transform* doorFrameRoot = makeDoorFrame( bulletWorld, im );
    root->addChild( doorFrameRoot );

    // Debug: Show the doorframe reference frame with the axes model.
    osg::Vec3 pivotA( -.12f, 1.5f, .25f );
    osg::Vec3 axisA( 0., 0., 1. );
    osg::MatrixTransform* mt = new osg::MatrixTransform(
        osg::Matrix::translate( doorFrameBB.center() + pivotA ) );
    mt->addChild( axes.get() );
    doorFrameRoot->addChild( mt );


    // Add door (constraint body B)
    osg::Transform* doorRoot = makeDoor( bulletWorld, im );
    root->addChild( doorRoot );

    // Debug: Show the door reference frame with the axes model.
    osg::Vec3 pivotB( 0.f, -1.5f, -.12f );
    osg::Vec3 axisB( 1., 0., 0. );
    mt = new osg::MatrixTransform(
        osg::Matrix::translate( doorBB.center() + pivotB ) );
    mt->addChild( axes.get() );
    doorRoot->addChild( mt );

    root->addChild( osgbBullet::generateGroundPlane( osg::Vec4( 0.f, .2f, 1.f, -1.f ), bulletWorld ) );
    // create hinge constraint
    {
        // creating a hinge constraint and adding to world
        const btVector3 btPivotA( osgbBullet::asBtVector3( pivotA ) );
        btVector3 btAxisA( osgbBullet::asBtVector3( axisA ) );
        const btVector3 btPivotB( osgbBullet::asBtVector3( pivotB ) );
        btVector3 btAxisB( osgbBullet::asBtVector3( axisB ) );

        btHingeConstraint* hinge = new btHingeConstraint(
            *doorFrame, *door, btPivotA, btPivotB, btAxisA, btAxisB );
        hinge->setLimit( -3., -.5 );
        bulletWorld->addConstraint( hinge, true );
    }


    osgViewer::Viewer viewer;
    viewer.setThreadingModel( osgViewer::ViewerBase::SingleThreaded );
    viewer.setUpViewInWindow( 50, 200, 800, 600 );
    viewer.setSceneData( root );
    viewer.addEventHandler( im );

    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
    tb->setHomePosition( osg::Vec3( 0., -26., 12. ), osg::Vec3( 0., 0., 2. ), osg::Vec3( 0., 0., 1. ) ); 
    viewer.setCameraManipulator( tb );
    viewer.getCamera()->setClearColor( osg::Vec4( 1., 1., 1., 1. ) );

    viewer.realize();
    pt.setProcessorAffinity( 0 );
    pt.start();

    while( !viewer.done() )
    {
        TripleBufferMotionStateUpdate( msl, &tBuf );

        viewer.frame();

        im->updateView( viewer.getCamera() );
    }

    pt.stopPhysics();
    pt.join();

    return( 0 );
}
