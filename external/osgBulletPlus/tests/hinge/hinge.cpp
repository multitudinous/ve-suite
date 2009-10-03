// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgViewer/Viewer>
#include <osgGA/TrackballManipulator>
#include <osg/MatrixTransform>
#include <osg/ShapeDrawable>
#include <osg/Geode>

#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/GLDebugDrawer.h>
#include <osgBullet/ColladaUtils.h>
#include <osgBullet/Utils.h>

#include <btBulletDynamicsCommon.h>
#include <LinearMath/btIDebugDraw.h>
#include <stdio.h> //printf debugging

#include <osg/io_utils>
#include <string>
#include <map>


// Filter out collisions between the door and doorframe.
//
// Bullet collision filtering tutorial:
//   http://www.bulletphysics.com/mediawiki-1.5.8/index.php?title=Collision_Filtering
//
// Define filter groups
enum CollisionTypes {
    COL_DOOR = 0x1 << 0,
    COL_DOORFRAME = 0x1 << 1,
    COL_DEFAULT = 0x1 << 2,
};
// Define filter masks
unsigned int doorCollidesWith( COL_DEFAULT );
unsigned int doorFrameCollidesWith( COL_DEFAULT );
unsigned int defaultCollidesWith( COL_DOOR | COL_DOORFRAME | COL_DEFAULT );


class InteractionManipulator : public osgGA::GUIEventHandler
{
public:
    InteractionManipulator( btDiscreteDynamicsWorld* world, osg::Group* sg )
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
    btDiscreteDynamicsWorld* _world;
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
        osg::ref_ptr< osgTools::AbsoluteModelTransform > amt = new osgTools::AbsoluteModelTransform;
        amt->addChild( geode );
        _sg->addChild( amt.get() );

        btSphereShape* collision = new btSphereShape( .5 );

        osgBullet::MotionState* motion = new osgBullet::MotionState;
        motion->setTransform( amt.get() );

        motion->setParentTransform( osg::Matrix::translate( _viewPos ) );

        btScalar mass( 0.2 );
        btVector3 inertia( btVector3( 0., 0., 0. ) );//osgBullet::asBtVector3( _viewDir ) );
        collision->calculateLocalInertia( mass, inertia );
        btRigidBody::btRigidBodyConstructionInfo rbinfo( mass, motion, collision, inertia );
        btRigidBody* body = new btRigidBody( rbinfo );
        body->setLinearVelocity( osgBullet::asBtVector3( _viewDir * 50. ) );
        _world->addRigidBody( body, COL_DEFAULT, defaultCollidesWith );
    }
};


btRigidBody* doorFrame;
osg::BoundingSphere doorbb;

osg::Transform*
makeDoorFrame( btDiscreteDynamicsWorld* bw, InteractionManipulator* im )
{
    // Create the door frame scene graph, rooted at an AMT.
    osgTools::AbsoluteModelTransform* amt = new osgTools::AbsoluteModelTransform;

    osg::Node* node = osgDB::readNodeFile( "USMC23_1019.ASM.ive" );
    amt->addChild( node );


    // Create matrix transform to simulate an accumulated transformation in the hierarchy.
    // Create a NodePath from it.
    // In a real app, NodePath would come from visiting the parents.
    osg::Matrix m( osg::Matrix::rotate( osg::PI, osg::Vec3( 1., 0., 0. ) ) *
            osg::Matrix::translate( osg::Vec3( 0., 0., 7.1 ) ) );
    osg::MatrixTransform* mt = new osg::MatrixTransform( m );
    osg::NodePath np;
    np.push_back( mt );

    btRigidBody* rb = osgBullet::loadDae( amt, np, "USMC23_1019.ASM1.dae" );
    bw->addRigidBody( rb, COL_DOORFRAME, doorFrameCollidesWith );
    rb->setActivationState( DISABLE_DEACTIVATION );

    // Save RB in global, and also record its initial position in the InteractionManipulator (for reset)
    doorFrame=rb;

    return( amt );
}

btRigidBody* door;
osg::Transform*
makeDoor( btDiscreteDynamicsWorld* bw, InteractionManipulator* im )
{
    // Create the door scene graph, rooted at an AMY.
    osgTools::AbsoluteModelTransform* amt = new osgTools::AbsoluteModelTransform;
    amt->setDataVariance( osg::Object::DYNAMIC );

    osg::Node* node = osgDB::readNodeFile( "USMC23_1020.ASM.ive" );
    amt->addChild( node );

    // We'll use this later to position the debug axes.
    doorbb = node->getBound();


    // Create matrix transform to simulate an accumulated transformation in the hierarchy.
    // Create a NodePath from it.
    // In a real app, NodePath would come from visiting the parents.
    osg::Matrix m( osg::Matrix::rotate( osg::PI_2, osg::Vec3( 0., 1., 0. ) ) * 
        osg::Matrix::translate( osg::Vec3( -.26, -3.14, .2 ) ) );
    osg::MatrixTransform* mt = new osg::MatrixTransform( m );
    osg::NodePath np;
    np.push_back( mt );

    btRigidBody* rb = osgBullet::loadDae( amt, np, "USMC23_1020.ASM1.dae" );
    bw->addRigidBody( rb, COL_DOOR, doorCollidesWith );
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

    InteractionManipulator* im = new InteractionManipulator( bulletWorld, root );

    osg::ref_ptr< osg::Node > axes = osgDB::readNodeFile( "axes.osg" );


    // Add door
    osg::Transform* doorRoot = makeDoor( bulletWorld, im );
    root->addChild( doorRoot );

    // Debug: Show the door reference frame with the axes model.
    osg::Vec3 hingePivotPoint( 0.f, -1.5f, -.12f );
    osg::MatrixTransform* mt = new osg::MatrixTransform(
        osg::Matrix::translate( doorbb.center() + hingePivotPoint ) );
    mt->addChild( axes.get() );
    doorRoot->addChild( mt );


    // Add doorframe
    osg::Transform* doorFrameRoot = makeDoorFrame( bulletWorld, im );
    root->addChild( doorFrameRoot );


    // Make ground
    btCollisionShape* groundShape = new btStaticPlaneShape( btVector3( 0, 0, 1 ), 0 );
    btRigidBody::btRigidBodyConstructionInfo rbInfo( 0., NULL, groundShape, btVector3(0,0,0) );
    btRigidBody* ground = new btRigidBody(rbInfo);
    bulletWorld->addRigidBody( ground, COL_DEFAULT, defaultCollidesWith );
    


    // create hinge constraint
    {
        // creating a hinge constraint and adding to world
        const btVector3 btPivot( hingePivotPoint.x(), hingePivotPoint.y(), hingePivotPoint.z() ); 
        btVector3 btAxisA( 1., 0., 0. ); // rotation about the x axis
        btHingeConstraint* hinge = new btHingeConstraint( *door, btPivot, btAxisA );
        hinge->setLimit( -2.8f, 0.f );
        bulletWorld->addConstraint(hinge, true);
    }


    osgViewer::Viewer viewer;
    viewer.setUpViewInWindow( 50, 200, 800, 600 );
    viewer.setSceneData( root );
    viewer.addEventHandler( im );

    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
    tb->setHomePosition( osg::Vec3( 0., -26., 12. ), osg::Vec3( 0., 0., 2. ), osg::Vec3( 0., 0., 1. ) ); 
    viewer.setCameraManipulator( tb );
    viewer.getCamera()->setClearColor( osg::Vec4( 1., 1., 1., 1. ) );

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
