// Copyright (c) 2009 Skew Matrix  Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgViewer/Viewer>
#include <osgGA/TrackballManipulator>
#include <osg/MatrixTransform>
#include <osg/ShapeDrawable>
#include <osg/Geode>

#include <osgBullet/CollisionShape.h>
#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/GLDebugDrawer.h>
#include <osgBullet/ColladaUtils.h>
#include <osgBullet/Utils.h>

#include <btBulletDynamicsCommon.h>
#include <LinearMath/btIDebugDraw.h>
#include <stdio.h> //printf debugging
#include <BulletColladaConverter/ColladaConverter.h>

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

        btScalar mass( 0.2 );
        btVector3 inertia( btVector3( 0., 0., 0. ) );//osgBullet::asBtVector3( _viewDir ) );
        collision->calculateLocalInertia( mass, inertia );
        btRigidBody::btRigidBodyConstructionInfo rbinfo( mass, motion, collision, inertia );
        btRigidBody* body = new btRigidBody( rbinfo );
        body->setLinearVelocity( osgBullet::asBtVector3( _viewDir * 50. ) );
        _world->addRigidBody( body );
    }
};


btRigidBody* drawer;
osg::Transform*
makeDrawer( btDynamicsWorld* bw, InteractionManipulator* im )
{
    // Create the drawer scene graph, rooted at an AMY.
    osgBullet::AbsoluteModelTransform* amt = new osgBullet::AbsoluteModelTransform;
    amt->setDataVariance( osg::Object::DYNAMIC );

    osg::Node* node = osgDB::readNodeFile( "USMC23_4019.ASM.ive" );
    amt->addChild( node );


    // Create matrix transform to simulate an accumulated transformation in the hierarchy.
    // Create a NodePath from it.
    // In a real app, NodePath would come from visiting the parents.
    osg::Matrix m( osg::Matrix::rotate( osg::PI_2, osg::Vec3( -1., 0, 0. ) ) *
        osg::Matrix::rotate( osg::PI_2, osg::Vec3( 0., -1, 0. ) ) );
    osg::MatrixTransform* mt = new osg::MatrixTransform( m );
    osg::NodePath np;
    np.push_back( mt );

    btRigidBody* rb = osgBullet::loadDae( amt, np, "USMC23_4019.ASM1.dae", bw );
    rb->setActivationState( DISABLE_DEACTIVATION );

    // Save RB in global, and also record its initial position in the InteractionManipulator (for reset)
    drawer=rb;
    im->setInitialTransform( rb, m );

    return( amt );
}

btDynamicsWorld*
initPhysics()
{
    btDefaultCollisionConfiguration * collisionConfiguration = new btDefaultCollisionConfiguration();
    btCollisionDispatcher * dispatcher = new btCollisionDispatcher( collisionConfiguration );
    btConstraintSolver * solver = new btSequentialImpulseConstraintSolver;

    btVector3 worldAabbMin( -10000, -10000, -10000 );
    btVector3 worldAabbMax( 10000, 10000, 10000 );
    btBroadphaseInterface * inter = new btAxisSweep3( worldAabbMin, worldAabbMax, 1000 );

    btDynamicsWorld * dynamicsWorld = new btDiscreteDynamicsWorld( dispatcher, inter, solver, collisionConfiguration );

    dynamicsWorld->setGravity( btVector3( 0, 0, -9.81 ) );

    return( dynamicsWorld );
}


int
main( int argc,
      char ** argv )
{
    btDynamicsWorld* bulletWorld = initPhysics();
    osg::Group* root = new osg::Group;

    InteractionManipulator* im = new InteractionManipulator( bulletWorld, root );

    osg::ref_ptr< osg::Node > axes = osgDB::readNodeFile( "axes.osg" );


    // Add drawer
    osg::Transform* drawerRoot = makeDrawer( bulletWorld, im );
    root->addChild( drawerRoot );

    // Get reference frame from drawer.
    osg::Matrix drawerFrame;
    drawerRoot->computeLocalToWorldMatrix( drawerFrame, NULL );

    // Debug: Show the drawer reference frame with the axes model.
    osg::MatrixTransform* mt = new osg::MatrixTransform( drawerFrame );
    mt->addChild( axes.get() );
    drawerRoot->addChild( mt );


    // Make ground
    btCollisionShape* groundShape = new btStaticPlaneShape( btVector3( 0, 0, 1 ), 0 );
	btRigidBody::btRigidBodyConstructionInfo rbInfo( 0., NULL, groundShape, btVector3(0,0,0) );
	btRigidBody* ground = new btRigidBody(rbInfo);
	bulletWorld->addRigidBody( ground );
    


    // create slider constraint between drawer and groundplane and add it to world
    {
        // Ground space - orient x axis to y, and raise above the ground.
        osg::Matrix groundFrame = osg::Matrix::rotate( osg::PI_2, osg::Vec3( 0., 0., 1. ) ) *
            osg::Matrix::translate( osg::Vec3( 0., 0., 2. ) );
        btTransform btGroundFrame = osgBullet::asBtTransform( groundFrame );

        // Debug: Show the ground reference frame with the axes model.
        osg::MatrixTransform* mtg = new osg::MatrixTransform( groundFrame );
        mtg->addChild( axes.get() );
        root->addChild( mtg );

        // Drawer space
        btTransform btDrawerFrame = osgBullet::asBtTransform( drawerFrame );

        btSliderConstraint* slider = new btSliderConstraint( *ground, *drawer, btGroundFrame, btDrawerFrame, true );
        slider->setLowerLinLimit( -6.f );
	    slider->setUpperLinLimit( 0.f );
        bulletWorld->addConstraint( slider, true );
    }


    osgViewer::Viewer viewer;
    viewer.setUpViewInWindow( 50, 200, 800, 600 );
    viewer.setSceneData( root );
    viewer.addEventHandler( im );

    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
    tb->setHomePosition( osg::Vec3( 0., -26., 12. ), osg::Vec3( 0., 0., 2. ), osg::Vec3( 0., 0., 1. ) ); 
    viewer.setCameraManipulator( tb );

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
