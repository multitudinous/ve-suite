//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//

#include <osgViewer/Viewer>
#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgDB/FileUtils>
#include <osgGA/TrackballManipulator>
#include <osg/Geode>
#include <osg/ShapeDrawable>
#include <osg/Texture3D>
#include <osg/TextureCubeMap>
#include <osg/CullFace>
#include <osg/BlendFunc>
#include <osg/Depth>
#include <osg/Texture2D>


#include <btBulletDynamicsCommon.h>

#include <osg/io_utils>
#include <iostream>

#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>

osg::MatrixTransform * createOSGStaticObject( char * string )
{
    osg::MatrixTransform * xform = new osg::MatrixTransform;

    osg::Node * model = osgDB::readNodeFile( string );
    if( !model )
    {
        osg::notify( osg::FATAL ) << "Unable to load file " << string << std::endl;
        return( NULL );
    }
    xform->addChild( model );

    return( xform );
}

btRigidBody * createBTStaticObject( osg::MatrixTransform * xform )
{
    btCollisionShape * collision = osgBullet::btTriMeshCollisionShapeFromOSG( xform );
    btTransform groundTransform;

    groundTransform.setIdentity();

    osgBullet::MotionState * motion = new osgBullet::MotionState();
    motion->setMatrixTransform( xform );
    motion->setWorldTransform( groundTransform );

    btScalar mass( 0.0 );
    btVector3 inertia( 0, 0, 0 );
    btRigidBody::btRigidBodyConstructionInfo rb( mass, motion, collision, inertia );
    btRigidBody * body = new btRigidBody( rb );

    body->setFriction( 10 );
    body->setRestitution( 0.0 );

    return( body );
}

void createMarble( osg::Group * root,
                   btDynamicsWorld * world )
{
    osg::MatrixTransform * xform = new osg::MatrixTransform;

    std::string fileName( "marble_physics_decoration.ive" );
    osg::Node * model = osgDB::readNodeFile( fileName );
    if( !model )
    {
        osg::notify( osg::FATAL ) << "Unable to load file " << fileName << std::endl;
        return;
    }
    xform->addChild( model );
    root->addChild( xform );

    btCollisionShape * collision = osgBullet::btSphereCollisionShapeFromOSG( xform );

    btTransform transform;
    transform.setIdentity();
    transform.setOrigin( btVector3( 4.85, 2.5, 5.75 ) );

    osgBullet::MotionState * motion = new osgBullet::MotionState();
    motion->setMatrixTransform( xform );
    motion->setWorldTransform( transform );

    btScalar mass( 1.0 );
    btVector3 inertia;
    collision->calculateLocalInertia( mass, inertia );

    btRigidBody::btRigidBodyConstructionInfo rb( mass, motion, collision, inertia );
    btRigidBody * rigidBody = new btRigidBody( rb );

    rigidBody->setFriction( 1 );
    rigidBody->setRestitution( 0.0 );

    world->addRigidBody( rigidBody );
}

btDynamicsWorld * initPhysics()
{
    btDefaultCollisionConfiguration * collisionConfiguration = new btDefaultCollisionConfiguration();
    btCollisionDispatcher * dispatcher = new btCollisionDispatcher( collisionConfiguration );
    btConstraintSolver * solver = new btSequentialImpulseConstraintSolver;

    btVector3 worldAabbMin( -10000, -10000, -10000 );
    btVector3 worldAabbMax( 10000, 10000, 10000 );
    btBroadphaseInterface * inter = new btAxisSweep3( worldAabbMin, worldAabbMax, 1000 );

    btDynamicsWorld * dynamicsWorld = new btDiscreteDynamicsWorld( dispatcher, inter, solver, collisionConfiguration );

    dynamicsWorld->setGravity( btVector3( 0, 0, -10 ) );

    return( dynamicsWorld );
}

class keyboard
    : public osgGA::GUIEventHandler
{
public:
    keyboard( osg::Group * _root, btDynamicsWorld * _world )
        : root( _root )
        , world( _world )
    {
    }

    ~keyboard()
    {
    }

    bool handle( const osgGA::GUIEventAdapter & ea,
                 osgGA::GUIActionAdapter & aa )
    {
        if( ea.getHandled() )
        {
            return( false );
        }

        switch( ea.getEventType() )
        {
            case ( osgGA::GUIEventAdapter::KEYUP ):
            {
                if( ea.getKey() == osgGA::GUIEventAdapter::KEY_Return )
                {
                    createMarble( root.get(), world );
                    return( true );
                }
            }
        }

        return( false );
    }

protected:
    osg::ref_ptr< osg::Group > root;
    btDynamicsWorld * world;
};

int main( int argc,
          char * argv[] )
{
    osgViewer::Viewer viewer;

    viewer.setCameraManipulator( new osgGA::TrackballManipulator() );

    osg::ref_ptr< osg::Group > root = new osg::Group();
    viewer.setSceneData( root.get() );

    osgDB::getDataFilePathList().push_back( "../../data" );

    btDynamicsWorld * dynamicsWorld = initPhysics();

    viewer.addEventHandler( new keyboard( root.get(), dynamicsWorld ) );

    std::string fileName( "room_decoration.ive" );
    osg::Node * model = osgDB::readNodeFile( fileName );
    if( !model )
    {
        osg::notify( osg::FATAL ) << "Unable to load file " << fileName << std::endl;
        return( 1 );
    }
    root->addChild( model );

    fileName = "railing.ive";
    model = osgDB::readNodeFile( fileName );
    if( !model )
    {
        osg::notify( osg::FATAL ) << "Unable to load file " << fileName << std::endl;
        return( 1 );
    }
    root->addChild( model );

    fileName = "slide_decoration.ive";
    model = osgDB::readNodeFile( fileName );
    if( !model )
    {
        osg::notify( osg::FATAL ) << "Unable to load file " << fileName << std::endl;
        return( 1 );
    }
    root->addChild( model );

/*
 * root->addChild(osgDB::readNodeFile("column.ive"));
 * root->addChild(osgDB::readNodeFile("column_base.ive"));
 * root->addChild(osgDB::readNodeFile("column_detail.ive"));
 * root->addChild(osgDB::readNodeFile("column_top.ive"));
 */
    fileName = "water_decoration.ive";
    model = osgDB::readNodeFile( fileName );
    if( !model )
    {
        osg::notify( osg::FATAL ) << "Unable to load file " << fileName << std::endl;
        return( 1 );
    }
    root->addChild( model );

    osg::MatrixTransform * object;
    btRigidBody * body;

    object = createOSGStaticObject( "base.ive" );
    root->addChild( object );
    body = createBTStaticObject( object );
    dynamicsWorld->addRigidBody( body );

    object = createOSGStaticObject( "railing_decoration.ive" );
    root->addChild( object );
    body = createBTStaticObject( object );
    dynamicsWorld->addRigidBody( body );

    object = createOSGStaticObject( "slide_physics_decoration.ive" );
    root->addChild( object );
    body = createBTStaticObject( object );
    dynamicsWorld->addRigidBody( body );

    object = createOSGStaticObject( "funnel_decoration.ive" );
    root->addChild( object );
    body = createBTStaticObject( object );
    dynamicsWorld->addRigidBody( body );

    createMarble( root.get(), dynamicsWorld );


    double currSimTime;
    double prevSimTime = viewer.getFrameStamp()->getSimulationTime();

    viewer.getCameraManipulator()->setHomePosition( osg::Vec3( 0, 0, 30 ), osg::Vec3( 0, 0, 0 ), osg::Vec3( 0, 1, 0 ) );

    viewer.realize();

    while( !viewer.done() )
    {
        currSimTime = viewer.getFrameStamp()->getSimulationTime();
        dynamicsWorld->stepSimulation( currSimTime - prevSimTime );
        prevSimTime = currSimTime;
        viewer.frame();
    }

    return( 0 );
}

