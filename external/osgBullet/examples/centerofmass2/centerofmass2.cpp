//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//

#include <osgViewer/Viewer>
#include <osgDB/ReadFile>
#include <osgDB/FileUtils>
#include <osgGA/TrackballManipulator>
#include <osg/ShapeDrawable>
#include <osg/Geode>
#include <osg/Geometry>


#include <btBulletDynamicsCommon.h>

#include <osg/io_utils>
#include <iostream>

#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/RigidBody.h>
#include <osgBullet/RigidBodyAnimation.h>


osg::MatrixTransform * createOSGBox( osg::Vec3 size )
{
    osg::Box * box = new osg::Box();

    box->setHalfLengths( size );

    osg::ShapeDrawable * shape = new osg::ShapeDrawable( box );

    osg::Geode * geode = new osg::Geode();
    geode->addDrawable( shape );

    osg::MatrixTransform * transform = new osg::MatrixTransform();
    transform->addChild( geode );

    return( transform );
}

osg::MatrixTransform * createOffOriginOSGBox( osg::Vec3 size )
{
    const osg::Vec3 dim( size * 2 );

    osg::Geode * geode = new osg::Geode;
    osg::Geometry * geom = new osg::Geometry;
    osg::Vec3Array * v = new osg::Vec3Array;

    v->resize( 8 );
    ( *v )[ 0 ] = osg::Vec3( 0, 0, 0 );
    ( *v )[ 1 ] = osg::Vec3( 0, dim.y(), 0 );
    ( *v )[ 2 ] = osg::Vec3( dim.x(), dim.y(), 0 );
    ( *v )[ 3 ] = osg::Vec3( dim.x(), 0, 0 );
    ( *v )[ 4 ] = osg::Vec3( 0, 0, dim.z() );
    ( *v )[ 5 ] = osg::Vec3( dim.x(), 0, dim.z() );
    ( *v )[ 6 ] = osg::Vec3( dim.x(), dim.y(), dim.z() );
    ( *v )[ 7 ] = osg::Vec3( 0, dim.y(), dim.z() );
    geom->setVertexArray( v );

    osg::Vec3Array * n = new osg::Vec3Array;
    n->resize( 6 );
    ( *n )[ 0 ] = osg::Vec3( 0, 0, -1 );
    ( *n )[ 1 ] = osg::Vec3( 0, 0, 1 );
    ( *n )[ 2 ] = osg::Vec3( -1, 0, 0 );
    ( *n )[ 3 ] = osg::Vec3( 1, 0, 0 );
    ( *n )[ 4 ] = osg::Vec3( 0, -1, 0 );
    ( *n )[ 5 ] = osg::Vec3( 0, 1, 0 );
    geom->setNormalArray( n );
    geom->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    osg::Vec4Array * c = new osg::Vec4Array;
    c->resize( 8 );
    ( *c )[ 0 ] = osg::Vec4( 1, 1, 1, 1 );
    ( *c )[ 1 ] = osg::Vec4( .6, 0, 0, 1 );
    ( *c )[ 2 ] = osg::Vec4( .6, 0, 0, 1 );
    ( *c )[ 3 ] = osg::Vec4( .6, 0, 0, 1 );
    ( *c )[ 4 ] = osg::Vec4( .6, 0, 0, 1 );
    ( *c )[ 5 ] = osg::Vec4( .6, 0, 0, 1 );
    ( *c )[ 6 ] = osg::Vec4( .6, 0, 0, 1 );
    ( *c )[ 7 ] = osg::Vec4( .6, 0, 0, 1 );
    geom->setColorArray( c );
    geom->setColorBinding( osg::Geometry::BIND_PER_VERTEX );

    GLushort indices[] =
    {
        0, 1, 2, 3,
        4, 5, 6, 7,
        0, 4, 7, 1,
        3, 2, 6, 5,
        0, 3, 5, 4,
        1, 7, 6, 2
    };
    geom->addPrimitiveSet( new osg::DrawElementsUShort( GL_QUADS, 24, indices ) );
    geode->addDrawable( geom );

    osg::MatrixTransform * mt = new osg::MatrixTransform();
    mt->addChild( geode );
    return( mt );
}

btRigidBody * createBTBox( osg::MatrixTransform * box,
                           btVector3 center )
{
    btCollisionShape * collision = osgBullet::btBoxCollisionShapeFromOSG( box );

    btTransform groundTransform;
    groundTransform.setIdentity();
    groundTransform.setOrigin( center );

    osgBullet::MotionState * motion = new osgBullet::MotionState();
    motion->setMatrixTransform( box );
    motion->setWorldTransform( groundTransform );

    btScalar mass( 0. );
    btVector3 inertia( 0, 0, 0 );
    btRigidBody::btRigidBodyConstructionInfo rb( mass, motion, collision, inertia );
    btRigidBody * body = new btRigidBody( rb );

    return( body );
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

void createStack( osg::Group * root,
                  btDynamicsWorld * dynamicsWorld )
{
    osg::MatrixTransform * bottom = createOSGBox( osg::Vec3( 1, 1, 1 ) );
    root->addChild( bottom );
    btRigidBody * bottomBody = createBTBox( bottom, btVector3( 0, 0, 1.01 ) );
    dynamicsWorld->addRigidBody( bottomBody );


    osg::MatrixTransform * target = createOffOriginOSGBox( osg::Vec3( 2, 1, 1 ) );
    root->addChild( target );

/*  OSGBULLET CODE */
    osgBullet::MotionState * motion = new osgBullet::MotionState();
    motion->setMatrixTransform( target );

    btCollisionShape * collision = osgBullet::btConvexTriMeshCollisionShapeFromOSG( target );
    btTransform trans;
    trans.setIdentity();
    trans.setOrigin( btVector3( -2, -1, -1 ) );
    btCompoundShape * masterShape = new btCompoundShape();
    masterShape->addChildShape( trans, collision );
    motion->m_centerOfMassOffset = trans;

    osg::Geode * g = osgBullet::geodeFromCollisionShape( masterShape );
    if( g != NULL )
    {
        osg::notify( osg::ALWAYS ) << g->getName() << std::endl;
        g->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
        target->addChild( g );
        osg::notify( osg::ALWAYS ) << "here" << std::endl;
    }

    /*  BULLET CODE */
    btTransform bodyTransform;
    bodyTransform.setIdentity();
    bodyTransform.setOrigin( btVector3( 0, 0, 3.01 ) );
    motion->setWorldTransform( bodyTransform );

    btScalar mass( 1.0 );
    btVector3 inertia;
    masterShape->calculateLocalInertia( mass, inertia );
    btRigidBody::btRigidBodyConstructionInfo rbinfo( mass, motion, masterShape, inertia );
    btRigidBody * body = new btRigidBody( rbinfo );
    body->setLinearVelocity( btVector3( 0, 0, 0 ) );
    body->setAngularVelocity( btVector3( 0, 0, 0 ) );
    body->setWorldTransform( bodyTransform );
    dynamicsWorld->addRigidBody( body );
}

int main( int argc,
          char * argv[] )
{
    osg::ref_ptr< osg::Group > root = new osg::Group();

    btDynamicsWorld * dynamicsWorld = initPhysics();


    // Create stack of blocks
    createStack( root.get(), dynamicsWorld );


    // Make the ground plane
    osg::MatrixTransform * ground = createOSGBox( osg::Vec3( 10, 10, .01 ) );
    root->addChild( ground );
    btRigidBody * groundBody = createBTBox( ground, btVector3( 0, 0, 0 ) );
    dynamicsWorld->addRigidBody( groundBody );


    osgViewer::Viewer viewer;
    osgGA::TrackballManipulator * tb = new osgGA::TrackballManipulator();
    tb->setHomePosition( osg::Vec3( -6, -26, 7 ),
                        osg::Vec3( 0, 0, 4 ),
                        osg::Vec3( 0, 0, 1 ) );
    viewer.setCameraManipulator( tb );
    viewer.setSceneData( root.get() );

    double currSimTime;
    double prevSimTime = viewer.getFrameStamp()->getSimulationTime();

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

