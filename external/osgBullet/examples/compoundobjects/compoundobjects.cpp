//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//


#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/RigidBody.h>
#include <osgBullet/RigidBodyAnimation.h>
#include <btBulletDynamicsCommon.h>

#include <osgViewer/Viewer>
#include <osgDB/ReadFile>
#include <osgDB/FileUtils>
#include <osgGA/TrackballManipulator>
#include <osg/ShapeDrawable>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/AnimationPath>
#include <osg/PolygonMode>
#include <osg/PolygonOffset>
#include <osg/io_utils>
#include <iostream>


osg::AnimationPath * createAnimationPath( const osg::Vec3 & origin,
                                          const osg::Vec3 & direction,
                                          double duration )
{
    osg::AnimationPath * animationPath = new osg::AnimationPath;

    animationPath->setLoopMode( osg::AnimationPath::LOOP );

    int numSamples( 40 );
    double time( 0. );
    osg::Quat orient;
    int idx;
    for( idx = 0; idx < numSamples; idx++ )
    {
        const double scale( ( double )idx / ( double )numSamples );
        const double curTime( duration * scale );
        osg::Vec3 pos( origin + ( direction * scale ) );

        animationPath->insert( curTime, osg::AnimationPath::ControlPoint( pos, orient ) );
    }

    return( animationPath );
}

osg::MatrixTransform * createAnimatedObject( osg::Vec3 size )
{
    osg::Box * box = new osg::Box();

    box->setHalfLengths( size );

    osg::ShapeDrawable * shape = new osg::ShapeDrawable( box );

    osg::Geode * geode = new osg::Geode();
    geode->addDrawable( shape );

    osg::MatrixTransform* t0 = new osg::MatrixTransform();
    t0->setMatrix( osg::Matrix::translate( .25, -.25, -.25 ) );
    t0->addChild( geode );
    osg::MatrixTransform* t1 = new osg::MatrixTransform();
    t1->setMatrix( osg::Matrix::translate( -.25, .25, .25 ) );
    t1->addChild( geode );
    
    osg::MatrixTransform* transform = new osg::MatrixTransform();
    transform->addChild( t0 );
    transform->addChild( t1 );

    return( transform );
}

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

btRigidBody* createBTCompound( osg::MatrixTransform* obj,
                           btVector3 center )
{
    btCollisionShape* collision = osgBullet::btTriMeshCollisionShapeFromOSG( obj );

    btTransform groundTransform;
    groundTransform.setIdentity();
    groundTransform.setOrigin( center );

    osgBullet::MotionState * motion = new osgBullet::MotionState();
    motion->setMatrixTransform( obj );
    motion->setWorldTransform( groundTransform );

    btScalar mass( 0. );
    btVector3 inertia( 0, 0, 0 );
    btRigidBody::btRigidBodyConstructionInfo rb( mass, motion, collision, inertia );
    btRigidBody * body = new btRigidBody( rb );

    osg::Node* n = osgBullet::osgNodeFromBtCollisionShape( collision );
    obj->addChild( n );
        osg::StateSet* state = n->getOrCreateStateSet();
        osg::PolygonMode* pm = new osg::PolygonMode( osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );
        state->setAttributeAndModes( pm );
        osg::PolygonOffset* po = new osg::PolygonOffset( -1, -1 );
        state->setAttributeAndModes( po );
        state->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

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

void createTarget( osg::Group * root,
                   btDynamicsWorld * dynamicsWorld )
{
    osg::MatrixTransform * target = createOffOriginOSGBox( osg::Vec3( 1, 1, 1 ) );
    root->addChild( target );

/*  OSGBULLET CODE */
    osgBullet::MotionState * motion = new osgBullet::MotionState();
    motion->setMatrixTransform( target );

    btCollisionShape * collision = osgBullet::btConvexTriMeshCollisionShapeFromOSG( target );
    btTransform trans;
    trans.setIdentity();
    trans.setOrigin( btVector3( -1, -1, -1 ) );
    btCompoundShape * masterShape = new btCompoundShape();
    masterShape->addChildShape( trans, collision );
    motion->m_centerOfMassOffset = trans;

    // Create an OSG representation of the Bullet shape and attach it.
    // This is mainly for debugging.
    osg::Node * g = osgBullet::osgNodeFromBtCollisionShape( masterShape );
    if( g != NULL )
    {
        target->addChild( g );

        // Set debug node state.
        osg::StateSet* state = g->getOrCreateStateSet();
        osg::PolygonMode* pm = new osg::PolygonMode( osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );
        state->setAttributeAndModes( pm );
        osg::PolygonOffset* po = new osg::PolygonOffset( -1, -1 );
        state->setAttributeAndModes( po );
        state->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    }

    btScalar mass( 1.0 );
    btVector3 inertia;
    masterShape->calculateLocalInertia( mass, inertia );
    btRigidBody::btRigidBodyConstructionInfo rbinfo( mass, motion, masterShape, inertia );
    btRigidBody * body = new btRigidBody( rbinfo );
    body->setLinearVelocity( btVector3( 1, 0, 0 ) );
    body->setAngularVelocity( btVector3( 1, 0, 0 ) );
    dynamicsWorld->addRigidBody( body );
}

int main( int argc,
          char * argv[] )
{
    osg::ref_ptr< osg::Group > root = new osg::Group();

    btDynamicsWorld * dynamicsWorld = initPhysics();


    // Create target
    createTarget( root.get(), dynamicsWorld );


    // Make the ground plane
    osg::MatrixTransform * ground = createOSGBox( osg::Vec3( 10, 10, .01 ) );
    root->addChild( ground );
    btRigidBody * groundBody = createBTBox( ground, btVector3( 0, 0, -10 ) );
    dynamicsWorld->addRigidBody( groundBody );


    // Make animated compound objext.
    osg::MatrixTransform* animObj = createAnimatedObject( osg::Vec3( .5, .5, .5 ) );
    osg::AnimationPathCallback * apc = new osg::AnimationPathCallback(
        createAnimationPath( osg::Vec3( -6, -10, -9 ),
                             osg::Vec3( 16, 19, 0 ), 5 ),
        0, 1 );
    animObj->setUpdateCallback( apc );
    root->addChild( animObj );

    //   Add animated object to Bullet
    btRigidBody * animBody = createBTCompound( animObj, btVector3( -9, -3, -9 ) );
    animBody->setCollisionFlags( animBody->getCollisionFlags() | btCollisionObject::CF_KINEMATIC_OBJECT );
    animBody->setActivationState( DISABLE_DEACTIVATION );
    dynamicsWorld->addRigidBody( animBody );

    //   Add osgBullet code to link OSG and Bullet representations.
    osgBullet::RigidBody* animRB = new osgBullet::RigidBody();
    animRB->setRigidBody( animBody );
    animObj->setUserData( animRB );

    osgBullet::RigidBodyAnimation * rba = new osgBullet::RigidBodyAnimation;
    apc->setNestedCallback( rba );


    osgViewer::Viewer viewer;
    osgGA::TrackballManipulator * tb = new osgGA::TrackballManipulator();
    tb->setHomePosition( osg::Vec3( 20, -24, 3 ),
                        osg::Vec3( 2, 0, -10 ),
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
