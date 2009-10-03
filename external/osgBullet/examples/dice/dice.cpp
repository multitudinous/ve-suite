// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgViewer/Viewer>
#include <osg/MatrixTransform>
#include <osg/ShapeDrawable>
#include <osg/Geode>

#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/Utils.h>

#include <btBulletDynamicsCommon.h>

#include <string>
#include <osg/io_utils>



osg::MatrixTransform*
makeDie( btDynamicsWorld* bw )
{
    osg::MatrixTransform* root = new osg::MatrixTransform;
    osg::Node* node = osgDB::readNodeFile( "dice.osg" );
    root->addChild( node );

    btCollisionShape* cs = osgBullet::btBoxCollisionShapeFromOSG( node );
    osgBullet::MotionState* motion = new osgBullet::MotionState();
    motion->setTransform( root );
    btScalar mass( 1. );
    btVector3 inertia( 0, 0, 0 );
    cs->calculateLocalInertia( mass, inertia );
    btRigidBody::btRigidBodyConstructionInfo rb( mass, motion, cs, inertia );
    btRigidBody* body = new btRigidBody( rb );
    //body->setActivationState( DISABLE_DEACTIVATION );
    bw->addRigidBody( body );

    return( root );
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

    dynamicsWorld->setGravity( btVector3( 0, 0, 9.8 ) );

    return( dynamicsWorld );
}



class ShakeManipulator : public osgGA::GUIEventHandler
{
public:
    ShakeManipulator( osgBullet::MotionState* motion )
      : _motion( motion )
    {}

    bool handle( const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& )
    {
        switch( ea.getEventType() )
        {
            case osgGA::GUIEventAdapter::KEYUP:
            {
                if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Space)
                {
                    btTransform trans; trans.setIdentity();
                    _motion->setWorldTransform( trans );

                    return true;
                }

                return false;
            }

            case osgGA::GUIEventAdapter::PUSH:
            {
                _lastX = ea.getXnormalized();
                _lastY = ea.getYnormalized();

                btTransform world;
                _motion->getWorldTransform( world );
                btVector3 o = world.getOrigin();
                o[ 2 ] = 0.25;
                world.setOrigin( o );
                _motion->setWorldTransform( world );

                return true;
            }
            case osgGA::GUIEventAdapter::DRAG:
            {
                btVector3 move;
                move[ 0 ] = _lastX - ea.getXnormalized();
                move[ 1 ] = ea.getYnormalized() - _lastY;
                move[ 2 ] = 0.;
                move *= 10.;
                btTransform moveTrans; moveTrans.setIdentity();
                moveTrans.setOrigin( move );
                btTransform world;
                _motion->getWorldTransform( world );
                btTransform netTrans = moveTrans * world;
                btVector3 o = netTrans.getOrigin();
                o[ 2 ] = 0.;
                netTrans.setOrigin( o );

                _motion->setWorldTransform( netTrans );

                _lastX = ea.getXnormalized();
                _lastY = ea.getYnormalized();

                return true;
            }
            default:
            break;
        }
        return false;
    }

protected:
    osgBullet::MotionState* _motion;
    float _lastX, _lastY;
};





osg::Geode* osgBox( const osg::Vec3& center, const osg::Vec3& halfLengths )
{
    osg::Vec3 l( halfLengths * 2. );
    osg::Box* box = new osg::Box( center, l.x(), l.y(), l.z() );
    osg::ShapeDrawable* shape = new osg::ShapeDrawable( box );
    shape->setColor( osg::Vec4( 1., 1., 1., 1. ) );
    osg::Geode* geode = new osg::Geode();
    geode->addDrawable( shape );
    return( geode );
}



int
main( int argc,
      char ** argv )
{
    btDynamicsWorld* bulletWorld = initPhysics();
    osg::Group* root = new osg::Group;

    root->addChild( makeDie( bulletWorld ) );
    root->addChild( makeDie( bulletWorld ) );


    /* BEGIN: Create environment boxes */
    float xDim( 10. );
    float yDim( 10. );
    float zDim( 6. );
    float thick( .1 );

    osg::MatrixTransform* shakeBox = new osg::MatrixTransform;
    btCompoundShape* cs = new btCompoundShape;
    { // floor -Z (far back of the shake cube)
        osg::Vec3 halfLengths( xDim*.5, yDim*.5, thick*.5 );
        osg::Vec3 center( 0., 0., zDim*.5 );
        shakeBox->addChild( osgBox( center, halfLengths ) );
        btBoxShape* box = new btBoxShape( osgBullet::asBtVector3( halfLengths ) );
        btTransform trans; trans.setIdentity();
        trans.setOrigin( osgBullet::asBtVector3( center ) );
        cs->addChildShape( trans, box );
    }
    { // top +Z (invisible, to allow user to see through; no OSG analogue
        osg::Vec3 halfLengths( xDim*.5, yDim*.5, thick*.5 );
        osg::Vec3 center( 0., 0., -zDim*.5 );
        //shakeBox->addChild( osgBox( center, halfLengths ) );
        btBoxShape* box = new btBoxShape( osgBullet::asBtVector3( halfLengths ) );
        btTransform trans; trans.setIdentity();
        trans.setOrigin( osgBullet::asBtVector3( center ) );
        cs->addChildShape( trans, box );
    }
    { // left -X
        osg::Vec3 halfLengths( thick*.5, yDim*.5, zDim*.5 );
        osg::Vec3 center( -xDim*.5, 0., 0. );
        shakeBox->addChild( osgBox( center, halfLengths ) );
        btBoxShape* box = new btBoxShape( osgBullet::asBtVector3( halfLengths ) );
        btTransform trans; trans.setIdentity();
        trans.setOrigin( osgBullet::asBtVector3( center ) );
        cs->addChildShape( trans, box );
    }
    { // right +X
        osg::Vec3 halfLengths( thick*.5, yDim*.5, zDim*.5 );
        osg::Vec3 center( xDim*.5, 0., 0. );
        shakeBox->addChild( osgBox( center, halfLengths ) );
        btBoxShape* box = new btBoxShape( osgBullet::asBtVector3( halfLengths ) );
        btTransform trans; trans.setIdentity();
        trans.setOrigin( osgBullet::asBtVector3( center ) );
        cs->addChildShape( trans, box );
    }
    { // bottom of window -Y
        osg::Vec3 halfLengths( xDim*.5, thick*.5, zDim*.5 );
        osg::Vec3 center( 0., -yDim*.5, 0. );
        shakeBox->addChild( osgBox( center, halfLengths ) );
        btBoxShape* box = new btBoxShape( osgBullet::asBtVector3( halfLengths ) );
        btTransform trans; trans.setIdentity();
        trans.setOrigin( osgBullet::asBtVector3( center ) );
        cs->addChildShape( trans, box );
    }
    { // bottom of window -Y
        osg::Vec3 halfLengths( xDim*.5, thick*.5, zDim*.5 );
        osg::Vec3 center( 0., yDim*.5, 0. );
        shakeBox->addChild( osgBox( center, halfLengths ) );
        btBoxShape* box = new btBoxShape( osgBullet::asBtVector3( halfLengths ) );
        btTransform trans; trans.setIdentity();
        trans.setOrigin( osgBullet::asBtVector3( center ) );
        cs->addChildShape( trans, box );
    }
    /* END: Create environment boxes */

    osgBullet::MotionState * shakeMotion = new osgBullet::MotionState();
    shakeMotion->setTransform( shakeBox );
    btScalar mass( 0.0 );
    btVector3 inertia( 0, 0, 0 );
    btRigidBody::btRigidBodyConstructionInfo rb( mass, shakeMotion, cs, inertia );
    btRigidBody* shakeBody = new btRigidBody( rb );
    shakeBody->setCollisionFlags( shakeBody->getCollisionFlags() | btCollisionObject::CF_KINEMATIC_OBJECT );
    shakeBody->setActivationState( DISABLE_DEACTIVATION );
    bulletWorld->addRigidBody( shakeBody );

    root->addChild( shakeBox );

    osgViewer::Viewer viewer;
    viewer.setUpViewInWindow( 150, 150, 400, 400 );
    viewer.setSceneData( root );
    viewer.getCamera()->setViewMatrixAsLookAt(
        osg::Vec3( 0, 0, -20 ), osg::Vec3( 0, 0, 0 ), osg::Vec3( 0, 1, 0 ) );
    viewer.getCamera()->setProjectionMatrixAsPerspective( 40., 1., 1., 50. );
    viewer.addEventHandler( new ShakeManipulator( shakeMotion ) );

    viewer.realize();
    double prevSimTime = 0.;
    while( !viewer.done() )
    {
        const double currSimTime = viewer.getFrameStamp()->getSimulationTime();
        double elapsed( currSimTime - prevSimTime );
        if( viewer.getFrameStamp()->getFrameNumber() < 3 )
            elapsed = 1./60.;
        //osg::notify( osg::ALWAYS ) << elapsed / 3. << ", " << 1./180. << std::endl;
        bulletWorld->stepSimulation( elapsed, 4, elapsed/4. );
        prevSimTime = currSimTime;
        viewer.frame();
    }

    return( 0 );
}
