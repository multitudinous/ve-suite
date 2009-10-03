// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgViewer/Viewer>
#include <osg/MatrixTransform>
#include <osgGA/TrackballManipulator>
#include <osg/ShapeDrawable>
#include <osg/Geode>
#include <osg/PolygonMode>
#include <osg/PolygonOffset>

#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>
#include <osgTools/AbsoluteModelTransform.h>
#include <osgBulletPlus/HandNode.h>
#include <osgBullet/Utils.h>

#include <osgBullet/TripleBuffer.h>
#include <osgBullet/PhysicsThread.h>

#include <btBulletDynamicsCommon.h>


osgBullet::TripleBuffer tBuf;
osgBullet::MotionStateList msl;



class HandManipulator : public osgGA::GUIEventHandler
{
public:
    HandManipulator( osgBulletPlus::HandNode* hn )
        : _hand( hn ),
        _mode( osgBulletPlus::HandNode::FINGER_0_TRANSLATE ) {}

    bool handle( const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& )
    {
        const unsigned int mod = ea.getModKeyMask();
        const bool ctrl = ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_CTRL) ||
            (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_CTRL) );
        const bool alt = ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_ALT) ||
            (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_ALT) );

        const unsigned int buttonMask( ea.getButtonMask() );
        const bool ourLeft( (ctrl || alt) && (buttonMask == osgGA::GUIEventAdapter::LEFT_MOUSE_BUTTON) );

        switch( ea.getEventType() )
        {
            case osgGA::GUIEventAdapter::KEYUP:
            {
                if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Home)
                {
                    _hand->setPose( osgBulletPlus::HandNode::POSE_DEFAULT );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_End)
                {
                    _hand->setPose( osgBulletPlus::HandNode::POSE_HOOK );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Page_Up)
                {
                    _hand->setPose( osgBulletPlus::HandNode::POSE_POINT );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Page_Down)
                {
                    _hand->setPose( osgBulletPlus::HandNode::POSE_FIST );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Delete)
                {
                    _hand->dump();
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F1)
                {
                    _mode = osgBulletPlus::HandNode::FINGER_0_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F2)
                {
                    _mode = osgBulletPlus::HandNode::FINGER_1_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F3)
                {
                    _mode = osgBulletPlus::HandNode::FINGER_2_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F4)
                {
                    _mode = osgBulletPlus::HandNode::FINGER_3_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F5)
                {
                    _mode = osgBulletPlus::HandNode::FINGER_4_TRANSLATE;
                    return true;
                }

                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Left)
                {
                    _hand->setArticulation( _mode,
                        _hand->getArticulation( _mode ) + 0.1 );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Right)
                {
                    _hand->setArticulation( _mode,
                        _hand->getArticulation( _mode ) - 0.1 );
                    return true;
                }
                return false;
            }

            case osgGA::GUIEventAdapter::SCROLL:
            {
                const unsigned int mod = ea.getModKeyMask();
                const bool k1 = ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_CTRL) ||
                    (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_CTRL) );
                const bool k0 = ( !k1 || ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_ALT) ||
                    (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_ALT) ) );

                float delta( 0.05 );
                osgGA::GUIEventAdapter::ScrollingMotion sm = ea.getScrollingMotion();
                if (sm == osgGA::GUIEventAdapter::SCROLL_UP)
                    delta = -delta;

                if (k0) _hand->setArticulation( _mode + 5 , _hand->getArticulation( _mode+5  ) + delta );
                if (k1) _hand->setArticulation( _mode + 10, _hand->getArticulation( _mode+10 ) + delta );
                return true;
            }
            case osgGA::GUIEventAdapter::PUSH:
            {
                if( !ourLeft )
                    return false;

                _lastX = ea.getXnormalized();
                _lastY = ea.getYnormalized();
                return true;
            }
            case osgGA::GUIEventAdapter::DRAG:
            {
                if( !ourLeft )
                    return false;

                osg::Vec3 move;
                if( ctrl )
                {
                    move[ 0 ] = _lastX - ea.getXnormalized();
                    move[ 1 ] = _lastY - ea.getYnormalized();
                }
                else if( alt )
                    move[ 2 ] = ea.getYnormalized() - _lastY;
                _lastX = ea.getXnormalized();
                _lastY = ea.getYnormalized();

                osg::Quat q = _hand->getAttitude();
                osg::Vec3 tmove = q * move * 3.f;
                _hand->setPosition( tmove + _hand->getPosition() );
                return true;
            }
            default:
            break;
        }
        return false;
    }

protected:
    osg::ref_ptr< osgBulletPlus::HandNode > _hand;
    osgBulletPlus::HandNode::Articulation _mode;
    float _lastX, _lastY;
};


// Creates a Bullet collision shape from an osg::Shape.
class BulletShapeVisitor : public osg::ConstShapeVisitor
{
public:
    BulletShapeVisitor() : _shape( NULL ) {}
    virtual ~BulletShapeVisitor() {}

    virtual void apply( const osg::Shape& s )
    {
        osg::notify( osg::ALWAYS ) << "Unknown shape." << std::endl;
    }
    virtual void apply( const osg::Sphere& s )
    {
        osg::Vec3 c = s.getCenter();
        float radius = s.getRadius();

        btSphereShape* collision = new btSphereShape( radius );
        btTransform xform;
        xform.setIdentity();
        xform.setOrigin( osgBullet::asBtVector3( c ) );

        _shape = new btCompoundShape;
        _shape->addChildShape( xform, collision );
    }
    virtual void apply( const osg::Box& s )
    {
        osg::notify( osg::ALWAYS ) << "Found Box." << std::endl;

        osg::Vec3 c = s.getCenter();
        osg::Vec3 sizes = s.getHalfLengths();

        btBoxShape* collision = new btBoxShape( btVector3( sizes.x(), sizes.y(), sizes.z() ) );
        btTransform xform;
        xform.setIdentity();
        xform.setOrigin( osgBullet::asBtVector3( c ) );

        _shape = new btCompoundShape;
        _shape->addChildShape( xform, collision );
    }
    virtual void apply( const osg::Cylinder& s )
    {
        osg::notify( osg::ALWAYS ) << "Found Cylinder." << std::endl;

        osg::Vec3 c = s.getCenter();
        float radius = s.getRadius();
        float height = s.getHeight();

        btCylinderShape* collision = new btCylinderShapeZ( btVector3( radius, 0., height * .5 ) );
        btTransform xform;
        xform.setIdentity();
        //xform.setOrigin( osgBullet::asBtVector3( c ) );

        _shape = new btCompoundShape;
        _shape->addChildShape( xform, collision );
    }

    btCompoundShape* _shape;
};



osg::MatrixTransform* createOSGBox( osg::Vec3 size )
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

btRigidBody * createBTBox( osg::MatrixTransform* box,
                          osg::Vec3 center )
{
    btCollisionShape* collision = osgBullet::btBoxCollisionShapeFromOSG( box );

    osgBullet::MotionState * motion = new osgBullet::MotionState();

    btScalar mass( 0.0 );
    btVector3 inertia( 0, 0, 0 );
    btRigidBody::btRigidBodyConstructionInfo rb( mass, motion, collision, inertia );
    btRigidBody * body = new btRigidBody( rb );

    motion->setTransform( box );
    osg::Matrix groundTransform( osg::Matrix::translate( center ) );
    motion->setParentTransform( groundTransform );
    body->setMotionState( motion );

    return( body );
}

osg::Transform*
createBall( btDynamicsWorld* dynamicsWorld )
{
    osg::Sphere* sp = new osg::Sphere( osg::Vec3( 0., 0., 0. ), 1. );
    osg::ShapeDrawable* shape = new osg::ShapeDrawable( sp );
    osg::Geode* geode = new osg::Geode();
    geode->addDrawable( shape );
    osg::ref_ptr< osgTools::AbsoluteModelTransform > amt = new osgTools::AbsoluteModelTransform;
    amt->addChild( geode );

    BulletShapeVisitor bsv;
    sp->accept( bsv );
    btCollisionShape* collision = bsv._shape;

    osgBullet::MotionState* motion = new osgBullet::MotionState;
    motion->setTransform( amt.get() );

    // Debug OSG rep of bullet shape.
    osg::Node* debugNode = osgBullet::osgNodeFromBtCollisionShape( collision );
    amt->addChild( debugNode );

    motion->setParentTransform( osg::Matrix::translate( osg::Vec3( -10, 0, 10 ) ) );

    btScalar mass( 1. );
    btVector3 inertia;
    collision->calculateLocalInertia( mass, inertia );
    btRigidBody::btRigidBodyConstructionInfo rbinfo( mass, motion, collision, inertia );
    btRigidBody * body = new btRigidBody( rbinfo );
    body->setLinearVelocity( btVector3( 5, -.1, 0 ) );
    body->setActivationState( DISABLE_DEACTIVATION );
    dynamicsWorld->addRigidBody( body );

    // Set up for triple buffering.
    motion->registerTripleBuffer( &tBuf );
    msl.push_back( motion );

    return( amt.release() );
}

btDynamicsWorld* initPhysics()
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

int
main( int argc,
      char ** argv )
{
    btDynamicsWorld* bulletWorld = initPhysics();
    osgBullet::PhysicsThread pt( bulletWorld, &tBuf );

    osg::Group* root = new osg::Group;

    if( argc > 1 )
        root->addChild( osgDB::readNodeFile( argv[ 1 ] ) );

    root->addChild( createBall( bulletWorld ) );

    float thin = .01;
    osg::MatrixTransform* ground = createOSGBox( osg::Vec3( 10, 10, thin ) );
    root->addChild( ground );
    btRigidBody* groundBody = createBTBox( ground, osg::Vec3( 0, 0, -1 ) );
    bulletWorld->addRigidBody( groundBody );

    osg::ref_ptr< osgBulletPlus::HandNode > hn = new osgBulletPlus::HandNode(
        bulletWorld, osgBulletPlus::HandNode::LEFT, 2.2 );
    root->addChild( hn.get() );
    hn->registerMultiThreaded( &pt );

    hn->setPosition( osg::Vec3( 4., -.6, -.2 ) );
    osg::Quat quat( -1.5, osg::Vec3( .1, 0., 1. ) );
    hn->setAttitude( quat );
    //hn->setDebug( true );

    osgViewer::Viewer viewer;
    viewer.setUpViewInWindow( 10, 30, 800, 600 );
    viewer.setSceneData( root );
    viewer.setCameraManipulator( new osgGA::TrackballManipulator );
    viewer.addEventHandler( new HandManipulator( hn.get() ) );

    viewer.realize();
    pt.setProcessorAffinity( 0 );
    pt.start();

    while( !viewer.done() )
    {
        TripleBufferMotionStateUpdate( msl, &tBuf );

        viewer.frame();
    }

    pt.stopPhysics();
    while( pt.isRunning() )
        OpenThreads::Thread::microSleep( 100 );

    return( 0 );
}

