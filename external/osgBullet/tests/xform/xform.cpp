// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix  Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgDB/FileUtils>
#include <osgViewer/Viewer>
#include <osg/MatrixTransform>
#include <osgGA/TrackballManipulator>
#include <osg/ShapeDrawable>
#include <osg/Geode>
#include <osg/PolygonMode>
#include <osg/PolygonOffset>

#include <osgBullet/CollisionShape.h>
#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/AbsoluteModelTransform.h>
#include <osgBullet/HandNode.h>
#include <osgBullet/DebugBullet.h>
#include <osgBullet/ColladaUtils.h>
#include <osgBullet/Utils.h>

#include <btBulletDynamicsCommon.h>
#include <BulletColladaConverter/ColladaConverter.h>
#include <BulletColladaConverter/ColladaConverter.h>

#include <list>
#include <map>
#include <string>
#include <sstream>
#include <osg/io_utils>

//#define NO_PHYSICS

osg::Node*
makeScene()
{
    osg::Group* root = new osg::Group;

    osg::Matrix m(
        osg::Matrix::translate( osg::Vec3( 1, 1, 0 ) ) *
        osg::Matrix::rotate( .7, osg::Vec3( .707, .707, 0 ) )
        );
    osg::MatrixTransform* mt = new osg::MatrixTransform( m );
    mt->setName( "CubeParentTransform" );
    mt->addChild( osgDB::readNodeFile( "offcube.osg" ) );
    root->addChild( mt );

    osg::Geode* geode = new osg::Geode;
    geode->addDrawable(
        osg::createTexturedQuadGeometry( osg::Vec3( 1., 1., 1. ),
            osg::Vec3( 2., 0., 0. ), osg::Vec3( 0., 0., 2. ) ) );
    geode->addDrawable(
        osg::createTexturedQuadGeometry( osg::Vec3( -3., 1., 1. ),
            osg::Vec3( 2., 0., 0. ), osg::Vec3( 0., 0., 2. ) ) );
    geode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    root->addChild( geode );

    return( (osg::Node*) root );
}


osgBullet::DebugBullet _debugBullet;


btDynamicsWorld* initPhysics()
{
    btDefaultCollisionConfiguration * collisionConfiguration = new btDefaultCollisionConfiguration();
    btCollisionDispatcher * dispatcher = new btCollisionDispatcher( collisionConfiguration );
    btConstraintSolver * solver = new btSequentialImpulseConstraintSolver;

    btVector3 worldAabbMin( -10000, -10000, -10000 );
    btVector3 worldAabbMax( 10000, 10000, 10000 );
    btBroadphaseInterface * inter = new btAxisSweep3( worldAabbMin, worldAabbMax, 1000 );

    btDynamicsWorld * dynamicsWorld = new btDiscreteDynamicsWorld( dispatcher, inter, solver, collisionConfiguration );

    dynamicsWorld->setGravity( btVector3( 0, 0, -9.8 ) );

    return( dynamicsWorld );
}



class HandManipulator : public osgGA::GUIEventHandler
{
public:
    HandManipulator( osgBullet::HandNode* hn )
        : _hand( hn ),
        _mode( osgBullet::HandNode::FINGER_0_TRANSLATE ) {}

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
                    _hand->setPose( osgBullet::HandNode::POSE_DEFAULT );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_End)
                {
                    _hand->setPose( osgBullet::HandNode::POSE_HOOK );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Page_Up)
                {
                    _hand->setPose( osgBullet::HandNode::POSE_POINT );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Page_Down)
                {
                    _hand->setPose( osgBullet::HandNode::POSE_FIST );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Delete)
                {
                    _hand->dump();
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F1)
                {
                    _mode = osgBullet::HandNode::FINGER_0_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F2)
                {
                    _mode = osgBullet::HandNode::FINGER_1_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F3)
                {
                    _mode = osgBullet::HandNode::FINGER_2_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F4)
                {
                    _mode = osgBullet::HandNode::FINGER_3_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F5)
                {
                    _mode = osgBullet::HandNode::FINGER_4_TRANSLATE;
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
                osg::Vec3 tmove = q * move * 5.f;
                _hand->setPosition( tmove + _hand->getPosition() );
                return true;
            }
            default:
            break;
        }
        return false;
    }

protected:
    osg::ref_ptr< osgBullet::HandNode > _hand;
    osgBullet::HandNode::Articulation _mode;
    float _lastX, _lastY;
};

class FindNamedNode : public osg::NodeVisitor
{
public:
    FindNamedNode( std::string name )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _name( name )
    {}
    osg::ref_ptr< osg::Node > _node;
    osg::NodePath _np;

    void reset()
    {
        _node = NULL;
        _np.clear();
    }

    void apply( osg::Node& node )
    {
        if( node.getName() == _name )
        {
            _node = &node;
            _np = getNodePath();
            osg::notify( osg::ALWAYS ) << "Found " << _name << std::endl;
        }
        else
            traverse( node );
    }

protected:
    std::string _name;
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

    osg::Node* dbgGround = osgBullet::osgNodeFromBtCollisionShape( collision );
    if( dbgGround )
    {
        osg::MatrixTransform* dmt = new osg::MatrixTransform;
        dmt->addChild( dbgGround );
        motion->setDebugTransform( dmt );
        _debugBullet.addDynamic( dmt );
    }
    motion->setTransform( box );
    osg::Matrix groundTransform( osg::Matrix::translate( center ) );
    motion->setParentTransform( groundTransform );
    body->setMotionState( motion );

    return( body );
}


int
main( int argc,
      char ** argv )
{
    btDynamicsWorld* bulletWorld = initPhysics();
    osg::Group* root = new osg::Group;
    root->addChild( _debugBullet.getRoot() );


    osg::ref_ptr< osgBullet::HandNode > hn = new osgBullet::HandNode( bulletWorld, osgBullet::HandNode::RIGHT, 2. );
    hn->setPosition( osg::Vec3( 3., -3., 0. ) );
    osg::Quat q( osg::PI, osg::Vec3( 0., 0., 1. ) );
    hn->setAttitude( q );
    root->addChild( hn.get() );


    osg::ref_ptr< osg::Node > model = makeScene();
    osg::Matrix m( /*osg::Matrix::rotate( osg::PI, osg::Vec3( 1,0,0 ) ) * */
        osg::Matrix::translate( osg::Vec3( 0, 0, 0 ) ) );
    osg::ref_ptr< osg::MatrixTransform > orient = new osg::MatrixTransform( m );
    orient->addChild( model.get() );
    root->addChild( orient.get() );


    // Find node of interest, insert AMT above it,
    // and load DAE for it.
    FindNamedNode fnn( "OffOriginCube" );
    orient->accept( fnn );
    osgBullet::AbsoluteModelTransform* amt = new osgBullet::AbsoluteModelTransform;
    osg::Group* parent = fnn._node->getParent( 0 );
    parent->addChild( amt );
    amt->addChild( fnn._node.get() );
    parent->removeChild( fnn._node.get() );
    osgBullet::loadDae( amt, fnn._np, "offCube0.dae", bulletWorld, &_debugBullet );
#ifdef NO_PHYSICS
    amt->setReferenceFrame( osg::Transform::RELATIVE_RF );
    amt->setMatrix( osg::Matrix::identity() );
#endif


    float thin = .1;
    osg::MatrixTransform* ground = createOSGBox( osg::Vec3( 10, 10, thin ) );
    root->addChild( ground );
    btRigidBody* groundBody = createBTBox( ground, osg::Vec3( 0, 0, -3.-thin ) );
    bulletWorld->addRigidBody( groundBody );


    osgViewer::Viewer viewer;
    viewer.setUpViewOnSingleScreen( 0 );
    viewer.setSceneData( root );
    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
    tb->setHomePosition( osg::Vec3( 5, -22, 5 ), osg::Vec3( 1, 0, 1 ), osg::Vec3( 0,0,1) );
    viewer.setCameraManipulator( tb );
    viewer.addEventHandler( new HandManipulator( hn.get() ) );

    osgDB::writeNodeFile( *root, "/tmp/out.osg" );

    double currSimTime = viewer.getFrameStamp()->getSimulationTime();
    double prevSimTime = viewer.getFrameStamp()->getSimulationTime();
    viewer.realize();
    while( !viewer.done() )
    {
        currSimTime = viewer.getFrameStamp()->getSimulationTime();
#ifndef NO_PHYSICS
        bulletWorld->stepSimulation( currSimTime - prevSimTime );
#endif
        prevSimTime = currSimTime;
        viewer.frame();
    }

    return( 0 );
}

