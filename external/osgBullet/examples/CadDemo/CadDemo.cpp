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
#include <osgBullet/OSGToCollada.h>
#include <osgBullet/HandNode.h>
#include <osgBullet/ColladaUtils.h>
#include <osgBullet/Utils.h>

#include <btBulletDynamicsCommon.h>
#include <BulletColladaConverter/ColladaConverter.h>

#include <list>
#include <map>
#include <string>
#include <sstream>
#include <osg/io_utils>

#include <iostream>



//#define USE_PARALLEL_DISPATCHER
#ifdef USE_PARALLEL_DISPATCHER

#include "BulletMultiThreaded/SpuGatheringCollisionDispatcher.h"

#ifdef USE_LIBSPE2
#include "BulletMultiThreaded/SpuLibspe2Support.h"
#elif defined (WIN32)
#include "BulletMultiThreaded/Win32ThreadSupport.h"
#include "BulletMultiThreaded/SpuNarrowPhaseCollisionTask/SpuGatheringCollisionTask.h"

#elif defined (USE_PTHREADS)

#include "BulletMultiThreaded/PosixThreadSupport.h"
#include "BulletMultiThreaded/SpuNarrowPhaseCollisionTask/SpuGatheringCollisionTask.h"

#else
//other platforms run the parallel code sequentially (until pthread support or other parallel implementation is added)
#include "BulletMultiThreaded/SequentialThreadSupport.h"
#include "BulletMultiThreaded/SpuNarrowPhaseCollisionTask/SpuGatheringCollisionTask.h"
#endif //USE_LIBSPE2

#endif



//#define DO_DEBUG_DRAW
#ifdef DO_DEBUG_DRAW
#include <osgBullet/GLDebugDrawer.h>
#endif


//#define DBG_DUMP 1
#ifdef DBG_DUMP
#include <BulletColladaConverter/ColladaConverter.h>
#endif




btDynamicsWorld* initPhysics( osg::Vec3 gravity = osg::Vec3( 0, 0, -1 ) )
{
    btDefaultCollisionConfiguration * collisionConfiguration = new btDefaultCollisionConfiguration();

#ifdef USE_PARALLEL_DISPATCHER
    btThreadSupportInterface*		threadSupportCollision;
    threadSupportCollision = new Win32ThreadSupport(Win32ThreadSupport::Win32ThreadConstructionInfo(
								"collision",
								processCollisionTask,
								createCollisionLocalStoreMemory,
								2));
    btCollisionDispatcher * dispatcher = new SpuGatheringCollisionDispatcher(
        threadSupportCollision, 2, collisionConfiguration );
#else
    btCollisionDispatcher* dispatcher = new btCollisionDispatcher( collisionConfiguration );
#endif

    btConstraintSolver * solver = new btSequentialImpulseConstraintSolver;

    btVector3 worldAabbMin( -10000, -10000, -10000 );
    btVector3 worldAabbMax( 10000, 10000, 10000 );
    btBroadphaseInterface * inter = new btAxisSweep3( worldAabbMin, worldAabbMax, 1000 );

    btDynamicsWorld* dynamicsWorld = new btDiscreteDynamicsWorld( dispatcher, inter, solver, collisionConfiguration );

    dynamicsWorld->setGravity( osgBullet::asBtVector3( gravity * 9.8 ) );

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
        const bool alt = ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_SHIFT) ||
            (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_SHIFT) );

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
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F6)
                {
                    _mode = osgBullet::HandNode::MAX_ARTICULATIONS;
                    return true;
                }

                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Left)
                {
                    if( _mode == osgBullet::HandNode::MAX_ARTICULATIONS )
                    {
                        osgBullet::HandNode::Articulation art;
                        for( art=osgBullet::HandNode::FINGER_0_TRANSLATE;
                            art<=osgBullet::HandNode::FINGER_4_TRANSLATE; art++ )
                        {
                            _hand->setArticulation( art,
                                _hand->getArticulation( art ) + 0.1 );
                        }
                    }
                    else
                        _hand->setArticulation( _mode,
                            _hand->getArticulation( _mode ) + 0.1 );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Right)
                {
                    if( _mode == osgBullet::HandNode::MAX_ARTICULATIONS )
                    {
                        osgBullet::HandNode::Articulation art;
                        for( art=osgBullet::HandNode::FINGER_0_TRANSLATE;
                            art<=osgBullet::HandNode::FINGER_4_TRANSLATE; art++ )
                        {
                            _hand->setArticulation( art,
                                _hand->getArticulation( art ) - 0.1 );
                        }
                    }
                    else
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
                const bool k0 = ( !k1 || ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_SHIFT) ||
                    (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_SHIFT) ) );

                float delta( 0.05 );
                osgGA::GUIEventAdapter::ScrollingMotion sm = ea.getScrollingMotion();
                if (sm == osgGA::GUIEventAdapter::SCROLL_UP)
                    delta = -delta;

                if( _mode == osgBullet::HandNode::MAX_ARTICULATIONS )
                {
                    osgBullet::HandNode::Articulation art;
                    for( art=osgBullet::HandNode::FINGER_0_TRANSLATE;
                        art<=osgBullet::HandNode::FINGER_4_TRANSLATE; art++ )
                    {
                        if (k0) _hand->setArticulation( art + 5 , _hand->getArticulation( art+5  ) + delta );
                        if (k1) _hand->setArticulation( art + 10, _hand->getArticulation( art+10 ) + delta );
                    }
                }
                else
                {
                    if (k0) _hand->setArticulation( _mode + 5 , _hand->getArticulation( _mode+5  ) + delta );
                    if (k1) _hand->setArticulation( _mode + 10, _hand->getArticulation( _mode+10 ) + delta );
                }
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

    typedef std::pair< osg::Node*, osg::NodePath > NodeAndPath;
    typedef std::vector< NodeAndPath > NodeAndPathList;
    NodeAndPathList _napl;

    void reset()
    {
        _napl.clear();
    }

    void apply( osg::Node& node )
    {
        if( node.getName() == _name )
        {
            NodeAndPath nap( &node, getNodePath() );
            _napl.push_back( nap );
        }
        traverse( node );
    }

protected:
    std::string _name;
};


// This class can read and write itself to a file. Long-term,
// we might want something like this in an osgPlugin form.
class ConfigReaderWriter : public osg::Referenced
{
public:
    ConfigReaderWriter( btDynamicsWorld* dw )
      : _dw( dw ),
      _up( osg::Vec3( 0, 0, -1 ) )
    {}

    bool read( const std::string& fileName )
    {
        std::string inFile( osgDB::findDataFile( fileName ) );
        std::ifstream in( inFile.c_str() );
        if( !in.good() )
        {
            osg::notify( osg::FATAL ) << "CondigReaderWriter::read: Can't open " << fileName << std::endl;
            return false;
        }

        std::string node, dae;
        char bufCh[ 1024 ];
        in.getline( bufCh, 1024 );
        std::string buf( bufCh );
        while( !in.eof() )
        {
            osg::notify( osg::DEBUG_INFO ) << "Data: " << buf << std::endl;
            int spacePos( buf.find_first_of( " " ) );
            std::string key = buf.substr( 0, spacePos );
            osg::notify( osg::DEBUG_INFO ) << spacePos << " KEY " << key << std::endl;
            if( (spacePos == std::string::npos) ||
                (key == std::string( "#" )) )
            {
                in.getline( bufCh, 1024 );
                buf = std::string( bufCh );
                continue;
            }
            std::istringstream istr( buf.substr( spacePos ) );
            if( key == std::string( "Up:" ) )
            {
                istr >> _up;
            }
            else if( key == std::string( "HandNode:" ) )
            {
                osg::Vec3 pos;
                osg::Quat quat;
                float length;
                bool right;
                istr >> pos >> quat >> length >> right;
                if( !_hn.valid() )
                {
                    osg::notify( osg::DEBUG_INFO ) << "  New HandNode " << length << " " << right << std::endl;
                    _hn = new osgBullet::HandNode( _dw,
                        (right ? osgBullet::HandNode::RIGHT : osgBullet::HandNode::LEFT), length );
                }
                _hn->setPosition( pos );
                _hn->setAttitude( quat );
            }
            else if( key == std::string( "Model:" ) )
            {
                istr >> _model;
            }
            else if( key == std::string( "Node:" ) )
            {
                istr >> node;
            }
            else if( key == std::string( "DAE:" ) )
            {
                istr >> dae;
                _nodeDaeMap[ node ] = dae;
            }
            else if( key == std::string( "CreateDAE:" ) )
            {
                istr >> node;
                _nodeCreateList.push_back( node );
            }
            else if( key == std::string( "CreateDAE-tm:" ) )
            {
                istr >> node;
                _nodeCreateTMList.push_back( node );
            }
            else
                osg::notify( osg::WARN ) << "ConfigReaderWriter: Unknown key: " << key << std::endl;

            in.getline( bufCh, 1024 );
            buf = std::string( bufCh );
        }

        return true;
    }
    bool write( const std::string& fileName )
    {
        std::ofstream out( fileName.c_str() );
        if( !out.good() )
        {
            osg::notify( osg::FATAL ) << "CondigReaderWriter::write: Can't open " << fileName << std::endl;
            return false;
        }
        out << "HandNode: " << _hn->getPosition() << " " <<
            _hn->getAttitude() << " " <<
            _hn->getHandLength() << " " <<
            (_hn->getHandedness() == osgBullet::HandNode::RIGHT) << std::endl;
        out << "Model: " << _model << std::endl;
        NodeDaeMap::const_iterator itr;
        for( itr = _nodeDaeMap.begin(); itr != _nodeDaeMap.end(); itr++ )
        {
            out << "Node: " << itr->first << std::endl;
            out << "DAE: " << itr->second << std::endl;
        }
        return true;
    }

    osg::ref_ptr< osgBullet::HandNode > _hn;
    std::string _model;

    typedef std::map< std::string, std::string > NodeDaeMap;
    NodeDaeMap _nodeDaeMap;
    typedef std::vector< std::string > NodeCreateList;
    NodeCreateList _nodeCreateList;
    NodeCreateList _nodeCreateTMList;
    osg::Vec3 _up;

protected:
    ~ConfigReaderWriter() {}

    btDynamicsWorld* _dw;
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


int
main( int argc,
      char ** argv )
{
    if( argc < 2 )
    {
        osg::notify( osg::FATAL ) << "Specify a config file (such as door.txt) on the command line." << std::endl;
        return 1;
    }

    btDynamicsWorld* bulletWorld = initPhysics();
    osg::Group* root = new osg::Group;


    ConfigReaderWriter* crw = new ConfigReaderWriter( bulletWorld );
    crw->read( std::string( argv[ 1 ] ) );

    osg::Vec3& up( crw->_up );
    bulletWorld->setGravity( osgBullet::asBtVector3( up * -9.8 ) );

    //osg::Quat q( osg::PI, osg::Vec3f( 0, 1, 0 ) );
    //crw->_hn->setAttitude( q );
    //crw->_hn->setPosition( osg::Vec3( -2, 6, -3 ) );
    //crw->write( "C:\\Projects\\Physics\\physics_hg\\data\\door.txt" );


    osg::ref_ptr< osgBullet::HandNode > hn = crw->_hn.get();
    root->addChild( hn.get() );


    osg::ref_ptr< osg::Node > model = osgDB::readNodeFile( crw->_model );
    osg::Matrix m( osg::Matrix::translate( osg::Vec3( 0, 0, 7*up[2] ) ) );
    osg::ref_ptr< osg::MatrixTransform > orient = new osg::MatrixTransform( m );
    orient->addChild( model.get() );
    root->addChild( orient.get() );


    ConfigReaderWriter::NodeDaeMap::const_iterator itr;
    for( itr = crw->_nodeDaeMap.begin(); itr != crw->_nodeDaeMap.end(); itr++ )
    {
        FindNamedNode fnn( itr->first );
        orient->accept( fnn );
        osg::notify( osg::ALWAYS ) << "Adding " << fnn._napl.size() << " Instances of " << itr->first << std::endl;

        unsigned int idx;
        for( idx=0; idx<fnn._napl.size(); idx++ )
        {
            FindNamedNode::NodeAndPath& nap( fnn._napl[ idx ] );
            osg::Node* subgraph = nap.first;
            osg::NodePath& np = nap.second;
            osgBullet::AbsoluteModelTransform* amt = new osgBullet::AbsoluteModelTransform;
            osg::Group* parent = subgraph->getParent( 0 );
            parent->addChild( amt );
            amt->addChild( subgraph );
            parent->removeChild( subgraph );
            osgBullet::loadDae( amt, np, itr->second, bulletWorld );
        }
    }

    if( crw->_nodeCreateList.size() > 0 )
        osg::notify( osg::ALWAYS ) << "Must create physics data..." << std::endl;
    ConfigReaderWriter::NodeCreateList::const_iterator vitr;
    for( vitr = crw->_nodeCreateList.begin(); vitr != crw->_nodeCreateList.end(); vitr++ )
    {
        osg::notify( osg::ALWAYS ) << "  Creating physics data for: " << *vitr << std::endl;
        FindNamedNode fnn( *vitr );
        orient->accept( fnn );
        osg::Node* subgraph = fnn._napl[ 0 ].first;
        osg::NodePath& np = fnn._napl[ 0 ].second;

        osgBullet::OSGToCollada converter;
        converter.setSceneGraph( subgraph );
        converter.setShapeType( BOX_SHAPE_PROXYTYPE );
        converter.setMass( 1.0 );
        converter.setOverall( false );
        converter.convert();

        osg::ref_ptr< osgBullet::AbsoluteModelTransform > amt = new osgBullet::AbsoluteModelTransform();
        amt->setDataVariance( osg::Object::DYNAMIC );
        amt->addChild( subgraph );
        osg::Group* parent = subgraph->getParent( 0 );
        parent->addChild( amt.get() );
        parent->removeChild( subgraph );

        btRigidBody* rb = converter.getRigidBody();
        osgBullet::MotionState* motion = new osgBullet::MotionState;
        motion->setTransform( amt.get() );
        osg::BoundingSphere bs = subgraph->getBound();
        rb->setActivationState( DISABLE_DEACTIVATION );

        osg::Matrix m = osg::computeLocalToWorld( np );
        motion->setParentTransform( m );
        motion->setCenterOfMass( bs.center() );
        rb->setMotionState( motion );
        bulletWorld->addRigidBody( rb );
    }

    if( crw->_nodeCreateTMList.size() > 0 )
        osg::notify( osg::ALWAYS ) << "Must create TM physics data..." << std::endl;
    for( vitr = crw->_nodeCreateTMList.begin(); vitr != crw->_nodeCreateTMList.end(); vitr++ )
    {
        osg::notify( osg::ALWAYS ) << "  Creating TM physics data for: " << *vitr << std::endl;
        FindNamedNode fnn( *vitr );
        orient->accept( fnn );
        osg::Node* subgraph = fnn._napl[ 0 ].first;
        osg::NodePath& np = fnn._napl[ 0 ].second;

        osgBullet::OSGToCollada converter;
        converter.setSceneGraph( subgraph );
        converter.setShapeType( TRIANGLE_MESH_SHAPE_PROXYTYPE );
        converter.setMass( 1.0 );
        converter.setOverall( false );
        converter.convert();

        osg::ref_ptr< osgBullet::AbsoluteModelTransform > amt = new osgBullet::AbsoluteModelTransform();
        amt->setDataVariance( osg::Object::DYNAMIC );
        amt->addChild( subgraph );
        osg::Group* parent = subgraph->getParent( 0 );
        parent->addChild( amt.get() );
        parent->removeChild( subgraph );

        btRigidBody* rb = converter.getRigidBody();
        osgBullet::MotionState* motion = new osgBullet::MotionState;
        motion->setTransform( amt.get() );
        osg::BoundingSphere bs = subgraph->getBound();
        rb->setActivationState( DISABLE_DEACTIVATION );

        osg::Matrix m = osg::computeLocalToWorld( np );
        motion->setParentTransform( m );
        motion->setCenterOfMass( bs.center() );
        rb->setMotionState( motion );
        bulletWorld->addRigidBody( rb );
    }


    float thin = .1;
    osg::MatrixTransform* ground = createOSGBox( osg::Vec3( 10, 10, thin ) );
    root->addChild( ground );
    btRigidBody* groundBody = createBTBox( ground, osg::Vec3( 0, 0, -1*up[2]*thin ) );
    bulletWorld->addRigidBody( groundBody );


    osgViewer::Viewer viewer;
    viewer.setSceneData( root );
    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
    tb->setHomePosition( osg::Vec3( 0, 35, 0 ), osg::Vec3( 0, 0, 0 ), up );
    viewer.setCameraManipulator( tb );
    viewer.addEventHandler( new HandManipulator( hn.get() ) );

    double currSimTime = viewer.getFrameStamp()->getSimulationTime();
    double prevSimTime = viewer.getFrameStamp()->getSimulationTime();
    viewer.realize();
    int count( 4 );

#ifdef DO_DEBUG_DRAW
    osgBullet::GLDebugDrawer* dbgDraw = new osgBullet::GLDebugDrawer( root );
    dbgDraw->setDebugMode( ~btIDebugDraw::DBG_DrawText );
    bulletWorld->setDebugDrawer( dbgDraw );
#endif

#ifdef DBG_DUMP
    bool first( true );
#endif

    while( /*count-- &&*/ !viewer.done() )
    {
#ifdef DO_DEBUG_DRAW
        dbgDraw->BeginDraw();
#endif

        currSimTime = viewer.getFrameStamp()->getSimulationTime();
        bulletWorld->stepSimulation( currSimTime - prevSimTime );
        float dt = currSimTime - prevSimTime;
        prevSimTime = currSimTime;

#ifdef DO_DEBUG_DRAW
        bulletWorld->debugDrawWorld();
        dbgDraw->EndDraw();
#endif

        viewer.frame();

#ifdef DBG_DUMP
        if( first )
        {
            first = false;

            osg::notify( osg::ALWAYS ) << "DBG_DUMP: Writing to .dae" << std::endl;
            ColladaConverter* cc = new ColladaConverter( bulletWorld );
            cc->save( "dump.dae" );
            delete cc;
            osg::notify( osg::ALWAYS ) << "DBG_DUMP: Success" << std::endl;
        }
#endif
    }

    return( 0 );
}

