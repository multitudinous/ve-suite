// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgDB/FileUtils>
#include <osgViewer/Viewer>
#include <osg/BoundingSphere>
#include <osg/MatrixTransform>
#include <osgGA/TrackballManipulator>
#include <osg/ShapeDrawable>
#include <osg/Geode>
#include <osg/PolygonMode>
#include <osg/PolygonOffset>

#include <osgwTools/FindNamedNode.h>

#include <osgbBullet/RefRigidBody.h>
#include <osgbBullet/MotionState.h>
#include <osgbBullet/CollisionShapes.h>
#include <osgbBullet/OSGToCollada.h>
#include <osgbBulletPlus/HandNode.h>
#include <osgbBullet/ColladaUtils.h>
#include <osgbBullet/Utils.h>

#include <osgwTools/AbsoluteModelTransform.h>

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
#include <osgbBullet/GLDebugDrawer.h>
#endif


//#define DBG_DUMP 1
#ifdef DBG_DUMP
#include <BulletColladaConverter/ColladaConverter.h>
#endif




btDiscreteDynamicsWorld* initPhysics( osg::Vec3 gravity = osg::Vec3( 0, 0, -1 ) )
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

    btDiscreteDynamicsWorld* dynamicsWorld = new btDiscreteDynamicsWorld( dispatcher, inter, solver, collisionConfiguration );

    dynamicsWorld->setGravity( osgbBullet::asBtVector3( gravity * 9.8 ) );

    return( dynamicsWorld );
}



class HandManipulator : public osgGA::GUIEventHandler
{
public:
    HandManipulator( osgbBulletPlus::HandNode* hn )
        : _hand( hn ),
        _mode( osgbBulletPlus::HandNode::FINGER_0_TRANSLATE ) {}

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
                    _hand->setPose( osgbBulletPlus::HandNode::POSE_DEFAULT );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_End)
                {
                    _hand->setPose( osgbBulletPlus::HandNode::POSE_HOOK );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Page_Up)
                {
                    _hand->setPose( osgbBulletPlus::HandNode::POSE_POINT );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Page_Down)
                {
                    _hand->setPose( osgbBulletPlus::HandNode::POSE_FIST );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Delete)
                {
                    _hand->dump();
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F1)
                {
                    _mode = osgbBulletPlus::HandNode::FINGER_0_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F2)
                {
                    _mode = osgbBulletPlus::HandNode::FINGER_1_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F3)
                {
                    _mode = osgbBulletPlus::HandNode::FINGER_2_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F4)
                {
                    _mode = osgbBulletPlus::HandNode::FINGER_3_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F5)
                {
                    _mode = osgbBulletPlus::HandNode::FINGER_4_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F6)
                {
                    _mode = osgbBulletPlus::HandNode::MAX_ARTICULATIONS;
                    return true;
                }

                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Left)
                {
                    if( _mode == osgbBulletPlus::HandNode::MAX_ARTICULATIONS )
                    {
                        osgbBulletPlus::HandNode::Articulation art;
                        for( art=osgbBulletPlus::HandNode::FINGER_0_TRANSLATE;
                            art<=osgbBulletPlus::HandNode::FINGER_4_TRANSLATE; art++ )
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
                    if( _mode == osgbBulletPlus::HandNode::MAX_ARTICULATIONS )
                    {
                        osgbBulletPlus::HandNode::Articulation art;
                        for( art=osgbBulletPlus::HandNode::FINGER_0_TRANSLATE;
                            art<=osgbBulletPlus::HandNode::FINGER_4_TRANSLATE; art++ )
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

                if( _mode == osgbBulletPlus::HandNode::MAX_ARTICULATIONS )
                {
                    osgbBulletPlus::HandNode::Articulation art;
                    for( art=osgbBulletPlus::HandNode::FINGER_0_TRANSLATE;
                        art<=osgbBulletPlus::HandNode::FINGER_4_TRANSLATE; art++ )
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
    osg::ref_ptr< osgbBulletPlus::HandNode > _hand;
    osgbBulletPlus::HandNode::Articulation _mode;
    float _lastX, _lastY;
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

        short filterGroup( 0 );
        short filterWith( 0 );
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
                    _hn = new osgbBulletPlus::HandNode( _dw,
                        (right ? osgbBulletPlus::HandNode::RIGHT : osgbBulletPlus::HandNode::LEFT), length );
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
                NodeInfo ni;
                ni._name = node;
                ni._filterGroup = filterGroup;
                ni._filterWith = filterWith;
                istr >> dae;
                _nodeDaeMap[ ni ] = dae;
            }
            else if( key == std::string( "CreateDAE:" ) )
            {
                istr >> node;
                NodeInfo ni;
                ni._name = node;
                ni._filterGroup = filterGroup;
                ni._filterWith = filterWith;
                _nodeCreateList.push_back( ni );
            }
            else if( key == std::string( "CreateDAE-tm:" ) )
            {
                istr >> node;
                NodeInfo ni;
                ni._name = node;
                ni._filterGroup = filterGroup;
                ni._filterWith = filterWith;
                _nodeCreateTMList.push_back( ni );
            }

            else if( key == std::string( "Hinge:" ) )
            {
                HingeInfo hi;
                istr >> hi._nodeA >> hi._nodeB >> hi._pivotA >> hi._pivotB >> hi._axisA >> hi._axisB;
                _hingeList.push_back( hi );
            }
            else if( key == std::string( "Slider:" ) )
            {
                SliderInfo si;
                istr >> si._nodeA >> si._nodeB >> si._axis >> si._low >> si._high >> si._useA;
                _sliderList.push_back( si );
            }
            else if( key == std::string( "CollisionFilter:" ) )
            {
                istr >> std::hex >> filterGroup >> filterWith >> std::dec;
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
        // Disabling this unused code.
#if 0
        std::ofstream out( fileName.c_str() );
        if( !out.good() )
        {
            osg::notify( osg::FATAL ) << "CondigReaderWriter::write: Can't open " << fileName << std::endl;
            return false;
        }
        out << "HandNode: " << _hn->getPosition() << " " <<
            _hn->getAttitude() << " " <<
            _hn->getHandLength() << " " <<
            (_hn->getHandedness() == osgbBulletPlus::HandNode::RIGHT) << std::endl;
        out << "Model: " << _model << std::endl;
        NodeDaeMap::const_iterator itr;
        for( itr = _nodeDaeMap.begin(); itr != _nodeDaeMap.end(); itr++ )
        {
            out << "Node: " << itr->first << std::endl;
            out << "DAE: " << itr->second << std::endl;
        }
        return true;
#endif
    }

    osg::ref_ptr< osgbBulletPlus::HandNode > _hn;
    std::string _model;

    typedef struct NodeInfo {
        std::string _name;
        short _filterGroup, _filterWith;

        bool operator<( const NodeInfo& rhs ) const
        {
            return( (_name < rhs._name ) ||
                (_filterGroup < rhs._filterGroup) ||
                (_filterWith < rhs._filterWith) );
        }
    } NodeInfo;

    typedef std::map< NodeInfo, std::string > NodeDaeMap;
    NodeDaeMap _nodeDaeMap;
    typedef std::vector< NodeInfo > NodeCreateList;
    NodeCreateList _nodeCreateList;
    NodeCreateList _nodeCreateTMList;
    osg::Vec3 _up;

    typedef struct {
        std::string _nodeA;
        osg::Vec3 _pivotA;
        osg::Vec3 _axisA;
        std::string _nodeB;
        osg::Vec3 _pivotB;
        osg::Vec3 _axisB;
    } HingeInfo;
    typedef std::vector< HingeInfo > HingeList;
    HingeList _hingeList;

    typedef struct {
        std::string _nodeA;
        std::string _nodeB;
        osg::Vec3 _axis;
        float _low, _high;
        bool _useA;
    } SliderInfo;
    typedef std::vector< SliderInfo > SliderList;
    SliderList _sliderList;

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
    btCollisionShape* collision = osgbBullet::btBoxCollisionShapeFromOSG( box );

    osgbBullet::MotionState * motion = new osgbBullet::MotionState();

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

    btDiscreteDynamicsWorld* bulletWorld = initPhysics();
    osg::Group* root = new osg::Group;


    ConfigReaderWriter* crw = new ConfigReaderWriter( bulletWorld );
    crw->read( std::string( argv[ 1 ] ) );

    osg::Vec3& up( crw->_up );
    bulletWorld->setGravity( osgbBullet::asBtVector3( up * -9.8 ) );

    //osg::Quat q( osg::PI, osg::Vec3f( 0, 1, 0 ) );
    //crw->_hn->setAttitude( q );
    //crw->_hn->setPosition( osg::Vec3( -2, 6, -3 ) );
    //crw->write( "C:\\Projects\\Physics\\physics_hg\\data\\door.txt" );


    osg::ref_ptr< osgbBulletPlus::HandNode > hn = crw->_hn.get();
    root->addChild( hn.get() );


    osg::ref_ptr< osg::Node > model = osgDB::readNodeFile( crw->_model );
    osg::Matrix m( osg::Matrix::translate( osg::Vec3( 0, 0, 7*up[2] ) ) );
    osg::ref_ptr< osg::MatrixTransform > orient = new osg::MatrixTransform( m );
    orient->addChild( model.get() );
    root->addChild( orient.get() );


    ConfigReaderWriter::NodeDaeMap::const_iterator itr;
    for( itr = crw->_nodeDaeMap.begin(); itr != crw->_nodeDaeMap.end(); itr++ )
    {
        ConfigReaderWriter::NodeInfo ni( itr->first );
        osgwTools::FindNamedNode fnn( ni._name );
        orient->accept( fnn );

        unsigned int idx;
        for( idx=0; idx<fnn._napl.size(); idx++ )
        {
            osgwTools::FindNamedNode::NodeAndPath& nap( fnn._napl[ idx ] );
            osg::Node* subgraph = nap.first;
            osg::NodePath& np = nap.second;
            osgwTools::AbsoluteModelTransform* amt = new osgwTools::AbsoluteModelTransform;
            osg::Group* parent = subgraph->getParent( 0 );
            parent->addChild( amt );
            amt->addChild( subgraph );
            parent->removeChild( subgraph );

            // Create a ref-counted rigid body and store it as user data.
            osgbBullet::RefRigidBody* rb = new osgbBullet::RefRigidBody( osgbBullet::loadDae( amt, np, itr->second ) );
            if( (ni._filterGroup == 0) && (ni._filterWith == 0) )
                bulletWorld->addRigidBody( rb->getRigidBody() );
            else
                bulletWorld->addRigidBody( rb->getRigidBody(), ni._filterGroup, ni._filterWith );
            amt->setUserData( rb );
        }
    }

    if( crw->_nodeCreateList.size() > 0 )
        osg::notify( osg::ALWAYS ) << "Must create physics data..." << std::endl;
    ConfigReaderWriter::NodeCreateList::const_iterator vitr;
    for( vitr = crw->_nodeCreateList.begin(); vitr != crw->_nodeCreateList.end(); vitr++ )
    {
        const ConfigReaderWriter::NodeInfo& ni( *vitr );
        osgwTools::FindNamedNode fnn( ni._name );
        orient->accept( fnn );
        osg::Node* subgraph = fnn._napl[ 0 ].first;
        osg::ref_ptr< osg::Group > dispose = new osg::Group( *(subgraph->asGroup()), osg::CopyOp::DEEP_COPY_ALL );
        osg::NodePath& np = fnn._napl[ 0 ].second;

        osgbBullet::OSGToCollada converter;
        converter.setSceneGraph( dispose.get() );
        converter.setShapeType( BOX_SHAPE_PROXYTYPE );
        converter.setMass( 1.0 );
        converter.setOverall( false );
        converter.convert();

        osg::ref_ptr< osgwTools::AbsoluteModelTransform > amt = new osgwTools::AbsoluteModelTransform();
        amt->setDataVariance( osg::Object::DYNAMIC );
        amt->addChild( subgraph );
        osg::Group* parent = subgraph->getParent( 0 );
        parent->addChild( amt.get() );
        parent->removeChild( subgraph );

        btRigidBody* rb = converter.getRigidBody();
        osgbBullet::MotionState* motion = new osgbBullet::MotionState;
        motion->setTransform( amt.get() );
        osg::BoundingSphere bs = subgraph->getBound();
        rb->setActivationState( DISABLE_DEACTIVATION );

        osg::Matrix m = osg::computeLocalToWorld( np );
        motion->setParentTransform( m );
        motion->setCenterOfMass( bs.center() );
        rb->setMotionState( motion );
        if( (ni._filterGroup == 0) && (ni._filterWith == 0) )
            bulletWorld->addRigidBody( rb );
        else
            bulletWorld->addRigidBody( rb, ni._filterGroup, ni._filterWith );
    }

    if( crw->_nodeCreateTMList.size() > 0 )
        osg::notify( osg::ALWAYS ) << "Must create TM physics data..." << std::endl;
    for( vitr = crw->_nodeCreateTMList.begin(); vitr != crw->_nodeCreateTMList.end(); vitr++ )
    {
        const ConfigReaderWriter::NodeInfo& ni( *vitr );
        osgwTools::FindNamedNode fnn( ni._name );
        orient->accept( fnn );
        osg::Node* subgraph = fnn._napl[ 0 ].first;
        osg::ref_ptr< osg::Group > dispose = new osg::Group( *(subgraph->asGroup()), osg::CopyOp::DEEP_COPY_ALL );
        osg::NodePath& np = fnn._napl[ 0 ].second;

        osgbBullet::OSGToCollada converter;
        converter.setSceneGraph( dispose.get() );
        converter.setShapeType( TRIANGLE_MESH_SHAPE_PROXYTYPE );
        converter.setMass( 1.0 );
        converter.setOverall( false );
        converter.convert();

        osg::ref_ptr< osgwTools::AbsoluteModelTransform > amt = new osgwTools::AbsoluteModelTransform();
        amt->setDataVariance( osg::Object::DYNAMIC );
        amt->addChild( subgraph );
        osg::Group* parent = subgraph->getParent( 0 );
        parent->addChild( amt.get() );
        parent->removeChild( subgraph );

        btRigidBody* rb = converter.getRigidBody();
        osgbBullet::MotionState* motion = new osgbBullet::MotionState;
        motion->setTransform( amt.get() );
        osg::BoundingSphere bs = subgraph->getBound();
        rb->setActivationState( DISABLE_DEACTIVATION );

        osg::Matrix m = osg::computeLocalToWorld( np );
        motion->setParentTransform( m );
        motion->setCenterOfMass( bs.center() );
        rb->setMotionState( motion );
        if( (ni._filterGroup == 0) && (ni._filterWith == 0) )
            bulletWorld->addRigidBody( rb );
        else
            bulletWorld->addRigidBody( rb, ni._filterGroup, ni._filterWith );
    }


    if( crw->_hingeList.size() > 0 )
        osg::notify( osg::ALWAYS ) << "Creating hinge constraints..." << std::endl;
    ConfigReaderWriter::HingeList::const_iterator hitr;
    for( hitr = crw->_hingeList.begin(); hitr != crw->_hingeList.end(); hitr++ )
    {
        const ConfigReaderWriter::HingeInfo& hi = *hitr;

        osgwTools::FindNamedNode fnnA( hi._nodeA );
        orient->accept( fnnA );
        osgwTools::FindNamedNode fnnB( hi._nodeB );
        orient->accept( fnnB );

        osgwTools::FindNamedNode::NodeAndPath& napA( fnnA._napl[ 0 ] );
        osgwTools::AbsoluteModelTransform* amtA = dynamic_cast< osgwTools::AbsoluteModelTransform* >( napA.first->getParent( 0 ) );
        if( amtA == NULL )
        {
            osg::notify( osg::FATAL ) << "Hinge node (" << hi._nodeA << ") parent is not AMY." << std::endl;
            continue;
        }
        osgbBullet::RefRigidBody* rbA = dynamic_cast< osgbBullet::RefRigidBody* >( amtA->getUserData() );
        if( rbA == NULL )
        {
            osg::notify( osg::FATAL ) << "AMT for " << hi._nodeA << " has invalid user data." << std::endl;
            continue;
        }

        osgwTools::FindNamedNode::NodeAndPath& napB( fnnB._napl[ 0 ] );
        osgwTools::AbsoluteModelTransform* amtB = dynamic_cast< osgwTools::AbsoluteModelTransform* >( napB.first->getParent( 0 ) );
        if( amtB == NULL )
        {
            osg::notify( osg::FATAL ) << "Hinge node (" << hi._nodeB << ") parent is not AMY." << std::endl;
            continue;
        }
        osgbBullet::RefRigidBody* rbB = dynamic_cast< osgbBullet::RefRigidBody* >( amtB->getUserData() );
        if( rbB == NULL )
        {
            osg::notify( osg::FATAL ) << "AMT for " << hi._nodeB << " has invalid user data." << std::endl;
            continue;
        }

        const btVector3 btPivotA( osgbBullet::asBtVector3( hi._pivotA ) );
        const btVector3 btPivotB( osgbBullet::asBtVector3( hi._pivotB ) );
        btVector3 btAxisA( osgbBullet::asBtVector3( hi._axisA ) );
        btVector3 btAxisB( osgbBullet::asBtVector3( hi._axisB ) );
        btHingeConstraint* hinge = new btHingeConstraint( *( rbA->getRigidBody() ), *( rbB->getRigidBody() ),
            btPivotA, btPivotB, btAxisA, btAxisB );
        hinge->setLimit( -3.f, -.3f );
        hinge->setAngularOnly( true );
        bulletWorld->addConstraint( hinge, true );
    }


    osg::ref_ptr< osg::Node > axes = osgDB::readNodeFile( "axes.osg" );

    if( crw->_sliderList.size() > 0 )
        osg::notify( osg::ALWAYS ) << "Creating slider constraints..." << std::endl;
    ConfigReaderWriter::SliderList::const_iterator sitr;
    for( sitr = crw->_sliderList.begin(); sitr != crw->_sliderList.end(); sitr++ )
    {
        const ConfigReaderWriter::SliderInfo& si = *sitr;

        osg::Vec3 axis( si._axis );
        osg::Matrix axisMatrix;
        if( axis != osg::Vec3( 1., 0., 0. ) )
            axisMatrix = osg::Matrix::rotate( osg::Vec3( 1., 0., 0. ), axis );


        // Find Node A (usually a static object, like the cabinet framework.
        osgwTools::FindNamedNode fnnA( si._nodeA );
        orient->accept( fnnA );

        // We get back a list of nodes and paths, but only use the first one.
        osgwTools::FindNamedNode::NodeAndPath& napA( fnnA._napl[ 0 ] );
        osgwTools::AbsoluteModelTransform* amtA = dynamic_cast< osgwTools::AbsoluteModelTransform* >( napA.first->getParent( 0 ) );
        if( amtA == NULL )
        {
            osg::notify( osg::FATAL ) << "Slider node (" << si._nodeA << ") parent is not AMY." << std::endl;
            continue;
        }
        osgbBullet::RefRigidBody* rbA = dynamic_cast< osgbBullet::RefRigidBody* >( amtA->getUserData() );
        if( rbA == NULL )
        {
            osg::notify( osg::FATAL ) << "AMT for " << si._nodeA << " has invalid user data." << std::endl;
            continue;
        }

        osg::BoundingSphere bs;
        {
            // Debug -- Visualize the reference frame with the axis model at the center of mass.
            bs = amtA->getChild( 0 )->getBound();
            osg::Vec3 centerA( bs.center() );
            osg::MatrixTransform* mtA = new osg::MatrixTransform( osg::Matrix::translate( centerA ) );
            mtA->addChild( axes.get() );
            amtA->addChild( mtA );
        }

        // Get the world coordinate center to assist in computing the reference frame of the constraint.
        // Body B will be constrained to (centerB - centerA).
        bs = amtA->getBound();
        osg::Vec3 centerA = bs.center();

        // Get the 3x3 basis of body A's reference frame.
        osg::Matrix m;
        amtA->computeLocalToWorldMatrix( m, NULL );
        m = m * axisMatrix;
        btMatrix3x3 m3A( osgbBullet::asBtMatrix3x3( m ) );


        // Find all possible body B objects.
        // Usually something dynamic, like a drawer.
        // We will constraint each body B to the first body A (which we found and processed above)
        osgwTools::FindNamedNode fnnB( si._nodeB );
        orient->accept( fnnB );

        osgwTools::FindNamedNode::NodeAndPath& napB( fnnB._napl[ 0 ] );
        unsigned int idx;
        for( idx=0; idx<fnnB._napl.size(); idx++ )
        {
            osgwTools::FindNamedNode::NodeAndPath& nap( fnnB._napl[ idx ] );
            osgwTools::AbsoluteModelTransform* amtB = dynamic_cast< osgwTools::AbsoluteModelTransform* >( nap.first->getParent( 0 ) );
            if( amtB == NULL )
            {
                osg::notify( osg::FATAL ) << "Slider node (" << si._nodeB << ") parent is not AMY." << std::endl;
                continue;
            }
            osgbBullet::RefRigidBody* rbB = dynamic_cast< osgbBullet::RefRigidBody* >( amtB->getUserData() );
            if( rbB == NULL )
            {
                osg::notify( osg::FATAL ) << "AMT for " << si._nodeB << " has invalid user data." << std::endl;
                continue;
            }

            {
                // Debug -- Visualize the reference frame with the axis model at the center of mass.
                bs = amtB->getChild( 0 )->getBound();
                osg::Vec3 centerB( bs.center() );
                osg::MatrixTransform* mtB = new osg::MatrixTransform( osg::Matrix::translate( centerB ) );
                mtB->addChild( axes.get() );
                amtB->addChild( mtB );
            }

            // Get the world coordinate center to assist in computing the reference frame of the constraint.
            // Body B will be constrained to offsetA = (centerB - centerA).
            bs = amtB->getBound();
            osg::Vec3 centerB = bs.center();
            btVector3 offsetA( osgbBullet::asBtVector3( centerB - centerA ) );

            // Get the 3x3 basis of body B's reference frame.
            amtB->computeLocalToWorldMatrix( m, NULL );
            m = m * axisMatrix;
            btMatrix3x3 m3B( osgbBullet::asBtMatrix3x3( m ) );

            // Finally, set up the constraint.
            // Set the Bullet Transforms. The constraint will align these two coordinate systems.
            // If _useA is true, frameB is placed in frameA between _low and _high on the specified _axis (axisMatrix).
            const btTransform frameInA( m3A, offsetA );
            const btTransform frameInB( m3B );
            btSliderConstraint* slider = new btSliderConstraint( *( rbA->getRigidBody() ), *( rbB->getRigidBody() ),
                frameInA, frameInB, si._useA );
            slider->setLowerLinLimit( si._low );
            slider->setUpperLinLimit( si._high );
            bulletWorld->addConstraint( slider, true );
        }
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
    osgbBullet::GLDebugDrawer* dbgDraw = new osgbBullet::GLDebugDrawer();
    dbgDraw->setDebugMode( ~btIDebugDraw::DBG_DrawText );
    bulletWorld->setDebugDrawer( dbgDraw );
    root->addChild( dbgDraw->getSceneGraph() );
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

