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
#include <osgBullet/DebugBullet.h>
#include <osgBullet/ColladaUtils.h>
#include <osgBullet/Utils.h>
#include <osgBullet/GLDebugDrawer.h>

#include <btBulletDynamicsCommon.h>
#include <BulletColladaConverter/ColladaConverter.h>
#include <BulletColladaConverter/ColladaConverter.h>

#include <list>
#include <map>
#include <string>
#include <sstream>
#include <osg/io_utils>

#include <iostream>

osgBullet::DebugBullet _debugBullet;

void debugDynamicsWorld( btDynamicsWorld* tempWorld )
{
    for( int i = 0; i < tempWorld->getNumCollisionObjects(); ++i )
    {
        btCollisionObject* temp = tempWorld->getCollisionObjectArray()[ i ];
        btVector3 bbMin = temp->getBroadphaseHandle()->m_aabbMin;
        btVector3 bbMax = temp->getBroadphaseHandle()->m_aabbMax;
        std::cout << "Min = " << bbMin.x() << " " << bbMin.y() << " " << bbMin.z() << std::endl;
        std::cout << "Max = " << bbMax.x() << " " << bbMax.y() << " " << bbMax.z() << std::endl;
        
        temp->getCollisionShape()->getAabb(temp->getWorldTransform(), bbMin,bbMax);
        std::cout << "Min2 = " << bbMin.x() << " " << bbMin.y() << " " << bbMin.z() << std::endl;
        std::cout << "Max2 = " << bbMax.x() << " " << bbMax.y() << " " << bbMax.z() << std::endl;
    }

    int numManifolds1 = tempWorld->getDispatcher()->getNumManifolds();
    for( int i = 0; i < numManifolds1; ++i )
    {
        btPersistentManifold* contactManifold =
        tempWorld->getDispatcher()->getManifoldByIndexInternal( i );
        //contactManifold->refreshContactPoints(
        //bodyA->getWorldTransform(), bodyB->getWorldTransform() );
        
        int numContacts = contactManifold->getNumContacts();
        for (int p=0;p<contactManifold->getNumContacts();p++)
        {
            const btManifoldPoint& pt = contactManifold->getContactPoint(p);
            
            btVector3 posWorldB = pt.getPositionWorldOnB();
            btVector3 posWorldA = pt.m_normalWorldOnB;
            std::cout << "Position = " << posWorldB.x() << " " << posWorldB.y() << " " << posWorldB.z() << std::endl;
            std::cout << "Normal = " << posWorldA.x() << " " << posWorldA.y() << " " << posWorldA.z() << std::endl;
            std::cout << "Distance = " << pt.getDistance() << std::endl;
            std::cout << "Lifetime = " << pt.getLifeTime() << std::endl;
        }
    }
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

    dynamicsWorld->setGravity( btVector3( 0, 0, 9.8 ) );

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
      : _dw( dw )
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
            if( key == std::string( "HandNode:" ) )
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
                _hn->setTraverseHand( false );
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
    if( argc < 2 )
    {
        osg::notify( osg::FATAL ) << "Specify a config file (such as door.txt) on the command line." << std::endl;
        return 1;
    }

    btDynamicsWorld* bulletWorld = initPhysics();
    osg::Group* root = new osg::Group;
    //root->addChild( _debugBullet.getRoot() );


    ConfigReaderWriter* crw = new ConfigReaderWriter( bulletWorld );
    crw->read( std::string( argv[ 1 ] ) );

    //osg::Quat q( osg::PI, osg::Vec3f( 0, 1, 0 ) );
    //crw->_hn->setAttitude( q );
    //crw->_hn->setPosition( osg::Vec3( -2, 6, -3 ) );
    //crw->write( "C:\\Projects\\Physics\\physics_hg\\data\\door.txt" );


    osg::ref_ptr< osgBullet::HandNode > hn = crw->_hn.get();
    root->addChild( hn.get() );


    osg::ref_ptr< osg::Node > model = osgDB::readNodeFile( crw->_model );
    osg::Matrix m( /*osg::Matrix::rotate( osg::PI, osg::Vec3( 1,0,0 ) ) * */
        osg::Matrix::translate( osg::Vec3( 0, 0, -7 ) ) );
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
            osgBullet::loadDae( amt, np, itr->second, bulletWorld, &_debugBullet );
        }
    }

    osg::notify( osg::ALWAYS ) << "Must create physics data..." << std::endl;
    ConfigReaderWriter::NodeCreateList::const_iterator vitr;
    for( vitr = crw->_nodeCreateList.begin(); vitr != crw->_nodeCreateList.end(); vitr++ )
    {
        osg::notify( osg::ALWAYS ) << "  Creating physics data for: " << *vitr << std::endl;
        FindNamedNode fnn( *vitr );
        orient->accept( fnn );
        osg::Node* subgraph = fnn._napl[ 0 ].first;
        osg::NodePath& np = fnn._napl[ 0 ].second;

        osgBullet::OSGToCollada converter( 
            subgraph, BOX_SHAPE_PROXYTYPE, 1.0, "", 1.0, false );

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

        // Add visual rep of Bullet Collision shape.
        osg::Node* visNode = osgBullet::osgNodeFromBtCollisionShape( rb->getCollisionShape() );
        if( visNode != NULL )
        {
            osgBullet::AbsoluteModelTransform* dmt = new osgBullet::AbsoluteModelTransform;
            dmt->addChild( visNode );
            motion->setDebugTransform( dmt );
            _debugBullet.addDynamic( dmt );
        }

        osg::Matrix m = osg::computeLocalToWorld( np );
        motion->setParentTransform( m );
        motion->setCenterOfMass( bs.center() );
        rb->setMotionState( motion );
        bulletWorld->addRigidBody( rb );
    }

    osg::notify( osg::ALWAYS ) << "Must create TM physics data..." << std::endl;
    for( vitr = crw->_nodeCreateTMList.begin(); vitr != crw->_nodeCreateTMList.end(); vitr++ )
    {
        osg::notify( osg::ALWAYS ) << "  Creating TM physics data for: " << *vitr << std::endl;
        FindNamedNode fnn( *vitr );
        orient->accept( fnn );
        osg::Node* subgraph = fnn._napl[ 0 ].first;
        osg::NodePath& np = fnn._napl[ 0 ].second;

        osgBullet::OSGToCollada converter( 
            subgraph, TRIANGLE_MESH_SHAPE_PROXYTYPE, 1.0, "", 1.0, false );

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

        // Add visual rep of Bullet Collision shape.
        osg::Node* visNode = osgBullet::osgNodeFromBtCollisionShape( rb->getCollisionShape() );
        if( visNode != NULL )
        {
            osgBullet::AbsoluteModelTransform* dmt = new osgBullet::AbsoluteModelTransform;
            dmt->addChild( visNode );
            motion->setDebugTransform( dmt );
            _debugBullet.addDynamic( dmt );
        }

        osg::Matrix m = osg::computeLocalToWorld( np );
        motion->setParentTransform( m );
        motion->setCenterOfMass( bs.center() );
        rb->setMotionState( motion );
        bulletWorld->addRigidBody( rb );
    }


    float thin = .1;
    osg::MatrixTransform* ground = createOSGBox( osg::Vec3( 10, 10, thin ) );
    root->addChild( ground );
    btRigidBody* groundBody = createBTBox( ground, osg::Vec3( 0, 0, -thin ) );
    bulletWorld->addRigidBody( groundBody );


    osgViewer::Viewer viewer;
    viewer.setUpViewOnSingleScreen( 0 );
    viewer.setSceneData( root );
    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
    tb->setHomePosition( osg::Vec3( 0, 35, 0 ), osg::Vec3( 0, 0, 0 ), osg::Vec3( 0,0,-1) );
    viewer.setCameraManipulator( tb );
    viewer.addEventHandler( new HandManipulator( hn.get() ) );

    double currSimTime = viewer.getFrameStamp()->getSimulationTime();
    double prevSimTime = viewer.getFrameStamp()->getSimulationTime();
    viewer.realize();
    int count( 4 );
    
    bulletWorld->setDebugDrawer( new GLDebugDrawer( root ) );
    /*DBG_DrawWireframe = 1,
    DBG_DrawAabb=2,
    DBG_DrawFeaturesText=4,
    DBG_DrawContactPoints=8,
    DBG_NoDeactivation=16,
    DBG_NoHelpText = 32,
    DBG_DrawText=64,
    */
    //btIDebugDraw::DBG_MAX_DEBUG_DRAW_MODE
    bulletWorld->getDebugDrawer()->setDebugMode(  
    btIDebugDraw::DBG_DrawWireframe |
    //btIDebugDraw::DBG_DrawFeaturesText |
    //btIDebugDraw::DBG_DrawText |
    btIDebugDraw::DBG_DrawAabb |
    btIDebugDraw::DBG_DrawContactPoints
    );

    while( /*count-- &&*/ !viewer.done() )
    {
        currSimTime = viewer.getFrameStamp()->getSimulationTime();
        bulletWorld->stepSimulation( currSimTime - prevSimTime );
        float dt = currSimTime - prevSimTime;
        prevSimTime = currSimTime;
        dynamic_cast< GLDebugDrawer* >( bulletWorld->getDebugDrawer() )->BeginDraw();
        bulletWorld->debugDrawWorld();
        dynamic_cast< GLDebugDrawer* >( bulletWorld->getDebugDrawer() )->EndDraw();
        viewer.frame();
    }

    return( 0 );
}

