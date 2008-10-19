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

    dynamicsWorld->setGravity( btVector3( 0, 0, 9.8 ) );

    return( dynamicsWorld );
}

#if 0
bool loadDae( osg::Node* node, std::string daeName, btDynamicsWorld* dw, const osg::NodePath& np )
{
    osg::NodePath::const_iterator it;
    for( it=np.begin(); it!=np.end(); it++ )
        osg::notify( osg::ALWAYS ) << (*it)->className() << ", ";
    osg::notify( osg::ALWAYS ) << std::endl;

    std::string fullDaeName( osgDB::findDataFile( daeName ) );
    if( fullDaeName.empty() )
    {
        osg::notify( osg::FATAL ) << "Can't find DAE file: " << daeName << std::endl;
        osg::notify( osg::FATAL ) << "See scripts/mkdae.bat for info on creating DAE files using osgbpp." << std::endl;
        return false;
    }
    osg::notify( osg::ALWAYS ) << "Attempting to load DAE file: " << fullDaeName << std::endl;

    osg::Matrix l2w = osg::computeLocalToWorld( np );
    osg::notify( osg::ALWAYS ) << "loadDae " << l2w << std::endl;
    osg::Matrix invL2w = osg::Matrix::inverse( l2w );

    osg::ref_ptr< osg::MatrixTransform > mt = new osg::MatrixTransform;
    osg::Group* parent = node->getParent( 0 );
    mt->addChild( node );
    parent->removeChild( node );
    parent->addChild( mt.get() );

    btDynamicsWorld* lw = initPhysics();
    ColladaConverter* cc = new ColladaConverter( lw );
    cc->load( fullDaeName.c_str() );
    btRigidBody* rb = cc->getRigidBody( 0 );
    cc->reset();
    lw->removeRigidBody( rb );
    delete cc;
    delete lw;

    btTransform com; com.setIdentity();
    osg::BoundingSphere bs = node->getBound();
    com.setOrigin( osgBullet::asBtVector3( -( bs._center ) ) );
    osg::notify( osg::ALWAYS ) << "COM: " << -( bs._center ) << std::endl;

    osgBullet::MotionState* motion = new osgBullet::MotionState;
    motion->setMatrixTransform( mt.get() );
    motion->setInverseParentWorldTransform( invL2w );
    //motion->m_centerOfMassOffset = com;
    dw->addRigidBody( rb );
    osg::notify( osg::ALWAYS ) << "rb grav: " << osgBullet::asOsgVec3( rb->getGravity() ) << std::endl;

    // Add visual rep og Bullet Collision shape.
    osg::Node* visNode = osgBullet::osgNodeFromBtCollisionShape( rb->getCollisionShape() );
    if( visNode != NULL )
    {
        osg::notify( osg::ALWAYS ) << "Adding vis node" << std::endl;
        osg::MatrixTransform* dmt = new osg::MatrixTransform;
        dmt->addChild( visNode );
        motion->setDebugMatrixTransform( dmt );
        _debugBullet.addDynamic( dmt );

        // Set debug node state.
        osg::StateSet* state = visNode->getOrCreateStateSet();
        osg::PolygonMode* pm = new osg::PolygonMode( osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );
        state->setAttributeAndModes( pm );
        osg::PolygonOffset* po = new osg::PolygonOffset( -1, -1 );
        state->setAttributeAndModes( po );
        state->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    }

    motion->setWorldTransform( osgBullet::asBtTransform( l2w ) );
    rb->setMotionState( motion );

    return true;
}
#endif


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
        }
        else
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
            else if( key == std::string( "View:" ) )
            {
                std::string name;
                istr >> name;
                _viewNodeList.push_back( name );
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
    typedef std::vector< std::string > ViewNodeList;
    ViewNodeList _viewNodeList;

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

btRigidBody * createBTBox( osg::MatrixTransform * box,
                           btVector3 center )
{
    btCollisionShape* collision = osgBullet::btBoxCollisionShapeFromOSG( box );

    btTransform groundTransform;
    groundTransform.setIdentity();
    groundTransform.setOrigin( center );

    osgBullet::MotionState * motion = new osgBullet::MotionState();
    motion->setMatrixTransform( box );
    motion->setWorldTransform( groundTransform );

    btScalar mass( 0.0 );
    btVector3 inertia( 0, 0, 0 );
    btRigidBody::btRigidBodyConstructionInfo rb( mass, motion, collision, inertia );
    btRigidBody * body = new btRigidBody( rb );

    osg::Node* dbgGround = osgBullet::osgNodeFromBtCollisionShape( collision );
    if( dbgGround )
    {
        osg::MatrixTransform* dmt = new osg::MatrixTransform;
        dmt->addChild( dbgGround );
        motion->setDebugMatrixTransform( dmt );
        _debugBullet.addDynamic( dmt );
    }

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
        osgBullet::loadDae( fnn._node.get(), fnn._np, itr->second, bulletWorld, &_debugBullet );
    }
    /*
    ConfigReaderWriter::ViewNodeList::const_iterator vitr;
    for( vitr = crw->_viewNodeList.begin(); vitr != crw->_viewNodeList.end(); vitr++ )
    {
        osg::notify( osg::ALWAYS ) << "Viewing: " << *vitr << std::endl;
        FindNamedNode fnn( *vitr );
        orient->accept( fnn );
        osg::Matrix m = osg::computeLocalToWorld( fnn._np );
        osg::ref_ptr< osg::MatrixTransform > mt = new osg::MatrixTransform( m );
        mt->addChild( fnn._node.get() );
#ifdef DIRTY_HACK
        root->addChild( mt.get() );
#endif
    }
    */


    float thin = .1;
    osg::MatrixTransform* ground = createOSGBox( osg::Vec3( 10, 10, thin ) );
    root->addChild( ground );
    btRigidBody* groundBody = createBTBox( ground, btVector3( 0, 0, -thin ) );
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
    while( /*count-- &&*/ !viewer.done() )
    {
        currSimTime = viewer.getFrameStamp()->getSimulationTime();
        bulletWorld->stepSimulation( currSimTime - prevSimTime );
        prevSimTime = currSimTime;
        viewer.frame();
    }

    return( 0 );
}




#if 0

class DynamicRigidBodyTransform : public osg::MatrixTransform
{
public:
    DynamicRigidBodyTransform();
    DynamicRigidBodyTransform( const DynamicRigidBodyTransform& drbt, const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY );
    ~DynamicRigidBodyTransform();
    META_Node( osgBullet, DynamicRigidBodyTransform );

    void parentTransformChange( const osg::NodePath& np );
    const osg::Matrix& getParentTransform() const;

    virtual osg::BoundingSphere computeBound() const;

protected:
    osg::Matrix _l2w;
    osg::Matrix _invL2w;
};

DynamicRigidBodyTransform::DynamicRigidBodyTransform()
{
}

DynamicRigidBodyTransform::DynamicRigidBodyTransform( const DynamicRigidBodyTransform& drbt, const osg::CopyOp& copyop )
{
    osg::notify( osg::WARN ) << "DynamicRigidBodyTransform: copy constructor not yet implemented." << std::endl;
}

DynamicRigidBodyTransform::~DynamicRigidBodyTransform()
{
}

void
DynamicRigidBodyTransform::parentTransformChange( const osg::NodePath& np )
{
    osg::Matrix& l2w = osg::computeLocalToWorld( np );
    _l2w = l2w;
    _invL2w = osg::Matrix::inverse( l2w );
}

const osg::Matrix&
DynamicRigidBodyTransform::getParentTransform() const
{
    return _l2w;
}

osg::BoundingSphere
DynamicRigidBodyTransform::computeBound() const
{
    DynamicRigidBodyTransform* nonConstThis = const_cast< DynamicRigidBodyTransform* >( this );
    nonConstThis->_matrix.postMult( _invL2w );

    osg::BoundingSphere bs = osg::MatrixTransform::computeBound();
    return bs;
}

#endif
