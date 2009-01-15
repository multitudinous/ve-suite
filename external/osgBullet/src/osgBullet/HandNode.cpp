//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osgBullet/HandNode.h>
#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osg/NodeVisitor>
#include <osg/NodeCallback>
#include <osgSim/DOFTransform>
#include <osg/MatrixTransform>
#include <osg/Matrix>
#include <osg/PolygonMode>
#include <osg/PolygonOffset>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/Utils.h>
#include <btBulletCollisionCommon.h>
#include <btBulletDynamicsCommon.h>
#include <osg/ComputeBoundsVisitor>

#include <string>
#include <osg/io_utils>


// Debug utility. Dump a NodePath.
void dumpNP( const osg::NodePath& p )
{
    osg::notify( osg::ALWAYS ) << p.size() << ": ";
    unsigned int idx;
    for( idx=0; idx<p.size(); idx++ )
        osg::notify( osg::ALWAYS ) << p[idx]->getName() << ", ";
    osg::notify( osg::ALWAYS ) << std::endl;
}

namespace osgBullet
{


// For debug: create an OSG rep of the Bullet compound shape
// and render it as wireframe over the hand model.
HandNode::DebugBullet::DebugBullet()
{
    // Yes it's a MatrixTransform. We are going to be drawn in
    //   the base class (Transform) space, but that transform is already in the
    //   Bullet shape transforms. So, to render accureately we put
    //   the inverse matrix in this MatrixTransform.
    _root = new osg::MatrixTransform;

    osg::StateSet* state = _root->getOrCreateStateSet();
    osg::PolygonMode* pm = new osg::PolygonMode( osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );
    state->setAttributeAndModes( pm );
    osg::PolygonOffset* po = new osg::PolygonOffset( -1, -1 );
    state->setAttributeAndModes( po );
    state->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
}
HandNode::DebugBullet::~DebugBullet()
{
}

unsigned int HandNode::DebugBullet::addStatic( osg::Node* node )
{
    osg::MatrixTransform* mt = new osg::MatrixTransform;
    mt->setMatrix( osg::Matrix::identity() );
    mt->addChild( node );
    _root->addChild( mt );
    return _root->getNumChildren() - 1;
}
void HandNode::DebugBullet::setTransform( unsigned int idx, const osg::Matrix& m )
{
    osg::MatrixTransform* mt = dynamic_cast< osg::MatrixTransform* >( _root->getChild( idx ) );
    mt->setMatrix( m );
}
osg::MatrixTransform* HandNode::DebugBullet::getRoot() const
{
    return _root.get();
}


// ScaleVisitor is designed specifically to scale the hand model.
// The hand model must be scaled into an app-specified size at load time to
//   avoid the complexities of scaling in Bullet. Bullet does not support
//   scaled collision shapes. So, rather than scale with a transform as we
//   would otherwise do in OSG, we apply a scale to the whole model.
// Can't use osgUtil::Optimizer::FlattenStaticTransformVisitor because it
//   doesn't handle DOFTransform correctly and it would scale the normals.
class ScaleVisitor : public osg::NodeVisitor
{
public:
    ScaleVisitor( float scale )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _scale( scale )
    {}
    ~ScaleVisitor()
    {}

    void apply( osg::Transform& node )
    {
        osgSim::DOFTransform* dof = dynamic_cast< osgSim::DOFTransform* >( &node );
        if( dof != NULL )
        {
            osg::Matrix m = dof->getInversePutMatrix();
            osg::Vec3 trans = m.getTrans();
            m.setTrans( trans * _scale );
            dof->setInversePutMatrix( m );
            dof->setPutMatrix( osg::Matrix::inverse( m ) );
        }
        traverse( node );
    }

    void apply( osg::Geode& node )
    {
        unsigned int idx;
        for( idx=0; idx<node.getNumDrawables(); idx++ )
        {
            osg::Geometry* geom = dynamic_cast< osg::Geometry* >( node.getDrawable( idx ) );
            if( geom == NULL )
                continue;
            osg::Vec3Array* v = dynamic_cast< osg::Vec3Array* >( geom->getVertexArray() );
            if( v == NULL )
            {
                osg::notify( osg::WARN ) << "HandNode: Unexpected non-Vec3Array while scaling hand." << std::endl;
                continue;
            }
            unsigned int jdx;
            for( jdx=0; jdx<v->getNumElements(); jdx++ )
            {
                (*v)[ jdx ] *= _scale;
            }
        }
        traverse( node );
    }

protected:
    float _scale;
};



// Creates an osg::NodePath. The child-most node is in
// element zero, and the root node is at the end of the vector.
class CreateNodePath : public osg::NodeVisitor
{
public:
    CreateNodePath( osg::Node* firstNode=NULL )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_PARENTS ),
        _node( firstNode )
    {
        if( _node != NULL )
            _p.push_back( _node );
    }

    void apply( osg::Node& node )
    {
        traverse( node );
        _p.push_back( &node );
    }
    void apply( osg::Camera& node )
    {
        // Shouldn't be here.
        osg::notify( osg::WARN ) << "HandNode: CreateNodePath encountered unexpected Camera node." << std::endl;
        // Don't include in NodePath and stop traversing.
        return;
    }

    osg::NodePath getNodePath()
    {
        return _p;
    }

protected:
    osg::NodePath _p;
    osg::Node* _node;
};

// At init time, the HandNode loads the hand model, then uses this NodeVisitor
//   to traverse the model and identify all the articulatable parts and create
//   Bullet collision shapes for them.
class FindArticulations : public osg::NodeVisitor
{
public:
    FindArticulations( HandNode* hn, HandNode::ArticulationInfoList& ail, HandNode::DebugBullet* debugBullet )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _hn( hn ),
        _ail( ail ),
        _debugBullet( debugBullet )
    {
        _cs = new btCompoundShape;

        // Load the name map. Maps the names of the nodes we're looking for
        //   to the Articulation enum defined in HandNode.h.
        articulations_[ "f0trans" ] = HandNode::FINGER_0_TRANSLATE;
        articulations_[ "f1trans" ] = HandNode::FINGER_1_TRANSLATE;
        articulations_[ "f2trans" ] = HandNode::FINGER_2_TRANSLATE;
        articulations_[ "f3trans" ] = HandNode::FINGER_3_TRANSLATE;
        articulations_[ "f4trans" ] = HandNode::FINGER_4_TRANSLATE;

        articulations_[ "f0k0" ]    = HandNode::FINGER_0_ROTATE_INNER;
        articulations_[ "f1k0" ]    = HandNode::FINGER_1_ROTATE_INNER;
        articulations_[ "f2k0" ]    = HandNode::FINGER_2_ROTATE_INNER;
        articulations_[ "f3k0" ]    = HandNode::FINGER_3_ROTATE_INNER;
        articulations_[ "f4k0" ]    = HandNode::FINGER_4_ROTATE_INNER;

        articulations_[ "f0k1" ]    = HandNode::FINGER_0_ROTATE_OUTER;
        articulations_[ "f1k1" ]    = HandNode::FINGER_1_ROTATE_OUTER;
        articulations_[ "f2k1" ]    = HandNode::FINGER_2_ROTATE_OUTER;
        articulations_[ "f3k1" ]    = HandNode::FINGER_3_ROTATE_OUTER;
        articulations_[ "f4k1" ]    = HandNode::FINGER_4_ROTATE_OUTER;
    }
    ~FindArticulations() {}

    void apply( osg::Group& node )
    {
        if( node.getNumParents() != 0 )
            // Should be one parent, the scale transform to set the hand length.
            osg::notify( osg::WARN ) << "HandNode: Group node has " << node.getNumParents() << " parents, should be 0." << std::endl;

        traverse( node );

        // Store info about the palm in the _palm ArticulationInfo.
        // The palm isn't articulatable like the fingers and knuckles,
        // but we need to record info about it somewhere; ArticulationInfo
        // is really a superset if the info we need to store. But it's
        // good enough for now.
        _palm._dof = NULL;
        _palm._cs = _cs;
        _palm._debugBullet = _debugBullet;
        _palm._dependent = NULL;

        CreateNodePath cnp( _hn );
        node.accept( cnp );
        _palm._l2wNodePath = cnp.getNodePath();

        osg::BoundingBox bbox;
        btCollisionShape* shape = createChildCollisionShapes( bbox, node, true );
        if( shape != NULL )
        {
            // Vector from the origin to the center of the bounding box. This transforms
            //   the bullet shape so that it coincides with the object's actual location.
            _palm._bt2osg = bbox.center();

            btTransform xform; xform.setIdentity();
            _cs->addChildShape( xform, shape );
            _palm._btChildIdx = _cs->getNumChildShapes() - 1;

            osg::Node* debugNode = osgBullet::osgNodeFromBtCollisionShape( shape );
            _palm._debugIdx = _debugBullet->addStatic( debugNode );
        }
    }

    void apply( osg::Transform& node )
    {
        osgSim::DOFTransform* dof = dynamic_cast< osgSim::DOFTransform* >( &node );
        if ( !dof )
        {
            osg::notify( osg::WARN ) << "HandNode: FindArticulations found a non-DOF transform: " <<
                node.className() << ", " << node.getName() << std::endl;
            traverse( node );
            return;
        }

        traverse( node );

        // Use the node name to get the Articulation enum, then obtain a reference to
        //   the ArticulationInfo struct for this articulation.
        HandNode::Articulation part( HandNode::MAX_ARTICULATIONS );
        part = articulations_[ node.getName() ];
        if (part >= HandNode::MAX_ARTICULATIONS)
        {
            osg::notify( osg::WARN ) << "HandNode: Can't find articulation for " << node.getName() << std::endl;
            return;
        }
        HandNode::ArticulationInfo& ai( _ail[ part ] );

        // Fill in ArticulationInfo fields with all the required information.
        ai._dof = dof;
        ai._cs = _cs;
        ai._debugBullet = _debugBullet;

        // Find dependent AI. We can look this up using the name of the child DOF.
        // For example, the dependent AI of the inner knuckle is the outer knuckle.
        //   It's "dependent" because the inner knuckle transform affects the
        //   Bullet (absolute) transform of the outer knuckle.
        ai._dependent = NULL;
        osgSim::DOFTransform* childDOF = findChildDOF( *dof );
        if( childDOF != NULL )
        {
            HandNode::Articulation depPart( HandNode::MAX_ARTICULATIONS );
            depPart = articulations_[ childDOF->getName() ];
            if (part < HandNode::MAX_ARTICULATIONS)
                ai._dependent = &( _ail[ depPart ] );
        }

        // Get the NodePath, this allows the ArticulationInfo to accumulate
        //   parent transformations to create the absolute transform matrix.
        CreateNodePath cnp( _hn );
        node.accept( cnp );
        ai._l2wNodePath = cnp.getNodePath();

        osg::BoundingBox bbox;
        btCollisionShape* shape = createChildCollisionShapes( bbox, node, true );
        if( shape != NULL )
        {
            // Vector from the origin to the center of the bounding box. This transforms
            //   the bullet shape so that it coincides with the object's actual location.
            ai._bt2osg = bbox.center();

            btTransform xform; xform.setIdentity();
            _cs->addChildShape( xform, shape );
            ai._btChildIdx = _cs->getNumChildShapes() - 1;

            osg::Node* debugNode = osgBullet::osgNodeFromBtCollisionShape( shape );
            ai._debugIdx = _debugBullet->addStatic( debugNode );
        }
    }

    btCompoundShape* getCollisionShape() const
    {
        return _cs;
    }
    HandNode::ArticulationInfo getPalm() const
    {
        return _palm;
    }


protected:
    typedef std::map< std::string, HandNode::Articulation > ArticulationNameMap;
    ArticulationNameMap articulations_;

    HandNode* _hn;
    HandNode::ArticulationInfoList& _ail;
    HandNode::ArticulationInfo _palm;
    btCompoundShape* _cs;

    osg::ref_ptr< HandNode::DebugBullet > _debugBullet;

    // Create a single collision shape from the non-Transform children.
    // If 'palm' is true, create a box shape to represent the hand's palm.
    //   Otherwise, create Y-aligned cylinders for the finger segments.
    // 'bbox' is a return value and can be used by the calling routine to
    //   determine a translation for the shape.
    // 'node' is input and const, but not declared const to facilitate
    //   getting its children and running a NodeVisitor on them.
    static btCollisionShape* createChildCollisionShapes( osg::BoundingBox& bbox, osg::Group& node, bool palm )
    {
        osg::ref_ptr< osg::Group > tempRoot = new osg::Group;
        unsigned int idx;
        for ( idx=0; idx < node.getNumChildren(); idx++ )
        {
            osg::Node* child = node.getChild( idx );
            if ( dynamic_cast< osg::Transform* >( child ) )
                continue;
            tempRoot->addChild( child );
        }
        if( tempRoot->getNumChildren() == 0 )
            // No non-Transform children. Must be one of the flexure nodes.
            return NULL;

        // TBD handle palm true (box) or false (cylinder)?

        osg::ComputeBoundsVisitor visitor;
        tempRoot->accept( visitor );
        bbox = visitor.getBoundingBox();

        return osgBullet::btBoxCollisionShapeFromOSG( tempRoot.get(), &bbox );
    }

    static osgSim::DOFTransform* findChildDOF( osg::Group& grp )
    {
        unsigned int idx;
        for( idx=0; idx<grp.getNumChildren(); idx++ )
        {
            osgSim::DOFTransform* dof = dynamic_cast< osgSim::DOFTransform* >( grp.getChild( idx ) );
            if( dof != NULL )
                return dof;
        }
        return NULL;
    }
};



//
//
// HandNode
//
//



// Statics
float HandNode::_defaultLength( 6.5f );


HandNode::HandNode()
  : _rightOrLeft( HandNode::RIGHT ),
    _length( _defaultLength ),
    _bulletWorld( NULL ),
    _body( NULL ),
    _shape( NULL ),
    _traverseHand( true ),
    _debug( false )
{
    setName( "HandNode" );
    init();
}
HandNode::HandNode( btDynamicsWorld* bulletWorld, const HandNode::Handedness rightOrLeft, float handLength )
  : _rightOrLeft( rightOrLeft ),
    _length( handLength ),
    _bulletWorld( bulletWorld ),
    _body( NULL ),
    _shape( NULL ),
    _traverseHand( true ),
    _debug( false )
{
    setName( "HandNode" );
    init();
}
HandNode::HandNode( const HandNode& rhs, const osg::CopyOp& copyop )
  : _rightOrLeft( rhs._rightOrLeft ),
    _position( rhs._position ),
    _attitude( rhs._attitude ),
    _length( rhs._length ),
    _bulletWorld( rhs._bulletWorld ),
    _body( NULL ),
    _shape( NULL ),
    _traverseHand( rhs._traverseHand ),
    _debug( rhs._debug )
{
    setName( rhs.getName() );
    init();
}
HandNode::~HandNode()
{
    cleanup();
}

void HandNode::cleanup()
{
    if( _hand.valid())
        _hand = NULL;
    if( _debugBullet.valid())
        _debugBullet = NULL;

    _ail.clear();

    if( _body != NULL )
    {
        if( _bulletWorld != NULL )
            _bulletWorld->removeRigidBody( _body );
        delete _body;
        _body = NULL;
    }
    if( _shape != NULL )
    {
        // TBD possible memory leak, do we need to delete child shapes?
        delete _shape;
        _shape = NULL;
    }
}

// The Transform Node and its derived classes don't define ::traverse(), they
// just get the base class Group::traverse() behavior. The CullVisitor handles
// accumulating transformation matrices.
void HandNode::traverse( osg::NodeVisitor& nv )
{
    // Traverse children, if any
    Transform::traverse( nv );

    // Traverse the hand, if it's valid (it should be valid; see init() ).
    if (_traverseHand && _hand.valid())
        _hand->accept( nv );

    // Render the wireframe if debugging is on.
    if( _debug )
        _debugBullet->getRoot()->accept( nv );
}
osg::BoundingSphere HandNode::computeBound() const
{
    // Get the bounding sphere of any children;
    osg::BoundingSphere childBS = Transform::computeBound();

    if (!_hand.valid())
        return childBS;

    // Get the hand bounding sphere.
    osg::BoundingSphere bsphere = _hand->computeBound();

    // Transform the hand bounding sphere by PAT parameters.
    osg::Matrix l2w;
    computeLocalToWorldMatrix(l2w,NULL);

    osg::Vec3 xdash = bsphere._center;
    xdash.x() += bsphere._radius;
    xdash = xdash*l2w;

    osg::Vec3 ydash = bsphere._center;
    ydash.y() += bsphere._radius;
    ydash = ydash*l2w;

    osg::Vec3 zdash = bsphere._center;
    zdash.z() += bsphere._radius;
    zdash = zdash*l2w;


    bsphere._center = bsphere._center*l2w;

    xdash -= bsphere._center;
    float len_xdash = xdash.length();

    ydash -= bsphere._center;
    float len_ydash = ydash.length();

    zdash -= bsphere._center;
    float len_zdash = zdash.length();

    bsphere._radius = len_xdash;
    if (bsphere._radius<len_ydash) bsphere._radius = len_ydash;
    if (bsphere._radius<len_zdash) bsphere._radius = len_zdash;


    if (childBS.valid())
    {
        // Expand BS of children by transformed BS of the hand.
        childBS.expandBy( bsphere );
        return childBS;
    }
    else
    {
        // Just return the reansformed hand BS.
        return bsphere;
    }
}

bool HandNode::computeLocalToWorldMatrix( osg::Matrix& matrix, osg::NodeVisitor*) const
{
    const osg::Matrix l2w(
        osg::Matrix::rotate( _attitude ) *
        osg::Matrix::translate( _position ) );

    if( _referenceFrame==RELATIVE_RF )
        matrix.preMult( l2w );
    else // absolute
        matrix = l2w;
    return true;
}

bool HandNode::computeWorldToLocalMatrix( osg::Matrix& matrix, osg::NodeVisitor* ) const
{
    const osg::Matrix w2l(
        osg::Matrix::translate( -_position ) *
        osg::Matrix::rotate( _attitude.inverse() ) );

    if (_referenceFrame==RELATIVE_RF)
        matrix.postMult( w2l );
    else // absolute
        matrix = w2l;
    return true;
}

// Must be called to set the Bullet shape transformations any time
// the PAT transformation changes. This wouldn't be necessary if
// we could override methods like setPosition and setAttitude, but
// they are not declared virtual and this is unlikely to change.
void HandNode::updateTransform()
{
    // Set the HandNode inverse transform at the top of the debug group.
    // This is needed because the bullet shape transforms already have
    // our transform baked in, but it will also be applied by the CullVisitor,
    // so the inverse here ensures the transform is applied only once.
    osg::Matrix m;
    computeWorldToLocalMatrix( m, NULL );
    _debugBullet->getRoot()->setMatrix( m );

    // Update the Bullet transform for all component collision shapes.
    int idx;
    for( idx=0; idx<MAX_ARTICULATIONS; idx++ )
        _ail[ idx ].setBulletTransform();
    _palm.setBulletTransform();
}

void HandNode::init()
{
    // Start from a clean slate.
    cleanup();

    _debugBullet = new DebugBullet();

    // Load either the right or left hand.
    std::string fileName;
    if (_rightOrLeft == LEFT)
        fileName = "handL.ive";
    else
        fileName = "handR.ive";
    _hand = osgDB::readNodeFile( fileName );
    if( !_hand.valid() )
    {
        osg::notify( osg::FATAL ) << "HandNode: Can't load \"" << fileName << "\". Check osgDB data file search path." << std::endl;
        return;
    }
#if 0
    {
        // Code for determining default hand length in Y is 6.5 units.
        osg::ComputeBoundsVisitor cbv;
        handNode->accept( cbv );
        osg::notify( osg::ALWAYS ) << cbv.getBoundingBox()._min << ", " << cbv.getBoundingBox()._max << std::endl;
        osg::notify( osg::ALWAYS ) << cbv.getBoundingBox()._max[1]-cbv.getBoundingBox()._min[1] << std::endl;
    }
#endif

    // Scale the hand to the correct length.
    ScaleVisitor sv( _length / _defaultLength );
    _hand->accept( sv );

    // Run the FindArticulations visitor. This loads the ArticulationInfoList
    // and creates child shapes for our "_shape" compund shape.
    osg::notify( osg::INFO ) << "HandNode: Finding articulations." << std::endl;
    _ail.resize( MAX_ARTICULATIONS );
    FindArticulations fa( this, _ail, _debugBullet.get() );
    _hand->accept( fa );

    // FindArticulations created a compund collision shape for us;
    // save the address.
    _shape = fa.getCollisionShape();
    _palm = fa.getPalm();

    if (_rightOrLeft == LEFT)
    {
        // Left hand model seems to have been created with 
        // negated flexure transforms. Compensate for that...
        setNegateRotation( FINGER_0_TRANSLATE, true );
        setNegateRotation( FINGER_1_TRANSLATE, true );
        setNegateRotation( FINGER_2_TRANSLATE, true );
        setNegateRotation( FINGER_3_TRANSLATE, true );
        setNegateRotation( FINGER_4_TRANSLATE, true );
    }

    // Create a rigid body from the compound shape and add it to the
    // Bullet dynamics world.
    if (_bulletWorld != NULL)
    {
        // Create rigid body and add to bullet world.
        btTransform xform; xform.setIdentity();
        btDefaultMotionState* myMotionState = new btDefaultMotionState( xform );
	    btVector3 inertia( 0, 0, 0 );
	    btRigidBody::btRigidBodyConstructionInfo rbInfo( 3.0, myMotionState, _shape, inertia );
        rbInfo.m_friction = btScalar( 1. );
	    _body = new btRigidBody( rbInfo );
        _body->setCollisionFlags( _body->getCollisionFlags() | btCollisionObject::CF_KINEMATIC_OBJECT );
        _body->setActivationState( DISABLE_DEACTIVATION );
        //_body->setCcdMotionThreshold( 0.00001 );
        //_body->setCcdSweptSphereRadius( 0.1*_length );
        
	    _bulletWorld->addRigidBody( _body );
    }

    // Set the initial Bullet transformation matrices.
    updateTransform();

    // Let Bullet know the size of the new Aabb.
    if (_bulletWorld != NULL)
        _shape->recalculateLocalAabb();
}

void HandNode::setHandedness( const HandNode::Handedness rightOrLeft )
{
    if ( _rightOrLeft != rightOrLeft )
    {
        _rightOrLeft = rightOrLeft;
        init();
    }
}
HandNode::Handedness HandNode::getHandedness() const
{
    return _rightOrLeft;
}

void
HandNode::setHandLength( float length )
{
    if ( _length != length )
    {
        _length = length;
        init();
    }
}

float
HandNode::getHandLength() const
{
    return _length;
}


void HandNode::setArticulation( const HandNode::Articulation part, const float radians )
{
    _ail[ part ].setAngle( radians );
}
float HandNode::getArticulation( const HandNode::Articulation part ) const
{
    return( _ail[ part ].getAngle() );
}

void HandNode::setNegateRotation( const HandNode::Articulation part, const bool negate )
{
    ArticulationInfo& ai = _ail[ part ];
    ai._negate = negate;
}
bool HandNode::getNegateRotation( const HandNode::Articulation part ) const
{
    const ArticulationInfo& ai = _ail[ part ];
    return( ai._negate );
}

void HandNode::setDebug( bool enable )
{
    _debug = enable;
}
bool HandNode::getDebug() const
{
    return _debug;
}


// Support for moving to predefined hand poses.
// Possibly consider moving this class out to its own .cpp/.h files?
class MoveToPose : public osg::NodeCallback
{
public:
    MoveToPose( HandNode* hn, HandNode::Pose pose, float radiansPerSec )
      : _hn( hn ),
        _rate( radiansPerSec ),
        _lastTime( DBL_MAX ),
        _target( NULL )
    {
        switch( pose )
        {
        case HandNode::POSE_HOOK:
            _target = _poseHook;
            break;
        case HandNode::POSE_POINT:
            _target = _posePoint;
            break;
        case HandNode::POSE_FIST:
            _target = _poseFist;
            break;
        default:
            _target = _poseDefault;
            break;
        }
    }
    ~MoveToPose()
    {
    }

    virtual void operator()( osg::Node* node, osg::NodeVisitor* nv )
    {
        const double time( nv->getFrameStamp()->getSimulationTime() );
        if( _lastTime == DBL_MAX )
        {
            // First frame; record the time and continue.
            _lastTime = time;
            return;
        }

        // Calculate the articulation radians as specified rate * elapsed time.
        const double delta( _rate * (time - _lastTime) );
        _lastTime = time;

        bool done( true );
        int idx;
        for( idx=0; idx<HandNode::MAX_ARTICULATIONS; idx++ )
        {
            const double error( _target[ idx ] - _hn->getArticulation( idx ) );
            if( error == 0. ) // Already in position.
                continue;

            float setTo;
            if( error < -delta )
                setTo = _hn->getArticulation( idx ) - delta;
            else if( error > delta )
                setTo = _hn->getArticulation( idx ) + delta;
            else
                setTo = _target[ idx ];
            _hn->setArticulation( idx, setTo );
            done = false;
        }
        traverse( node, nv );

        if( done )
            // Everything is in position, remove the update callback.
            _hn->setUpdateCallback( NULL );
    }

protected:
    HandNode* _hn;
    float _rate;
    double _lastTime;

    float* _target;
    static float _poseDefault[ HandNode::MAX_ARTICULATIONS ];
    static float _poseHook[ HandNode::MAX_ARTICULATIONS ];
    static float _posePoint[ HandNode::MAX_ARTICULATIONS ];
    static float _poseFist[ HandNode::MAX_ARTICULATIONS ];
};

float MoveToPose::_poseDefault[] = {
//  F0   F1   F2   F3   F4
    0.f, 0.f, 0.f, 0.f, 0.f, // translation / flexure
    0.f, 0.f, 0.f, 0.f, 0.f, // inner knuckle
    0.f, 0.f, 0.f, 0.f, 0.f  // outer knuckle
};
float MoveToPose::_poseHook[] = {
    0.3f, 0.f, 0.f, 0.f, 0.f,
    0.55f, 0.25f, 2.25f, 2.1f, 2.2f,
    1.6f, 1.4f, 0.95f, 1.1f, 0.95f
};
float MoveToPose::_posePoint[] = {
    0.3f, 0.f, 0.f, 0.f, 0.f,
    0.55f, 0.f, 2.25f, 2.1f, 2.2f,
    1.6f, 0.f, 0.95f, 1.1f, 0.95f
};
float MoveToPose::_poseFist[] = {
    0.7f, -0.4f, -0.2f, -0.1f, 0.1f,
    0.2f, 1.7f, 1.75f, 1.85f, 1.85f,
    1.1f, 1.45f, 1.65f, 1.5f, 1.35f
};

void HandNode::setPose( Pose pose, float radiansPerSec )
{
    setUpdateCallback( new MoveToPose( this, pose, radiansPerSec ) );
}



HandNode::ArticulationInfo::ArticulationInfo()
  : _btChildIdx( -1 ),
    _debugIdx( -1 ),
    _angle( 0. ),
    _negate( false ),
    _dependent( NULL ),
    _cs( NULL )
{
}
HandNode::ArticulationInfo::~ArticulationInfo()
{
}

void HandNode::ArticulationInfo::setAngle( float angle )
{
    if( _negate )
        _angle = -angle;
    else
        _angle = angle;

    if( !_dof.valid() )
    {
        osg::notify( osg::WARN ) << "HandNode: Articulation has invalid DOFTransform." << std::endl;
        return;
    }

    osg::Vec3 hpr( 0., 0., _angle );
    _dof->setCurrentHPR( hpr );

    // If this is an inner transform (close to the palm), then let's update
    // the dependent transforms further out. Even though their angles haven't
    // changed, their absolute matrices have changed.
    if( _dependent != NULL)
        _dependent->setAngle( _dependent->getAngle() );

    setBulletTransform();
}
float HandNode::ArticulationInfo::getAngle() const
{
    if( _negate )
        return( -_angle );
    else
        return( _angle );
}

void HandNode::ArticulationInfo::setBulletTransform()
{
    if( _btChildIdx >= 0 )
    {
        // Get the absolute transform for the Bullet shape and update
        // the Bullet shape transformation within the larger compound shape.
        osg::Matrix l2w = osg::computeLocalToWorld( _l2wNodePath );
        osg::Matrix bt2osg = osg::Matrix::translate( _bt2osg );
        osg::Matrix btm = bt2osg * l2w;
        _cs->getChildList()[ _btChildIdx ].m_transform
            = osgBullet::asBtTransform( btm );

        // Update the debug rep.
        if( _debugIdx >= 0 )
            _debugBullet->setTransform( _debugIdx, btm );
    }

    // Let Bullet know the size of the new Aabb.
    _cs->recalculateLocalAabb();
}



void HandNode::dump() const
{
    // Create OSG files of: any child nodes, the hand subgraph, and the DebugBullet subgraph.
    osgDB::writeNodeFile( *( (Transform*)(this) ), "children.osg" );
    osgDB::writeNodeFile( *_hand, "hand.osg" );
    osgDB::writeNodeFile( *( _debugBullet->getRoot() ), "debug.osg" );

    // Display the Transform matrix.
    osg::Matrix m;
    computeLocalToWorldMatrix( m, NULL );
    osg::notify( osg::ALWAYS ) << "PAT local to world: " << m << std::endl;

    // Display all articulation angles in a manner that can easily be copied and
    // pasted into the source code as a new pose.
    osg::notify( osg::ALWAYS ) <<
        "Articulations: " << std::endl <<
        "//  F0   F1   F2   F3   F4" << std::endl <<
        "    " << getArticulation( 0 ) << ", " <<
        getArticulation( 1 ) << ", " <<
        getArticulation( 2 ) << ", " <<
        getArticulation( 3 ) << ", " <<
        getArticulation( 4 ) << ", // translation / flexure" << std::endl <<
        "    " << getArticulation( 5 ) << ", " <<
        getArticulation( 6 ) << ", " <<
        getArticulation( 7 ) << ", " <<
        getArticulation( 8 ) << ", " <<
        getArticulation( 9 ) << ", // inner knuckle" << std::endl <<
        "    " << getArticulation( 10 ) << ", " <<
        getArticulation( 11 ) << ", " <<
        getArticulation( 12 ) << ", " <<
        getArticulation( 13 ) << ", " <<
        getArticulation( 14 ) << " // outer knuckle" << std::endl << std::endl;
}


}
