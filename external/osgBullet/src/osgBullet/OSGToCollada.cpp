// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.

#include "osgBullet/OSGToCollada.h"
#include <btBulletDynamicsCommon.h>
#include <BulletColladaConverter/ColladaConverter.h>
#include "osgBullet/MotionState.h"
#include "osgBullet/Utils.h"
#include "osgBullet/AbsoluteModelTransform.h"

#include <osg/NodeVisitor>
#include <osg/MatrixTransform>
#include <osg/PositionAttitudeTransform>
#include <osg/ComputeBoundsVisitor>
#include <osgUtil/Simplifier>
#include <osgUtil/TransformAttributeFunctor>
#include <osg/io_utils>

using namespace osgBullet;



class ProcessSceneGraph : public osg::NodeVisitor
{
public:
    ProcessSceneGraph( const bool overall, const std::string& nodeName,
        const BroadphaseNativeTypes shapeType, const AXIS axis )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _overall( overall ),
        _nodeName( nodeName ),
        _shapeType( shapeType ),
        _axis( axis ),
        _shape( NULL ),
        _process( false )
    {
        if (!_overall)
            _shape = new btCompoundShape;
    }

    void apply( osg::Node& node )
    {
        osg::notify( osg::DEBUG_INFO ) << "Node" << std::endl;

        // If this is the _nodeName-specified subgraph root, toggle _process to off after calling traverse()
        bool clearProcess( false );
        // Process the entire subgraph rooted at this node?
        bool found( _overall );

        if( !_nodeName.empty() && !_process )
        {
            // _nodeName was specified and we did _not_ already encounter this node.
            if ( _nodeName != node.getName() )
                // This node's name doesn't match _nodeName
                found = false;
            else
                // Match! 'found' is already set based on _overall,
                //  so set this to true in case _overall is false.
                _process = clearProcess = true;
        }

        if (found)
        {
            _shape = createShape( node );
            return;
        }

        traverse( node );

        if( clearProcess )
            _process = false;;
    }

    void apply( osg::Geode& node )
    {
        osg::notify( osg::DEBUG_INFO ) << "Geode" << std::endl;

        // We're at a Geode. Regardless of _overall, we will process it.
        bool found( true );
        if( !_nodeName.empty() && !_process )
        {
            // But if a nodeName was specified and we didn't turn on _process at a higher level,
            // then we process this node only if the name matches.
            found = ( _nodeName == node.getName() );
        }

        if (found)
            createAndAddShape( node );
    }

    btCollisionShape* getShape()
    {
        return( _shape );
    }

protected:
    void createAndAddShape( osg::Node& node )
    {
        osg::notify( osg::DEBUG_INFO ) << "In createAndAddShape" << std::endl;

        btCollisionShape* child = createShape( node );
        if (child)
        {
            btCompoundShape* master = dynamic_cast< btCompoundShape* >( _shape );
            btTransform transform; transform.setIdentity();
            master->addChildShape( transform, child );
        }
    }
    btCollisionShape* createShape( osg::Node& node )
    {
        osg::notify( osg::DEBUG_INFO ) << "In createShape" << std::endl;

        btCollisionShape* collision( NULL );
        osg::Vec3 center;

        switch( _shapeType )
        {
        case BOX_SHAPE_PROXYTYPE:
        {
            osg::ComputeBoundsVisitor cbv;
            node.accept( cbv );
            osg::BoundingBox bb = cbv.getBoundingBox();
            center = bb.center();
            collision = osgBullet::btBoxCollisionShapeFromOSG( &node, &bb );
            break;
        }
        case SPHERE_SHAPE_PROXYTYPE:
        {
            osg::BoundingSphere bs = node.getBound();
            center = bs.center();
            collision = osgBullet::btSphereCollisionShapeFromOSG( &node );
            break;
        }
        case CYLINDER_SHAPE_PROXYTYPE:
        {
            osg::BoundingSphere bs = node.getBound();
            center = bs.center();
            collision = osgBullet::btCylinderCollisionShapeFromOSG( &node, _axis );
            break;
        }
        case TRIANGLE_MESH_SHAPE_PROXYTYPE:
        {
            collision = osgBullet::btTriMeshCollisionShapeFromOSG( &node );
            break;
        }
        case CONVEX_TRIANGLEMESH_SHAPE_PROXYTYPE:
        {
            collision = osgBullet::btConvexTriMeshCollisionShapeFromOSG( &node );
            break;
        }
        default:
        {
            osg::notify( osg::FATAL ) << "OSGToCollada: Error, unknown shape type, using tri mesh." << std::endl;
            break;
        }
        }

        if( collision && (center != osg::Vec3( 0., 0., 0. )) )
        {
            btTransform trans; trans.setIdentity();
            trans.setOrigin( osgBullet::asBtVector3( center ) );
            btCompoundShape* masterShape = new btCompoundShape();
            masterShape->addChildShape( trans, collision );
            collision = masterShape;
        }

        return( collision );
    }

    const bool _overall;
    const std::string& _nodeName;
    const BroadphaseNativeTypes _shapeType;
    const AXIS _axis;

    bool _process;

    btCollisionShape* _shape;
};

class FlattenStaticTransforms : public osg::NodeVisitor
{
public:
    FlattenStaticTransforms()
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN )
    {}

    void apply( osg::Transform& node )
    {
        osg::notify( osg::WARN ) << "OSGToCollada: Warning: Non-MatrixTransform encountered: (" <<
            node.className() << ") " << node.getName() << std::endl;
        traverse( node );
    }
    void apply( osg::MatrixTransform& node )
    {
        traverse( node );
        node.setMatrix( osg::Matrix::identity() );
    }
    void apply( osg::PositionAttitudeTransform& node )
    {
        traverse( node );
        node.setPosition(osg::Vec3(0.0f,0.0f,0.0f));
        node.setAttitude(osg::Quat());
        node.setPivotPoint(osg::Vec3(0.0f,0.0f,0.0f));
        node.setScale(osg::Vec3(1.0f,1.0f,1.0f));
    }

    void apply( osg::Geode& node )
    {
        osg::Matrix l2w = osg::computeLocalToWorld( getNodePath() );
        unsigned int idx;
        for( idx=0; idx<node.getNumDrawables(); idx++ )
            flattenDrawable( node.getDrawable( idx ), l2w );
    }

protected:
    void flattenDrawable( osg::Drawable* drawable, const osg::Matrix& matrix )
    {
        if (drawable)
        {
            osg::BoundingBox bb0 = drawable->getBound();
            osgUtil::TransformAttributeFunctor tf(matrix);
            drawable->accept(tf);
            drawable->dirtyBound();
            drawable->dirtyDisplayList();
            osg::BoundingBox bb1 = drawable->getBound();

            return;
        }
    }
};


OSGToCollada::OSGToCollada()
  : _sg( NULL ),
    _com( osg::Vec3( 0., 0., 0. ) ),
    _comSet( false ),
    _shapeType( TRIANGLE_MESH_SHAPE_PROXYTYPE ),
    _mass( 1.f ),
    _simplifyPercent( 1.f ),
    _overall( true ),
    _nodeName( std::string( "" ) ),
    _axis( osgBullet::Z )
{
}

bool OSGToCollada::convert( const std::string& outputFileName )
{
    osg::BoundingSphere bs = _sg->getBound();

    // Bullet collision shapes must be centered on the origin for correct
    // center of mass behavior. (TBD: In the future, we could allow the
    // calling app to specify a center of mass, then we would translate
    // to that pount instead of to the origin.)
    // Translate this subgraph so it is centered on the origin.
    osg::Vec3 com;
    osg::notify( osg::INFO ) << "OSGToCollada: ";
    if( getAutoComputeCenterOfMass() )
    {
        // Compute from bounding sphere.
        com = bs.center();
        osg::notify( osg::INFO ) << "Bounding sphere ";
    }
    else
    {
        // Use user-specified center of mass.
        com = _com;
        osg::notify( osg::INFO ) << "User-defined ";
    }
    osg::notify( osg::INFO ) << "center of mass: " << com << std::endl;
    osg::Matrix m( osg::Matrix::translate( -com ) );
    osg::ref_ptr< osg::MatrixTransform > root = new osg::MatrixTransform( m );
    root->setDataVariance( osg::Object::STATIC );
    root->setName( "CenterOfMassOffset" );
    root->addChild( _sg.get() );

    // Run the simplifier, if requested. Note we might develop a new
    // polygon decimator in the future, as the simplifier isn't really
    // cutting the mustard for us.
    if ( _simplifyPercent != 1.f )
    {
        osg::notify( osg::INFO ) << "OSGToCollada: Running Simplifier." << std::endl;
        osgUtil::Simplifier simple;
        simple.setSampleRatio( _simplifyPercent );
        root->accept( simple );
    }

    // Flatten transforms so that we don't need to multiply geometry by the current
    // OSG local to world matrix during traversal.
    // NOTE: Must remove loaded ProxyNodes in order to allow
    //   static transforms to be flattened.
    osg::notify( osg::INFO ) << "OSGToCollada: Flattening transforms." << std::endl;
    FlattenStaticTransforms fst;
    root->accept( fst );


    osg::notify( osg::INFO ) << "OSGToCollada: ProcessSceneGraph." << std::endl;
    ProcessSceneGraph psg( _overall, _nodeName, _shapeType, _axis );
    root->accept( psg );

    _rigidBody = createRigidBody( _mass, psg.getShape(),
        com, _sg.get() );


    if (!_rigidBody)
    {
        osg::notify( osg::FATAL ) << "OSGToCollada: Unable to create physics data." << std::endl;
        return false;
    }
    else if (outputFileName.empty())
    {
        osg::notify( osg::INFO ) << "OSGToCollada: No output file name, not writing DAE." << std::endl;
        return true;
    }
    else
    {
        btDynamicsWorld* dynamicsWorld = initPhysics();
        dynamicsWorld->addRigidBody( _rigidBody );

        ColladaConverter* cc = new ColladaConverter( dynamicsWorld );
        cc->save( outputFileName.c_str() );

        // clean up.
        delete cc;

        dynamicsWorld->removeRigidBody( _rigidBody );

        delete dynamicsWorld->getBroadphase();
        delete dynamicsWorld->getConstraintSolver();
        delete dynamicsWorld->getDispatcher();
        delete dynamicsWorld;

        return true;
    }
}

btRigidBody*
OSGToCollada::getRigidBody()
{
    return( _rigidBody );
}

void
OSGToCollada::setSceneGraph( osg::Node* sg )
{
    _sg = sg;
}
osg::Node*
OSGToCollada::getSceneGraph() const
{
    return( _sg.get() );
}

void
OSGToCollada::setCenterOfMass( osg::Vec3& com )
{
    _com = com;
    setAutoComputeCenterOfMass( false );
}
const osg::Vec3&
OSGToCollada::getCenterOfMass() const
{
    return( _com );
}
void
OSGToCollada::setAutoComputeCenterOfMass( bool compute )
{
    _comSet = !compute;
}
bool
OSGToCollada::getAutoComputeCenterOfMass() const
{
    return( !_comSet );
}

void
OSGToCollada::setShapeType( const BroadphaseNativeTypes shapeType )
{
    _shapeType = shapeType;
}
BroadphaseNativeTypes
OSGToCollada::getShapeType() const
{
    return( _shapeType );
}

void
OSGToCollada::setMass( float mass )
{
    _mass = mass;
}
float
OSGToCollada::getMass() const
{
    return( _mass );
}

void
OSGToCollada::setSimplifyPercent( float simplifyPercent )
{
    _simplifyPercent = simplifyPercent;
}
float
OSGToCollada::getSimplifyPercent() const
{
    return( _simplifyPercent );
}

void
OSGToCollada::setOverall( bool overall )
{
    _overall = overall;
}
bool
OSGToCollada::getOverall() const
{
    return( _overall );
}

void
OSGToCollada::setNodeName( const std::string& nodeName )
{
    _nodeName = nodeName;
}
const std::string&
OSGToCollada::getNodeName() const
{
    return( _nodeName );
}

void
OSGToCollada::setAxis( osgBullet::AXIS axis )
{
    _axis = axis;
}
osgBullet::AXIS
OSGToCollada::getAxis() const
{
    return( _axis );
}


btDynamicsWorld*
OSGToCollada::initPhysics()
{
    btDefaultCollisionConfiguration* collisionConfiguration = new btDefaultCollisionConfiguration();
    btCollisionDispatcher* dispatcher = new btCollisionDispatcher( collisionConfiguration );
    btConstraintSolver* solver = new btSequentialImpulseConstraintSolver;

    btVector3 worldAabbMin( -10000, -10000, -10000 );
    btVector3 worldAabbMax( 10000, 10000, 10000 );
    btBroadphaseInterface* inter = new btAxisSweep3( worldAabbMin, worldAabbMax, 1000 );

    btDynamicsWorld* dynamicsWorld = new btDiscreteDynamicsWorld( dispatcher, inter, solver, collisionConfiguration );

    dynamicsWorld->setGravity( btVector3( 0, 0, -9.8 ));

    return( dynamicsWorld );
}

btRigidBody*
OSGToCollada::createRigidBody( btScalar mass,
                              btCollisionShape* shape, const osg::Vec3& centerOfMass, osg::Node* node )
{
    if( (shape == NULL) || (node == NULL) )
        return NULL;

    btTransform startTransform; startTransform.setIdentity();

	btVector3 localInertia( 0, 0, 0 );
	const bool isDynamic = (mass != 0.f);
	if (isDynamic)
		shape->calculateLocalInertia( mass, localInertia );

    osgBullet::MotionState* motion( NULL );
    osg::Transform* trans = dynamic_cast< osg::Transform* >( node );
    if( trans != NULL )
    {
        motion = new osgBullet::MotionState();
        motion->setTransform( trans );
        motion->setCenterOfMass( centerOfMass );
    }

    btRigidBody::btRigidBodyConstructionInfo rbInfo( mass, motion, shape, localInertia );
    rbInfo.m_friction = btScalar( 1. );
	return( new btRigidBody( rbInfo ) );
}

