// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.

#include "osgBullet/OSGToCollada.h"
#include <btBulletDynamicsCommon.h>
#include <BulletColladaConverter/ColladaConverter.h>
#include "osgBullet/MotionState.h"
#include "osgBullet/Utils.h"

#include <osg/NodeVisitor>
#include <osg/MatrixTransform>
#include <osg/PositionAttitudeTransform>
#include <osg/PolygonMode>
#include <osg/PolygonOffset>
#include <osg/ComputeBoundsVisitor>
#include <osgUtil/Simplifier>
#include <osgUtil/Optimizer>
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
        osg::notify( osg::ALWAYS ) << "Node" << std::endl;

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
        osg::notify( osg::ALWAYS ) << "Geode" << std::endl;

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
        osg::notify( osg::ALWAYS ) << "In createAndAddShape" << std::endl;

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
        osg::notify( osg::ALWAYS ) << "In createShape" << std::endl;

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
            osg::Vec3 center = bs.center();
            collision = osgBullet::btSphereCollisionShapeFromOSG( &node );
            break;
        }
        case CYLINDER_SHAPE_PROXYTYPE:
        {
            osg::BoundingSphere bs = node.getBound();
            osg::Vec3 center = bs.center();
            collision = osgBullet::btCylinderCollisionShapeFromOSG( &node, _axis );
            break;
        }
        case TRIANGLE_MESH_SHAPE_PROXYTYPE:
        {
            osg::BoundingSphere bs = node.getBound();
            collision = osgBullet::btTriMeshCollisionShapeFromOSG( &node );
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

class TransformWatning : public osg::NodeVisitor
{
public:
    TransformWatning()
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _warn( false )
    {
    }

    void apply( osg::Transform& node )
    {
        osg::Matrix m;
        bool result( node.computeLocalToWorldMatrix( m, NULL ) );
        if ( !result )
            osg::notify( osg::WARN ) << "OSGToCollada: computeLocalToWorldMatrix() failed." << std::endl;
        if( !_warn && !( m.isIdentity() ) )
        {
            osg::notify( osg::WARN ) << "OSGToCollada: Non-identity transform encountered." << std::endl;
            _warn = true;
        }
        traverse( node );
    }

protected:
    bool _warn;
};


OSGToCollada::OSGToCollada( 
        osg::Node* sg,
        const BroadphaseNativeTypes shapeType,
        const float mass,
        const std::string& outputFileName,
        const float simplifyPercent,
        const bool overall,
        const std::string& nodeName,
        const AXIS axis )
{
    osg::BoundingSphere bs = sg->getBound();
    osg::Matrix m( osg::Matrix::translate( -bs.center() ) );
    osg::ref_ptr< osg::MatrixTransform > root = new osg::MatrixTransform( m );
    root->setDataVariance( osg::Object::STATIC );
    root->addChild( sg );

    if ( simplifyPercent != 1.f )
    {
        osg::notify( osg::INFO ) << "OSGToCollada: Running Simplifier." << std::endl;
        osgUtil::Simplifier simple;
        simple.setSampleRatio( simplifyPercent );
        root->accept( simple );
    }

    // Flatten transforms so that we don't need to multiply geometry by the current
    // OSG local to world matrix during traversal.
    // NOTE: Must remove loaded ProxyNodes in order to allow
    //   static transforms to be flattened.
    osg::notify( osg::INFO ) << "OSGToCollada: Running Optimizer." << std::endl;
    osgUtil::Optimizer optimize;
    optimize.optimize( root.get(),
        osgUtil::Optimizer::FLATTEN_STATIC_TRANSFORMS |
        osgUtil::Optimizer::REMOVE_LOADED_PROXY_NODES );

    // Warn if any non-identity transforms are present.
    osg::notify( osg::INFO ) << "OSGToCollada: Checking for non-identity transforms." << std::endl;
    TransformWatning tw;
    root->accept( tw );


    osg::notify( osg::INFO ) << "OSGToCollada: ProcessSceneGraph." << std::endl;
    ProcessSceneGraph psg( overall, nodeName, shapeType, axis );
    root->accept( psg );

    _rigidBody = createRigidBody( mass, psg.getShape(),
        osgBullet::asBtVector3( bs.center() ),
        dynamic_cast< osg::MatrixTransform* >( sg ) );


    if (!_rigidBody)
        osg::notify( osg::FATAL ) << "OSGToCollada: Unable to create physics data." << std::endl;
    else if (outputFileName.empty())
        osg::notify( osg::WARN ) << "OSGToCollada: no output file name." << std::endl;
    else
    {
        btDynamicsWorld* dynamicsWorld = initPhysics();
        dynamicsWorld->addRigidBody( _rigidBody );

        ColladaConverter* cc = new ColladaConverter( dynamicsWorld );
        cc->save( outputFileName.c_str() );
    }
}

btRigidBody*
OSGToCollada::getRigidBody()
{
    return( _rigidBody );
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
    btCollisionShape* shape, btVector3 centerOfMass, osg::MatrixTransform* mt )
{
    if( (shape == NULL) || (mt == NULL) )
        return NULL;

    btTransform startTransform; startTransform.setIdentity();

	btVector3 localInertia( 0, 0, 0 );
	const bool isDynamic = (mass != 0.f);
	if (isDynamic)
		shape->calculateLocalInertia( mass, localInertia );

    osgBullet::MotionState* motion = new osgBullet::MotionState();
    motion->setMatrixTransform( mt );
    motion->setInverseParentWorldTransform( osg::Matrix::identity() );

    btTransform com; com.setIdentity();
    com.setOrigin( centerOfMass );
    motion->_centerOfMass = osgBullet::asOsgVec3( centerOfMass );

    motion->setWorldTransform( com * startTransform );

    osg::notify( osg::INFO ) << "OSGToCollada: world transform: " <<
        osgBullet::asOsgVec3( startTransform.getOrigin() ) <<
        ", COM: " << osgBullet::asOsgVec3( centerOfMass ) <<
        std::endl;

    btRigidBody::btRigidBodyConstructionInfo rbInfo( mass, motion, shape, localInertia );
    rbInfo.m_friction = btScalar( 1. );
	return( new btRigidBody( rbInfo ) );
}

