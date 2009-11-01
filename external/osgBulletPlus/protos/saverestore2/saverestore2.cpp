// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgViewer/Viewer>
#include <osgGA/TrackballManipulator>
#include <osg/PositionAttitudeTransform>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/Geometry>
#include <osgUtil/Optimizer>

#include <osgbBulletPlus/DataLoader.h>
#include <osgbBullet/PhysicsState.h>
#include <osgbBullet/OSGToCollada.h>
#include <osgbBullet/MotionState.h>
#include <osgbBullet/Utils.h>
#include <osgbBullet/GLDebugDrawer.h>

#include <osgwTools/AbsoluteModelTransform.h>
#include <osgwTools/ParallelVisitor.h>
#include <osgwTools/InsertRemove.h>
#include <osgwTools/RefID.h>

#include <btBulletDynamicsCommon.h>

#include <osgwTools/FindNamedNode.h>

#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <osg/io_utils>


bool physicsEnabled( true );
//#define DBG_DRAW
#ifdef DBG_DRAW
osgbBullet::GLDebugDrawer* _dbgDraw( NULL );
#endif

void enablePhysics( osg::Node* root, const std::string& nodeName, btDynamicsWorld* bw );


btDynamicsWorld* bulletWorld( NULL );

// Save and restore global variable: The PhysicsState object,
// and a static unsigned int id.
osgbBullet::PhysicsState ps;
static unsigned int id( 0 );

osg::ref_ptr< osg::Group > _root;
osg::ref_ptr< osg::Group > _mt;

osgViewer::Viewer viewer;



// For testing purposes, our scene consists of the small drawer
// usmc23_4009.asm.ive, with physics enabled on two subparts:
// the handle (usmc23_4129.prt) and the side bracket
// (usmc23_4132.prt).
void
makeBasicScene()
{
    _root = new osg::Group;
    _mt = new osg::MatrixTransform( osg::Matrix::translate( 0., 0., 3. ) );
    _root->addChild( _mt.get() );

    // Load the drawer
    _mt->addChild( osgDB::readNodeFile( "usmc23_4009.asm.ive" ) );

    // Remove the ProxyNodes. Not needed.
    // Also, having them in the scene graph fouls up writing
    // the skeleton file during save.
    osgUtil::Optimizer opti;
    opti.optimize( _root.get(), osgUtil::Optimizer::REMOVE_LOADED_PROXY_NODES );
}

void
enableScene( btDynamicsWorld* bw )
{
    // Enable physics for front handle
    enablePhysics( _mt.get(), "USMC23_4129.PRT", bw );

    // Enable physics for side bracket
    enablePhysics( _mt.get(), "USMC23_4132.PRT", bw );
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

    dynamicsWorld->setGravity( btVector3( 0, 0, -9.8 ) );

    // Make the ground plane
    {
        btBoxShape* shape = new btBoxShape( btVector3( 6., 6., .5 ) );

		btRigidBody::btRigidBodyConstructionInfo rbInfo( 0., NULL, shape );
		btRigidBody* body = new btRigidBody( rbInfo );
        body->setCollisionFlags( body->getCollisionFlags() | btCollisionObject::CF_KINEMATIC_OBJECT );
        body->setActivationState( DISABLE_DEACTIVATION );

		dynamicsWorld->addRigidBody(body);
	}

    return( dynamicsWorld );
}



// Find the Node with name nodeName in the scene graph rooted at root.
// Enable physics for that node using the bullet world bw.
void
enablePhysics( osg::Node* root, const std::string& nodeName, btDynamicsWorld* bw )
{
    osgwTools::FindNamedNode fnn( nodeName );
    root->accept( fnn );
    if( fnn._napl.empty() )
    {
        osg::notify( osg::WARN ) << "Can't find node \"" << nodeName << "\"" << std::endl;
        return;
    }
    osg::Node* node = fnn._napl[ 0 ].first;
    osg::BoundingSphere bs( node->getBound() );
    osg::Group* parent = node->getParent( 0 );
    osg::NodePath np = fnn._napl[ 0 ].second;
    const osg::Matrix parentTrans = osg::computeLocalToWorld( np ); // Note that this might contain a scale.


    // Copy the subgraph for use with OSGToCollada.
    osg::Group* asGrp = node->asGroup();
    osg::ref_ptr< osg::Group > copyGrp = new osg::Group( *asGrp, osg::CopyOp::DEEP_COPY_ALL );

    osgbBullet::OSGToCollada converter;
#if 0
    // This currently crashes and has other issues. Many operations in
    // OSGToCOllada don't adhere to this setting, e.h., auto compute COM
    // is done on the _whole_ sg, FlattenStatisTransforms is done on the
    // _whole_ sg, etc.
    converter.setSceneGraph( root );
    converter.setNodeName( nodeName );
#else
    converter.setSceneGraph( copyGrp.get() );
#endif
    converter.setShapeType( CONVEX_HULL_SHAPE_PROXYTYPE );
    converter.setMass( 1. );
    converter.setOverall( true );

    converter.convert();

    osg::ref_ptr< osgwTools::AbsoluteModelTransform > model( new osgwTools::AbsoluteModelTransform );
    model->setDataVariance( osg::Object::DYNAMIC );
    osgwTools::insertAbove( node, model );

    btRigidBody* rb = converter.getRigidBody();
    osgbBullet::MotionState* motion = new osgbBullet::MotionState;
    motion->setTransform( model.get() );
    if( bs.center() != osg::Vec3( 0., 0., 0. ) )
        // If we don't have an explicit COM, and OSGToCollada auto compute was enabled (default),
        // then we need to explicitly set the COM here to be the bounding sphere center.
        motion->setCenterOfMass( bs.center() );
    // The parent transform (the local to world matrix extracted from the NodePath) might contain
    // a scale. However, the setParentTransform function orthonormalizes the matrix, which
    // eliminates any scale. So, even though Bullet doesn't support scaling, you can still pass in
    // a parent transform with a scale, and MotionState will just ignore it. (You must pass in any
    // parent scale transform using setScale.)
    motion->setParentTransform( parentTrans );
    rb->setMotionState( motion );
    bw->addRigidBody( rb );

    // Add to PhysicsSata for save/restore.
    osg::ref_ptr< osgbBullet::PhysicsData > pd = new osgbBullet::PhysicsData;
    pd->_cr = converter.getOrCreateCreationRecord();
    pd->_body = rb;
    std::ostringstream ostr;
    ostr << "id" << id++;
    osg::ref_ptr< osgwTools::RefID > rid = new osgwTools::RefID( ostr.str() );
    ps.addPhysicsData( rid.get(), pd.get() );

    model->setUserData( rid );
}


void
performSave()
{
#ifdef DBG_DRAW
    // Do not save the GLDebugDrawer.
    _root->removeChild( _dbgDraw->getSceneGraph() );
#endif

    // Save physics state.
    osgDB::writeObjectFile( ps, "physics_state_sr2.osgb" );

    // Make copy for skeleton file.
    osg::ref_ptr< osg::Group > grpCopy = new osg::Group( *_root,
        osg::CopyOp::DEEP_COPY_OBJECTS | osg::CopyOp::DEEP_COPY_NODES );

    osgDB::writeNodeFile( *grpCopy, "physics_state_skel.osg.skeleton" );

#ifdef DBG_DRAW
    // Re-enable debug draw.
    _root->addChild( _dbgDraw->getSceneGraph() );
#endif
}

struct AddAMTCallback : public osgwTools::ParallelVisitor::ParallelVisitorCallback
{
    virtual bool operator()( osg::Node& nodeA, osg::Node& nodeB )
    {
        if( nodeB.className() == std::string( "AbsoluteModelTransform" ) )
        {
            osgwTools::AbsoluteModelTransform* amtSkel = static_cast< osgwTools::AbsoluteModelTransform* >( &nodeB );
            osg::ref_ptr< osgwTools::AbsoluteModelTransform > amt = new osgwTools::AbsoluteModelTransform(
                *amtSkel, osg::CopyOp::DEEP_COPY_OBJECTS );
            amt->removeChildren( 0, amt->getNumChildren() );
            osgwTools::insertAbove( &nodeA, amt.get() );
        }
        return( true );
    }
};

class EnablePhysicsForRefID : public osg::NodeVisitor
{
public:
    EnablePhysicsForRefID( osgwTools::RefID* rid, osgbBullet::PhysicsData* pd )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _rid( rid ),
        _pd( pd )
    {}
    ~EnablePhysicsForRefID() {}

    virtual void apply( osg::Node& node )
    {
        osgwTools::RefID* rid = dynamic_cast< osgwTools::RefID* >( node.getUserData() );
        if( (rid == NULL) || (rid->str() != _rid->str()) )
        {
            traverse( node );
            return;
        }

        osgwTools::AbsoluteModelTransform* parent = dynamic_cast< osgwTools::AbsoluteModelTransform* >( &node );
        if( parent == NULL )
        {
            osg::notify( osg::FATAL ) << "EnablePhysicsForRefID: Node for enable physics is not an AMT." << std::endl;
            traverse( node );
            return;
        }
        if( parent->getNumChildren() != 1 )
        {
            osg::notify( osg::FATAL ) << "EnablePhysicsForRefID: Number of children should be 1, but is: " << parent->getNumChildren() << std::endl;
            traverse( node );
            return;
        }
        osg::Node* child = parent->getChild( 0 );


        // Found the node with the refID. Now enable physics for it.
        osgbBulletPlus::DataLoader* dl = new osgbBulletPlus::DataLoader( child, _pd.get(), bulletWorld );


        // Probably don't need to do this. Hard to imagine
        // a subgraph with physics enabled...?!?
        // traverse( node );
    }

protected:
    osg::ref_ptr< osgwTools::RefID > _rid;
    osg::ref_ptr< osgbBullet::PhysicsData > _pd;
    osg::ref_ptr< osg::Node > _node;
};

void
performRestore()
{
    viewer.setSceneData( NULL );

    bulletWorld->setDebugDrawer( NULL );
    delete bulletWorld;
    bulletWorld = initPhysics();

#ifdef DBG_DRAW
    _root->removeChild( _dbgDraw->getSceneGraph() );
    delete _dbgDraw;
    _dbgDraw = NULL;
#endif
    _root = NULL;

    makeBasicScene();
    viewer.setSceneData( _root.get() );

#ifdef DBG_DRAW
    _dbgDraw = new osgbBullet::GLDebugDrawer();
    _dbgDraw->setDebugMode( ~btIDebugDraw::DBG_DrawText );
    bulletWorld->setDebugDrawer( _dbgDraw );
    _root->addChild( _dbgDraw->getSceneGraph() );
#endif


    std::cout << "Starting restore" << std::endl;


    osg::ref_ptr< osg::Node > skel = osgDB::readNodeFile( "physics_state_skel.osg" );

    // Add AMTs from skeleton file to _root scene graph.
    osgwTools::ParallelVisitor pv( _root.get(), skel.get() );
    pv.setCallback( new AddAMTCallback );
    pv.compare();

    // Load physics state
    osg::ref_ptr< osg::Object > objIn = osgDB::readObjectFile( "physics_state_sr2.osgb" );
    osgbBullet::PhysicsState* ps = static_cast< osgbBullet::PhysicsState* >( objIn.get() );
    if( ps == NULL )
    {
        osg::notify( osg::FATAL ) << "performRestore: Error restoring file physics_state_sr2.osgb" << std::endl;
        return;
    }

    // find RefIDs in scene graph and enable physics for each subgraph.
    const osgbBullet::PhysicsState::DataMap dm( ps->getDataMap() );
    osgbBullet::PhysicsState::DataMap::const_iterator it;
    for( it = dm.begin(); it != dm.end(); it++ )
    {
        osg::ref_ptr< osgwTools::RefID > rid = new osgwTools::RefID( it->first );
        EnablePhysicsForRefID epr( rid.get(), it->second );
        _root->accept( epr );
    }


    std::cout << "Restore complete" << std::endl;
}

class SaveRestore : public osgGA::GUIEventHandler
{
public:
    SaveRestore()
    {}
    ~SaveRestore()
    {}

    bool handle( const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& )
    {
        switch( ea.getEventType() )
        {
            case osgGA::GUIEventAdapter::KEYDOWN:
            {
                if( ea.getKey() == 's' )
                {
                    performSave();
                    return( true );
                }
                else if( ea.getKey() == 'r' )
                {
                    performRestore();
                    return( true );
                }
            }
        }
        return( false );
    }
};


int
main( int argc,
      char ** argv )
{
    bulletWorld = initPhysics();

    makeBasicScene();
    enableScene( bulletWorld );


    viewer.setSceneData( _root.get() );
    viewer.setThreadingModel( osgViewer::ViewerBase::SingleThreaded );
    viewer.setUpViewInWindow( 500, 500, 800, 600 );
    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
    tb->setHomePosition( osg::Vec3( 0., -15., 3. ), osg::Vec3( 0., 0., 2. ), osg::Vec3( 0., 0., 1. ) );
    viewer.setCameraManipulator( tb );
    viewer.addEventHandler( new SaveRestore );


#ifdef DBG_DRAW
    _dbgDraw = new osgbBullet::GLDebugDrawer();
    _dbgDraw->setDebugMode( ~btIDebugDraw::DBG_DrawText );
    bulletWorld->setDebugDrawer( _dbgDraw );
    _root->addChild( _dbgDraw->getSceneGraph() );
#endif

    double currSimTime = viewer.getFrameStamp()->getSimulationTime();
    double prevSimTime = viewer.getFrameStamp()->getSimulationTime();
    viewer.realize();
    viewer.getCamera()->setClearColor( osg::Vec4( 0., 0., 0., 1. ) );

    while( !viewer.done() )
    {
        if( physicsEnabled )
        {
#ifdef DBG_DRAW
            _dbgDraw->BeginDraw();
#endif

            currSimTime = viewer.getFrameStamp()->getSimulationTime();
            bulletWorld->stepSimulation( currSimTime - prevSimTime );
            prevSimTime = currSimTime;

#ifdef DBG_DRAW
            bulletWorld->debugDrawWorld();
            _dbgDraw->EndDraw();
#endif
        }

        viewer.frame();
    }

#ifdef DBG_DRAW
    _root->removeChild( _dbgDraw->getSceneGraph() );
    delete _dbgDraw;
#endif

    return( 0 );
}

