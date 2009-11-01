// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgViewer/Viewer>
#include <osgGA/TrackballManipulator>
#include <osg/PositionAttitudeTransform>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/Geometry>

#include <osgwTools/AbsoluteModelTransform.h>
#include <osgwTools/Grid.h>

#include <osgbBullet/PhysicsState.h>
#include <osgbBulletPlus/DataLoader.h>
#include <osgbBullet/OSGToCollada.h>
#include <osgbBullet/MotionState.h>
#include <osgbBullet/Utils.h>
#include <osgbBullet/GLDebugDrawer.h>

#include <btBulletDynamicsCommon.h>

#include <osgwTools/FindNamedNode.h>

#include <vector>
#include <string>
#include <osg/io_utils>



// Save and restore global variable: The PhysicsState object,
// and a static unsigned int id.
osgbBullet::PhysicsState ps;
static unsigned int id( 0 );



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
        btBoxShape* shape = new btBoxShape( btVector3( 40., 40., .5 ) );

		btRigidBody::btRigidBodyConstructionInfo rbInfo( 0., NULL, shape );
		btRigidBody* body = new btRigidBody( rbInfo );
        body->setCollisionFlags( body->getCollisionFlags() | btCollisionObject::CF_KINEMATIC_OBJECT );
        body->setActivationState( DISABLE_DEACTIVATION );

		dynamicsWorld->addRigidBody(body);
	}

    return( dynamicsWorld );
}



// A class to save and restore a scene graph with physics data.
// During restore, fires off load/creation sequentially, so that
// rendering on a dual processor system remains smooth. (One core
// renders, the other loads scene graph data and creates physics data.)
class SaveRestore : public osgGA::GUIEventHandler
{
public:
    SaveRestore( osg::Group* root, btDynamicsWorld* bw )
      : _root( root ),
        _loadState( STARTUP ),
        _bw( bw ),
        _dl( NULL ),
        _ps( NULL )
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
                    // Save physics state.
                    osgDB::writeObjectFile( ps, "physics_state.osgb" );
                    return( true );
                }
                else if( ea.getKey() == 'r' )
                {
                    // Load physics state
                    // TBD? Currently blocks the main rendering thread. Should be OK for small
                    // files, but will be a problem for large files.
                    osg::ref_ptr< osg::Object > objIn = osgDB::readObjectFile( "physics_state.osgb" );
                    _ps = static_cast< osgbBullet::PhysicsState* >( objIn.get() );
                    if( ( _ps != NULL ) && ( !_ps->getDataMap().empty() ) )
                    {
                        osg::notify( osg::ALWAYS ) << "Loaded PhysicsState successfully." << std::endl;

                        // Prepare to load SG data / create physics data.
                        // See the update() method.
                        _dmItr = _ps->getDataMap().begin();
                        _loadState = LOADING;
                    }
                    else
                        osg::notify( osg::ALWAYS ) << "Error restoring physics state." << std::endl;
                    return( true );
                }
            }
        }
        return( false );
    }

    // Must be called once per frame. Checks the status of data load,
    // fires off new threads to load new data as needed, and returns
    // true when the last bit of data has been loaded, false if not
    // loading at all or in the process of loading.
    bool update()
    {
        if( _loadState == LOADING )
        {
            osg::notify( osg::ALWAYS ) << "SaveRestore::update: Loading." << std::endl;
            if( _dmItr != _ps->getDataMap().end() )
            {
                // There is still more data to load.

                if( _dl == NULL )
                {
                    // Not currently loading anything, so create a new DataLoader
                    // to load the next model & create physics data.

                    osg::notify( osg::ALWAYS ) << "SaveTestore::update: New DL." << std::endl;
                    osgwTools::AbsoluteModelTransform* amt = new osgwTools::AbsoluteModelTransform;
                    _root->addChild( amt );
                    osg::ref_ptr< osgbBullet::PhysicsData > pd = _dmItr->second;
                    _dl = new osgbBulletPlus::DataLoader( amt, pd->_fileName, pd.get(), _bw );
                    _dmItr++;
                }
                else if( ( _dl != NULL ) && ( _dl->loadComplete() ) )
                {
                    // We just finished loading something. Next time we get called,
                    // we'll take the above 'if' branch and start working on the
                    // next chunk.
                    osg::notify( osg::ALWAYS ) << "SaveTestore::update: Not at end, DL cleanup." << std::endl;
                    _dl = NULL;
                }
                // else we are actively loading something, so do nothing this time.
            }
            else
            {
                // There is no data left to load. We're either loading the
                // last chunk, or we just finished loading the last chunk.

                if( ( _dl != NULL ) && ( _dl->loadComplete() ) )
                {
                    // We just finished loading the last chunk. Clean up, and
                    // return true THIS ONE TIME ONLY.

                    osg::notify( osg::ALWAYS ) << "SaveTestore::update: At end, load complete, cleanup." << std::endl;
                    _dl = NULL;
                    _ps = NULL;
                    _loadState = STARTUP;
                    return( true );
                }
                // else we're still loading the last chunk.
            }
        }
        return( false );
    }

protected:
    enum LoadState {
        STARTUP = 0,
        LOADING = 1,
    };
    unsigned int _loadState;

    osg::ref_ptr< osg::Group > _root;
    btDynamicsWorld* _bw;

    osg::ref_ptr< osgbBulletPlus::DataLoader > _dl;
    osg::ref_ptr< osgbBullet::PhysicsState > _ps;
    osgbBullet::PhysicsState::DataMap::const_iterator _dmItr;
};


int
main( int argc,
      char ** argv )
{
    btDynamicsWorld* bulletWorld = initPhysics();
    osg::ref_ptr< osg::Group > root = new osg::Group;
    root->getOrCreateStateSet()->setMode( GL_NORMALIZE, osg::StateAttribute::ON );

    osg::Geode* geode = new osg::Geode;
    geode->addDrawable( osgwTools::makeGrid( osg::Vec3( -10, -10, 0 ), osg::Vec3( 20, 0, 0 ), osg::Vec3( 0, 20, 0 ), 5 ) );
    geode->addDrawable( osgwTools::makeGrid( osg::Vec3( -10, 10, 0 ), osg::Vec3( 20, 0, 0 ), osg::Vec3( 0, 0, 20 ), 5 ) );
    root->addChild( geode );


    osgViewer::Viewer viewer;
    viewer.setThreadingModel( osgViewer::ViewerBase::SingleThreaded );
    viewer.setSceneData( root.get() );
    viewer.setUpViewInWindow( 800, 400, 640, 480 );
    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
    tb->setHomePosition( osg::Vec3( 0., -70., 12. ), osg::Vec3( 0., 0., 12. ), osg::Vec3( 0., 0., 1. ) );
    viewer.setCameraManipulator( tb );

    SaveRestore* sr = new SaveRestore( root, bulletWorld );
    viewer.addEventHandler( sr );


    //osgbBullet::GLDebugDrawer* dbgDraw = new osgbBullet::GLDebugDrawer();
    //dbgDraw->setDebugMode( ~btIDebugDraw::DBG_DrawText );
    //bulletWorld->setDebugDrawer( dbgDraw );
    //root->addChild( dbgDraw->getSceneGraph() );

    double currSimTime = viewer.getFrameStamp()->getSimulationTime();
    double prevSimTime = viewer.getFrameStamp()->getSimulationTime();
    viewer.realize();

    bool loadComplete( false );
    while( !viewer.done() )
    {
        // Continue with load.
        if( !loadComplete )
            loadComplete = sr->update();

        //dbgDraw->BeginDraw();

        currSimTime = viewer.getFrameStamp()->getSimulationTime();
        if( loadComplete )
            // Load is complete. OK to start physics now.
            bulletWorld->stepSimulation( currSimTime - prevSimTime );
        prevSimTime = currSimTime;

        //bulletWorld->debugDrawWorld();
        //dbgDraw->EndDraw();

        viewer.frame();
    }

    return( 0 );
}

