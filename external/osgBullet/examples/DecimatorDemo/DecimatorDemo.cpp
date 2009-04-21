//
// Copyright (c) 2009 Skew Matrix Software LLC.
// All rights reserved.
//


#include <osgViewer/Viewer>
#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgDB/FileNameUtils>
#include <osgDB/FileUtils>
#include <osgGA/TrackballManipulator>
#include <osg/ShapeDrawable>
#include <osg/Geode>

#include <btBulletDynamicsCommon.h>

#include <osg/io_utils>
#include <iostream>
#include <sstream>

#include <osgBullet/AbsoluteModelTransform.h>
#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/RigidBody.h>
#include <osgBullet/OSGToCollada.h>
#include <osgBullet/DecimatorOp.h>
#include <osgBullet/VertexAggOp.h>
#include <osgBullet/GeometryModifier.h>
#include <osgBullet/DebugBullet.h>
#include <osgBullet/Utils.h>
#include <osg/NodeVisitor>
#include <osg/MatrixTransform>
#include <osg/PositionAttitudeTransform>
#include <osg/ComputeBoundsVisitor>
#include <osgUtil/TransformAttributeFunctor>
#include <osg/Version>
#include <osg/io_utils>
#include <osgGA/StateSetManipulator>


osgBullet::DebugBullet _debugBullet;


btDynamicsWorld* initPhysics()
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

osg::Transform* createOSGBox( osg::Vec3 size )
{
    osg::Box* box = new osg::Box();
    box->setHalfLengths( size );

    osg::ShapeDrawable* shape = new osg::ShapeDrawable( box );
    shape->setColor( osg::Vec4( 1., 1., 1., 1. ) );
    osg::Geode* geode = new osg::Geode();
    geode->addDrawable( shape );

    osg::MatrixTransform* mt = new osg::MatrixTransform();
    mt->addChild( geode );

    return( mt );
}

osg::Node*
createGround( float w, float h, const osg::Vec3& center )
{
    osg::Transform* ground = createOSGBox( osg::Vec3( w, h, .01 ) );

    osgBullet::OSGToCollada converter;
    converter.setSceneGraph( ground );
    converter.setShapeType( BOX_SHAPE_PROXYTYPE );
    converter.setMass( 0.f );
    converter.convert();

    btRigidBody* body = converter.getRigidBody();

    // OSGToCollada flattens transformation to transform all
    // verts, but that doesn't work with ShapeDrawables, so we must
    // transform the box explicitly.
    osgBullet::MotionState* motion = dynamic_cast< osgBullet::MotionState* >( body->getMotionState() );
    osg::Matrix m( osg::Matrix::translate( center ) );
    motion->setParentTransform( m );
    body->setWorldTransform( osgBullet::asBtTransform( m ) );

    ground->setUserData( new osgBullet::RigidBody( body ) );

    return ground;
}

int main( int argc,
          char* argv[] )
{
    osg::ArgumentParser arguments( &argc, argv );

    arguments.getApplicationUsage()->setApplicationName( arguments.getApplicationName() );
    arguments.getApplicationUsage()->setDescription( arguments.getApplicationName() + " shows a before and after image of the DecimatorOp module, using a default decimation of 0.6." );
    arguments.getApplicationUsage()->setCommandLineUsage( arguments.getApplicationName() + " [options] filename ..." );

    arguments.getApplicationUsage()->addCommandLineOption( "--decPercent <n>", "Runs the DecimatorOp on the scene graph before generating the Bullet collision shape. <n> is the target percentage of vertices to remove, and is usually in the range 0.0 to 1.0.Default 0.6" );
    arguments.getApplicationUsage()->addCommandLineOption( "--decMaxError <n>", "Specifies the Decimator maximum error tolerance. Geometry exceeding this tolerance is not reduced. <n> is in the range 0.0 to FLT_MAX. Default FLT_MAX" );
    arguments.getApplicationUsage()->addCommandLineOption( "--respectBoundries", "Will not decimate boundry polygons, will not decimate fully but may fix some mesh errors. Default False" );

    if( arguments.read( "-h" ) || arguments.read( "--help" ) )
    {
        arguments.getApplicationUsage()->write( osg::notify( osg::ALWAYS ), osg::ApplicationUsage::COMMAND_LINE_OPTION );
        return 1;
    }

    if (arguments.errors())
    {
        arguments.writeErrorMessages( osg::notify( osg::FATAL ) );
        return 1;
    }


    float decimatorPercent( .6 );
    std::string str;
    if ( arguments.read( "--decPercent", str ) )
    {
        if( sscanf( str.c_str(), "%f", &decimatorPercent ) != 1 )
        {
            arguments.getApplicationUsage()->write( osg::notify( osg::FATAL ) );
            return 1;
        }

    }
    float decimatorMaxError( FLT_MAX );
    if ( arguments.read( "--decMaxError", str ) )
    {
        if( sscanf( str.c_str(), "%f", &decimatorMaxError ) != 1 )
        {
            arguments.getApplicationUsage()->write( osg::notify( osg::FATAL ) );
            return 1;
        }
    }
    bool decimatorIgnoreBoundries = (true);
    if (arguments.read( "--respectBoundries" ))
        decimatorIgnoreBoundries = false;

    if (decimatorPercent < 1.f )
        osg::notify( osg::INFO ) << "osgbpp: DecimatorOp: " << decimatorPercent << ", " << decimatorMaxError << std::endl;

    bool useAgg = false;
    unsigned int aggMaxVerticies( 0 );
    osg::Vec3 aggMinCellSize( 0., 0., 0. );
    if(arguments.read( "--aggMaxVerts", str) )
    {
        if( sscanf( str.c_str(), "%u", &aggMaxVerticies) != 1 )
        {
            arguments.getApplicationUsage()->write( osg::notify( osg::FATAL ) );
        }
        useAgg = true;
        if ( arguments.read( "--aggMinCellSize", str ) )
        {
            char comma;
            std::istringstream oStr( str );
            oStr >> aggMinCellSize[ 0 ] >> comma >>
                aggMinCellSize[ 1 ] >> comma >>
                aggMinCellSize[ 2 ];
        }
    }
    
    osg::Node*  model = osgDB::readNodeFiles( arguments );
    if( !model )
        {
                osg::notify( osg::FATAL ) << "Can't load input file(s)." << std::endl;
                return 1;
           
        }
    osg::Group* grporig = new osg::Group;
    grporig->addChild(model);
    osg::notify( osg::INFO ) << "osgbpp: Loaded model(s)." << std::endl;

    osg::Group* grpcopy = new osg::Group( *grporig , osg::CopyOp::DEEP_COPY_ALL);

    osgBullet::GeometryOperation* reducer;
    if(!useAgg)
    {
        osgBullet::DecimatorOp* decimate = new osgBullet::DecimatorOp;
        decimate->setSampleRatio(decimatorPercent);
        decimate->setMaximumError(decimatorMaxError);
        decimate->setIgnoreBoundries(decimatorIgnoreBoundries);
        reducer = decimate;
    }else
    {
        osgBullet::VertexAggOp* vertagg = new osgBullet::VertexAggOp;
        vertagg->setMaxVertsPerCell( aggMaxVerticies );
        vertagg->setMinCellSize( aggMinCellSize );reducer = vertagg;
        reducer = vertagg;

    }
    osgBullet::GeometryModifier modifier(reducer);

    grpcopy->accept( modifier);
    modifier.displayStatistics(osg::notify(osg::NOTICE));
    

    osgViewer::Viewer viewer;

    osg::ref_ptr<osg::Group> root = new osg::Group();
    root->addChild(grporig);


    osg::BoundingSphere bs = grporig->getBound();
    
    osg::Matrix m;
    osg::ref_ptr<osg::MatrixTransform> mt = new osg::MatrixTransform;
    m.makeTranslate( 2.f * bs.radius(), 0.f, 0.f );
    mt->setMatrix( m );
    root->addChild( mt.get() );
    mt->addChild( grpcopy );

    viewer.setSceneData( root.get() );
    viewer.addEventHandler( new osgGA::StateSetManipulator(viewer.getCamera()->getOrCreateStateSet()) );
    viewer.run();

    //std::string out( "C:/Projects/OpenSceneGraph-Data/decimatorDemo.osg" );
    //osgDB::writeNodeFile( *(root.get()), out ); 

    return( 0 );
}
