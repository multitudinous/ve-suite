//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//


#include <osgViewer/Viewer>
#include <osgDB/ReadFile>
#include <osgDB/FileNameUtils>
#include <osgDB/FileUtils>
#include <osgGA/TrackballManipulator>
#include <osg/ShapeDrawable>
#include <osg/Geode>

#include <btBulletDynamicsCommon.h>

#include <osg/io_utils>
#include <iostream>

#include <osgBullet/AbsoluteModelTransform.h>
#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/RigidBody.h>
#include <osgBullet/OSGToCollada.h>
#include <osgBullet/DebugBullet.h>
#include <osgBullet/Utils.h>


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

    osgBullet::OSGToCollada converter( ground, BOX_SHAPE_PROXYTYPE, 0.f );

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
    arguments.getApplicationUsage()->setDescription( arguments.getApplicationName() + " creates physics data for model files and stores that data to COLLADA files." );
    arguments.getApplicationUsage()->setCommandLineUsage( arguments.getApplicationName() + " [options] filename ..." );
    arguments.getApplicationUsage()->addCommandLineOption( "--box",          "Creates a box collision shape." );
    arguments.getApplicationUsage()->addCommandLineOption( "--sphere",       "Creates a sphere collision shape." );
    arguments.getApplicationUsage()->addCommandLineOption( "--cylinder",     "Creates a cylinder collision shape." );
    arguments.getApplicationUsage()->addCommandLineOption( "--axis <x>",     "Specify the cylinder axis, \"X\", \"Y\", or \"Z\". Default is \"Z\"." );
    arguments.getApplicationUsage()->addCommandLineOption( "--triMesh",      "Default. Creates a tri mesh collision shape." );
    arguments.getApplicationUsage()->addCommandLineOption( "--simplify <n>", "Runs the osgUtil::Simplifier on the scene graph before generating the Bullet collition shape. <n> should be in the range 0.0 < n < 1.0." );
    arguments.getApplicationUsage()->addCommandLineOption( "--overall",      "Creates a single collision shape for the entire input scene graph (or named node; see --name), rather than a collision shape per Geode (the default)" );
    arguments.getApplicationUsage()->addCommandLineOption( "--name <name>",  "Interprets the scene graph from the first occurance of the names node. If not specified, the entire scene graph is processed." );
    arguments.getApplicationUsage()->addCommandLineOption( "--mass <n>",     "Specify the desired rigid body mass value. Default is 1.0." );
    arguments.getApplicationUsage()->addCommandLineOption( "-o <name.dae>",  "Output file name. If not present, the output file name is derived from the input file name by replacing the extension with \".dae\"." );
    arguments.getApplicationUsage()->addCommandLineOption( "--display",      "Disable displaying the loaded model (just convert)." );

    unsigned int helpType = 0;
    if ( (helpType = arguments.readHelpType()) )
    {
        arguments.getApplicationUsage()->write( osg::notify( osg::ALWAYS ), helpType );
        return 1;
    }

    if (arguments.errors())
    {
        arguments.writeErrorMessages( osg::notify( osg::FATAL ) );
        return 1;
    }

    if ( arguments.argc() <= 1 )
    {
        arguments.getApplicationUsage()->write( osg::notify( osg::FATAL ), osg::ApplicationUsage::COMMAND_LINE_OPTION );
        return 1;
    }


    // Get all arguments.
    BroadphaseNativeTypes shapeType( TRIANGLE_MESH_SHAPE_PROXYTYPE );
    if( arguments.read( "--box" ) ) shapeType = BOX_SHAPE_PROXYTYPE;
    if( arguments.read( "--sphere" ) ) shapeType = SPHERE_SHAPE_PROXYTYPE;
    if( arguments.read( "--cylinder" ) ) shapeType = CYLINDER_SHAPE_PROXYTYPE;
    if( arguments.read( "--triMesh" ) ) shapeType = TRIANGLE_MESH_SHAPE_PROXYTYPE;

    switch( shapeType )
    {
    case BOX_SHAPE_PROXYTYPE:
        osg::notify( osg::INFO ) << "osgbpp: Box" << std::endl;
        break;
    case SPHERE_SHAPE_PROXYTYPE:
        osg::notify( osg::INFO ) << "osgbpp: Sphere" << std::endl;
        break;
    case CYLINDER_SHAPE_PROXYTYPE:
        osg::notify( osg::INFO ) << "osgbpp: Cylinder" << std::endl;
        break;
    case TRIANGLE_MESH_SHAPE_PROXYTYPE:
        osg::notify( osg::INFO ) << "osgbpp: TriMesh" << std::endl;
        break;
    default:
        osg::notify( osg::FATAL ) << "osgbpp: Error, unknown shape type, using tri mesh." << std::endl;
        shapeType = TRIANGLE_MESH_SHAPE_PROXYTYPE;
        break;
    }

    std::string str;
    osgBullet::AXIS axis( osgBullet::Z );
    if ( arguments.read( "--axis", str ) )
    {
        if( (str.find( "X" ) != str.npos) || (str.find( "x" ) != str.npos) )
            axis = osgBullet::X;
        else if( (str.find( "Y" ) != str.npos) || (str.find( "y" ) != str.npos) )
            axis = osgBullet::Y;
        else if( (str.find( "Z" ) != str.npos) || (str.find( "z" ) != str.npos) )
            axis = osgBullet::Z;
        else
        {
            arguments.getApplicationUsage()->write( osg::notify( osg::FATAL ) );
            return 1;
        }
    }
    switch( axis )
    {
    case osgBullet::X:
        osg::notify( osg::INFO ) << "osgbpp: Axis: X" << std::endl;
        break;
    case osgBullet::Y:
        osg::notify( osg::INFO ) << "osgbpp: Axis: Y" << std::endl;
        break;
    case osgBullet::Z:
        osg::notify( osg::INFO ) << "osgbpp: Axis: Z" << std::endl;
        break;
    }

    float simplifyPercent = 1.f;
    if ( arguments.read( "--simplify", str ) )
    {
        if( sscanf( str.c_str(), "%f", &simplifyPercent ) != 1 )
        {
            arguments.getApplicationUsage()->write( osg::notify( osg::FATAL ) );
            return 1;
        }
    }
    if (simplifyPercent != 1.f )
        osg::notify( osg::INFO ) << "osgbpp: Simplify: " << simplifyPercent << std::endl;

    const bool overall( arguments.read( "--overall" ) );
    if (overall)
        osg::notify( osg::INFO ) << "osgbpp: Overall" << std::endl;

    std::string nodeName;
    if ( arguments.read( "--name", nodeName ) )
        osg::notify( osg::INFO ) << "osgbpp: Node name: " << nodeName << std::endl;

    float mass( 1.f );
    if ( arguments.read( "--mass", str ) )
    {
        if( sscanf( str.c_str(), "%f", &mass ) != 1 )
        {
            arguments.getApplicationUsage()->write( osg::notify( osg::FATAL ) );
            return 1;
        }
    }
    if (mass != 1.f )
        osg::notify( osg::INFO ) << "osgbpp: Mass: " << mass << std::endl;

    std::string outputFileName;
    if (! arguments.read( "-o", outputFileName ) )
    {
        // Find the first non-option and try to treat it like a file name.
        for( int pos = 1; pos < arguments.argc(); ++pos )
        {
            if ( ! arguments.isOption( pos ) )
            {
                outputFileName = osgDB::getNameLessExtension( arguments[ pos ] ) + ".dae";
                break;
            }
        }
    }
    osg::notify( osg::INFO ) << "osgbpp: Using output file name: " << outputFileName << std::endl;

    const bool display( arguments.read( "--display" ) );
    if (display)
        osg::notify( osg::INFO ) << "osgbpp: Display" << std::endl;



    osg::ref_ptr< osg::Node > model = osgDB::readNodeFiles( arguments );
    if( !model )
    {
        osg::notify( osg::FATAL ) << "Can't load input file(s)." << std::endl;
        return 1;
    }
    osg::notify( osg::INFO ) << "osgbpp: Loaded model(s)." << std::endl;


    osgBullet::OSGToCollada converter( 
        model.get(),
        shapeType,
        mass,
        outputFileName,
        simplifyPercent,
        overall,
        nodeName,
        axis );
    osg::notify( osg::INFO ) << "osgbpp: Completed Collada conversion." << std::endl;

    // TBD we can deallocate 'model' here, but don't want to deallocate 'converter' yet...

    if (!display)
        return 0;


    osg::ref_ptr< osgBullet::AbsoluteModelTransform > loadedModel( new osgBullet::AbsoluteModelTransform );
    {
        osg::Node* load = osgDB::readNodeFiles( arguments );
        if( !load )
        {
            osg::notify( osg::FATAL ) << "Can't load input file(s)." << std::endl;
            return 1;
        }
        loadedModel->addChild( load );
        if( mass != 0)
            loadedModel->setDataVariance( osg::Object::DYNAMIC );
    }
    osg::notify( osg::INFO ) << "osgbpp: Reloaded model(s) for display." << std::endl;

    btRigidBody* rb = converter.getRigidBody();
    osgBullet::MotionState* motion = new osgBullet::MotionState;
    motion->setTransform( loadedModel );
    osg::BoundingSphere bs = loadedModel->getBound();

    // Add visual rep of Bullet Collision shape.
    osg::Node* visNode = osgBullet::osgNodeFromBtCollisionShape( rb->getCollisionShape() );
    if( visNode != NULL )
    {
        osgBullet::AbsoluteModelTransform* dmt = new osgBullet::AbsoluteModelTransform;
        dmt->addChild( visNode );
        motion->setDebugTransform( dmt );
        _debugBullet.addDynamic( dmt );
    }

    motion->setCenterOfMass( bs.center() );
    rb->setMotionState( motion );


    osgViewer::Viewer viewer( arguments );

    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator();
    viewer.setCameraManipulator( tb );

    osg::ref_ptr<osg::Group> root = new osg::Group();
    root->addChild( loadedModel.get() );
    root->addChild( _debugBullet.getRoot() );


    btDynamicsWorld* dynamicsWorld = initPhysics();
    dynamicsWorld->addRigidBody( rb );

    // Compute a reasonable ground plane size based on the bounding sphere radius.
    float dim = loadedModel->getBound()._radius * 1.5;
    osg::Vec3 cen = loadedModel->getBound()._center;
    cen[ 2 ] -= dim;
    osg::ref_ptr< osg::Node > ground = createGround( dim, dim, cen );
    root->addChild( ground.get() );
    osgBullet::RigidBody* body = dynamic_cast< osgBullet::RigidBody* >( ground->getUserData() );
    dynamicsWorld->addRigidBody( body->getRigidBody() );


    double currSimTime;
    double prevSimTime = viewer.getFrameStamp()->getSimulationTime();

    viewer.setSceneData( root.get() );
    viewer.realize();

    while( !viewer.done())
    {
        currSimTime = viewer.getFrameStamp()->getSimulationTime();
        dynamicsWorld->stepSimulation( currSimTime - prevSimTime );
        prevSimTime = currSimTime;
        viewer.frame();
    }

    return( 0 );
}
