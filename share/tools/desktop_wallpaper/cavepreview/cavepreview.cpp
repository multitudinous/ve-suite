//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osgDB/Registry>
#include <osgGA/TrackballManipulator>
#include <osgViewer/Viewer>
#include <osg/Camera>
#include <osg/Geode>

#include "../cavegen/Wall.h"

#include <vector>
#include <string>



int main( int argc, char** argv )
{
    osg::ArgumentParser arguments( &argc, argv );

    arguments.getApplicationUsage()->setApplicationName( arguments.getApplicationName() );
    arguments.getApplicationUsage()->setDescription( arguments.getApplicationName() + " previews desktop wallpaper for a 4-wall cave system." );
    arguments.getApplicationUsage()->setCommandLineUsage( arguments.getApplicationName() + " [options]" );
    arguments.getApplicationUsage()->addCommandLineOption( "--config <filename>", "Cave config file containing viewpoint and wall size/location information." );


    if( arguments.read( "--help" ) || arguments.read( "-h" ) || arguments.read( "-?" ) )
    {
        arguments.getApplicationUsage()->write( std::cout, osg::ApplicationUsage::COMMAND_LINE_OPTION );
        return( 1 );
    }

    std::string config( "cave.txt" );
    arguments.read( "--config", config );
    osg::notify( osg::ALWAYS ) << "--config: " << config << std::endl;


    osg::Vec3 viewPos;
    std::string prefix;
    WallList wallList = readWallFile( config, viewPos, prefix );
    if( wallList.empty() )
        return( 1 );


    osgViewer::Viewer viewer;
    viewer.setUpViewOnSingleScreen( 0 );
    viewer.getCamera()->setClearColor( osg::Vec4( 0., 0., .7, 1. ) );
    viewer.realize();

    osgGA::TrackballManipulator* tm = new osgGA::TrackballManipulator;
    tm->setHomePosition( viewPos, osg::Vec3( 0., 0., 1. ), osg::Vec3( 0., 0., 1. ) );
    viewer.setCameraManipulator( tm );

    const osg::Viewport* vp = viewer.getCamera()->getViewport();
    float windowWidth = (float)( vp->width() );
    float windowHeight = (float)( vp->height() );

    // Projection matrices for the wall
    osg::Matrix wallProjection( osg::Matrix::perspective( 70., windowWidth/windowHeight, .1, 5000. ) );
    viewer.getCamera()->setProjectionMatrix( wallProjection );
    viewer.getCamera()->setComputeNearFarMode( osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR );

    // Add each wall geometry to the wallGroup.
    osg::ref_ptr< osg::Group > wallGroup = new osg::Group;

    // Modify OSG data file path so that it can find the textures. Could be
    // in different locations depending on cavegen current working directory.
    osgDB::Registry::instance()->getDataFilePathList().push_back( std::string( "../cavegen/" ) );
    osgDB::Registry::instance()->getDataFilePathList().push_back( std::string( "." ) );

    WallList::iterator wit;
    for( wit=wallList.begin(); wit!=wallList.end(); wit++ )
    {
        Wall& wall = *wit;
        wall.texName_ = prefix + wall.suffix_ + std::string( ".png" );
        wit->createGeode( wallProjection );
        wallGroup->addChild( wit->getGeode() );
    }
    viewer.setSceneData( wallGroup.get() );


    return( viewer.run() );
}
