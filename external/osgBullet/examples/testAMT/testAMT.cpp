// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix  Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgViewer/Viewer>
#include <osg/Group>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/Geometry>

#include <osgBullet/AbsoluteModelTransform.h>

#include <string>
#include <osg/io_utils>


osg::Node*
makeScene()
{
    osg::Group* root = new osg::Group;

    osg::Geode* geode = new osg::Geode;
    geode->addDrawable(
        osg::createTexturedQuadGeometry( osg::Vec3( -2., 0., -2. ),
            osg::Vec3( 4., 0., 0. ), osg::Vec3( 0., 0., 4. ) ) );

    osgBullet::AbsoluteModelTransform* amt = new
        osgBullet::AbsoluteModelTransform( osg::Matrix::translate( osg::Vec3( 0, 0, -6 ) ) );
    //amt->setReferenceFrame( osg::Transform::RELATIVE_RF );
    amt->addChild( geode );

    osg::Matrix m(
        osg::Matrix::translate( osg::Vec3( 5, -3, 0 ) )
        );
    osg::MatrixTransform* mt = new osg::MatrixTransform( m );
    mt->addChild( amt );
    root->addChild( mt );

    return( (osg::Node*) root );
}


int
main( int argc,
      char ** argv )
{
    osg::ref_ptr< osg::Group > root = new osg::Group;
    root->addChild( makeScene() );
    root->addChild( osgDB::readNodeFile( "cow.osg" ) );


    osgViewer::Viewer viewer;
    viewer.setUpViewOnSingleScreen( 0 );
    viewer.setSceneData( root );

    return( viewer.run() );
}
