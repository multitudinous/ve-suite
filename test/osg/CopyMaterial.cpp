//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osg/StateSet>
#include <osg/Material>
#include <osg/ShapeDrawable>
#include <osg/Geode>
#include <osgDB/ReadFile>
#include <osgViewer/Viewer>

osg::Node*
createScene()
{
    // Create a StateSet containing a red diffuse color.
    osg::StateSet* ssRed = new osg::StateSet;
    osg::Material* matRed = new osg::Material;
    matRed->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 1, 0, 0, 1 ) );
    matRed->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0, 0, 0, 1 ) );
    ssRed->setAttributeAndModes( matRed );


    osg::Vec3 center( -1, 0, 0 );
    const float width( 1.25f );

    // Create a box on the left, and assign it the "red" StateSet.
    osg::Box* box = new osg::Box( center, width );
    osg::ShapeDrawable* shape = new osg::ShapeDrawable( box );
    shape->setStateSet( ssRed );

    osg::Geode* geode = new osg::Geode;
    geode->addDrawable( shape );


    // Create a "green" StateSet by copying the red StateSet,
    // obtaining the Material attribute, and setting its
    // diffuse color to green.
#define USE_DEEP_COPY
#ifdef USE_DEEP_COPY
    // Deep copy creates two Materials, so one can be changed
    // without changing the other.
    osg::StateSet* ssGreen = new osg::StateSet( *ssRed, osg::CopyOp::DEEP_COPY_ALL );
#else
    // Shallow copy references the same material. Changing one to green
    // will also change the other, and both boxes render green.
    osg::StateSet* ssGreen = new osg::StateSet( *ssRed );
#endif
    osg::Material* matGreen = dynamic_cast< osg::Material* >(
        ssGreen->getAttribute( osg::StateAttribute::MATERIAL ) );
    matGreen->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0, 1, 0, 1 ) );

    center = osg::Vec3( 1, 0, 0 );

    // Assign the "green" StateSet to the box on the right.
    box = new osg::Box( center, width );
    shape = new osg::ShapeDrawable( box );
    shape->setStateSet( ssGreen );

    geode->addDrawable( shape );

    return geode;
}


int
main( int argc,
      char ** argv )
{
    osg::ref_ptr< osg::Node > root = createScene();

    osgViewer::Viewer viewer;
    viewer.setSceneData( root.get() );
    return( viewer.run() );
}

