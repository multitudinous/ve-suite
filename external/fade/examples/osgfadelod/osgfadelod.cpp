// C++ source file - Open Scene Graph Training - Copyright (C) 2004 Don Burns
// Distributed under the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE (LGPL)
// as published by the Free Software Foundation.

#include <osgDB/ReadFile>
#include <osgUtil/Optimizer>
#include <osgProducer/Viewer>
#include <osg/LOD>
#include <osg/Fade>
#include <osg/ShapeDrawable>

static osg::Node *createFadeLODScene()
{
    osg::LOD *lod = new osg::LOD;

    osg::Geode *geode = new osg::Geode;
    geode->addDrawable( new osg::ShapeDrawable( new osg::Sphere ));

    osg::Fade *fade = new osg::Fade;
    fade->setDataVariance( osg::Object::DYNAMIC );
    fade->addChild( geode );
    fade->addFadePoint(  0.0,  1.0 );
    fade->addFadePoint(  7.5, 1.0 );
    fade->addFadePoint( 12.5, 0.0 );

    lod->addChild( fade );
    lod->setRange( 0, 0.0, 12.5 );

    geode = new osg::Geode;
    geode->addDrawable( new osg::ShapeDrawable( new osg::Cylinder ));

    fade = new osg::Fade;
    fade->setDataVariance( osg::Object::DYNAMIC );
    fade->addChild( geode );
    fade->addFadePoint(  0.0, 0.0 );
    fade->addFadePoint(  7.0, 0.0 );
    fade->addFadePoint( 12.5, 1.0 );
    fade->addFadePoint( 17.5, 1.0 );
    fade->addFadePoint( 22.5, 0.0 );
    lod->addChild( fade );
    lod->setRange( 1, 7.5, 22.5 );

    geode = new osg::Geode;
    geode->addDrawable( new osg::ShapeDrawable( new osg::Cone ));
    fade = new osg::Fade;
    fade->setDataVariance( osg::Object::DYNAMIC );
    fade->addChild( geode );
    fade->addFadePoint( 0.0, 0.0 );
    fade->addFadePoint( 17.5, 0.0 );
    fade->addFadePoint( 22.5, 1.0 );
    fade->addFadePoint( 27.5, 1.0 );
    fade->addFadePoint( 32.5, 0.0 );
    lod->addChild( fade );
    lod->setRange( 2, 17.5, 32.5 );

    geode = new osg::Geode;
    geode->addDrawable( new osg::ShapeDrawable( new osg::Box ));
    fade = new osg::Fade;
    fade->setDataVariance( osg::Object::DYNAMIC );
    fade->addChild( geode );
    fade->addFadePoint(  0.0, 0. );
    fade->addFadePoint( 27.5, 0.0 );
    fade->addFadePoint( 32.5, 1.0 );
    lod->addChild( fade );
    lod->setRange( 3, 27.5, 1000.0 );

    return lod;

}

int main(int argc, char **argv)
{
    osg::ArgumentParser args( &argc, argv );

    osgProducer::Viewer viewer(args);
    viewer.setUpViewer(osgProducer::Viewer::STANDARD_SETTINGS);

    osg::ref_ptr<osg::Node> loadedModel = createFadeLODScene();
    if( !loadedModel.valid() )
    {
        std::cerr << argv[0] << ": No data loaded.  Exiting." << std::endl;
        return 1;
    }

    viewer.getUsage(*args.getApplicationUsage());

    osgUtil::Optimizer optimizer;
    optimizer.optimize(loadedModel.get());

    viewer.setSceneData(loadedModel.get());

    viewer.realize();

    while( !viewer.done() )
    {
        viewer.sync();
        viewer.update();
        viewer.frame();
    }
    viewer.sync();
    return 0;
}
