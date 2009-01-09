//
// Copyright (c) 2008 Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osgDB/ReadFile>
#include <osgViewer/CompositeViewer>

#include <osg/Geometry>
#include <osg/DisplaySettings>
#include <osg/MatrixTransform>
#include <osg/LineWidth>
#include <osg/Point>
#include <osg/ShadeModel>
#include <osgGA/TrackballManipulator>
#include <osg/LightSource>
#include <osg/Light>
#include <osg/LightModel>


// Allows you to change the animation play rate:
//   '1' Disable the LightSource node (uses light settings from osgViewer::Viewer).
//   '2' Toggle directional and positional (assumes LightSource is enabled).
//   '3' Toggle local viewer.
//   'r' Toggle LightSource reference frame between relative and absolute.
//   'd' Dump current state.
class LightHandler
    : public osgGA::GUIEventHandler
{
public:
    LightHandler( osg::LightSource* ls, osg::LightModel* lm )
      : _ls( ls ),
        _lm( lm )
    {}

    virtual bool handle( const osgGA::GUIEventAdapter & event_adaptor,
                         osgGA::GUIActionAdapter & action_adaptor )
    {
        bool handled = false;
        switch( event_adaptor.getEventType() )
        {
            case ( osgGA::GUIEventAdapter::KEYDOWN ):
            {
                int key = event_adaptor.getKey();
                switch( key )
                {
                    case '1':
                    {
                        if( _ls->getNodeMask() == 0x0 )
                        {
                            osg::notify( osg::ALWAYS ) << "Enabling the LightSource node." << std::endl;
                            _ls->setNodeMask( 0xffffffff );
                        }
                        else
                        {
                            osg::notify( osg::ALWAYS ) << "Disabling the LightSource node and using osgViewer light settings." << std::endl;
                            _ls->setNodeMask( 0x0 );
                        }

                        handled = true;
                    }
                    break;
                    case '2':
                    {
                        osg::Vec4 pos = _ls->getLight()->getPosition();
                        if( pos[3] == 0. )
                        {
                            osg::notify( osg::ALWAYS ) << "Setting a positional light." << std::endl;
                            pos[3] = 1.;
                        }
                        else
                        {
                            osg::notify( osg::ALWAYS ) << "Setting a directional light." << std::endl;
                            pos[3] = 0.;
                        }
                        _ls->getLight()->setPosition( pos );

                        handled = true;
                    }
                    break;
                    case '3':
                    {
                        if( _lm->getLocalViewer() )
                        {
                            osg::notify( osg::ALWAYS ) << "Turning local viewer OFF." << std::endl;
                            _lm->setLocalViewer( false );
                        }
                        else
                        {
                            osg::notify( osg::ALWAYS ) << "Turning local viewer ON." << std::endl;
                            _lm->setLocalViewer( true );
                        }

                        handled = true;
                    }
                    break;
                    case 'd':
                    {
                        bool lsEnabled = ( _ls->getNodeMask() != 0x0 );
                        std::string lightType( "POSITIONAL" );
                        if( _ls->getLight()->getPosition()[ 3 ] == 0. )
                            lightType = "DIRECTIONAL";
                        bool localViewer = _lm->getLocalViewer();
                        std::string refFrame( "ABSOLUTE_RF" );
                        if( _ls->getReferenceFrame() == osg::LightSource::RELATIVE_RF )
                            refFrame = "RELATIVE_RF";

                        osg::notify( osg::ALWAYS ) << "  '1' LightSource enabled? " << std::boolalpha << lsEnabled << std::endl;
                        osg::notify( osg::ALWAYS ) << "  '2' Light type: " << lightType << std::endl;
                        osg::notify( osg::ALWAYS ) << "  '3' Local viewer? " << std::boolalpha << localViewer << std::endl;
                        osg::notify( osg::ALWAYS ) << "  'r' Reference frame: " << refFrame << std::endl;

                        handled = true;
                    }
                    break;

                }
            }
        }
        return( handled );
    }

private:
    osg::ref_ptr< osg::LightSource > _ls;
    osg::ref_ptr< osg::LightModel > _lm;
};


osg::Node*
makeFrustumFromCamera( osg::Camera* camera )
{
    osg::Matrixd proj;
    osg::Matrixd mv;
    if (camera)
    {
        proj = camera->getProjectionMatrix();
        mv = camera->getViewMatrix();
    }
    else
    {
        proj.makePerspective( 30., 1., 1., 10. );
        // leave mv as identity
    }

    double near, far;
    double nLeft, nRight, nBottom, nTop;
    double fLeft, fRight, fBottom, fTop;

    near = proj(3,2) / (proj(2,2)-1.0);
    far = proj(3,2) / (1.0+proj(2,2));
    
    nLeft = near * (proj(2,0)-1.0) / proj(0,0);
    nRight = near * (1.0+proj(2,0)) / proj(0,0);
    nTop = near * (1.0+proj(2,1)) / proj(1,1);
    nBottom = near * (proj(2,1)-1.0) / proj(1,1);

    fLeft = far * (proj(2,0)-1.0) / proj(0,0);
    fRight = far * (1.0+proj(2,0)) / proj(0,0);
    fTop = far * (1.0+proj(2,1)) / proj(1,1);
    fBottom = far * (proj(2,1)-1.0) / proj(1,1);


    osg::Vec3Array* v = new osg::Vec3Array;
    v->resize( 10 );
    (*v)[0].set( 0., 0., 0. );
    (*v)[1].set( nLeft, nBottom, -near );
    (*v)[2].set( nRight, nBottom, -near );
    (*v)[3].set( nRight, nTop, -near );
    (*v)[4].set( nLeft, nTop, -near );
    (*v)[5].set( fLeft, fBottom, -far );
    (*v)[6].set( fRight, fBottom, -far );
    (*v)[7].set( fRight, fTop, -far );
    (*v)[8].set( fLeft, fTop, -far );
    (*v)[9].set( 0., 0., 0. );

    osg::Geometry* geom = new osg::Geometry;
    geom->setVertexArray( v );

    osg::Vec4Array* c = new osg::Vec4Array;
    c->resize( 10 );
    (*c)[0].set( .6, .6, .6, 1. );
    for (int ci=0; ci<9; ci++)
        (*c)[ci+1].set( 1., 1., 1., 1. );
    geom->setColorArray( c );
    geom->setColorBinding( osg::Geometry::BIND_PER_VERTEX );

    GLushort idxLines[16] = {
        1, 0, 2, 0, 3, 0, 4, 0,
        1, 5, 2, 6, 3, 7, 4, 8 };
    GLushort idxLoops0[4] = {
        1, 2, 3, 4 };
    GLushort idxLoops1[4] = {
        5, 6, 7, 8 };
    geom->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::POINTS, 9, 1 ) );
    geom->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINES, 16, idxLines ) );
    geom->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINE_LOOP, 4, idxLoops0 ) );
    geom->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINE_LOOP, 4, idxLoops1 ) );

    osg::Geode* geode = new osg::Geode;
    geode->addDrawable( geom );

    geode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    osg::LineWidth* lw = new osg::LineWidth( 2. );
    geode->getOrCreateStateSet()->setAttributeAndModes( lw, osg::StateAttribute::ON );
    osg::Point* pt = new osg::Point;
    pt->setSize( 4. );
    geode->getOrCreateStateSet()->setAttributeAndModes( pt, osg::StateAttribute::ON );
    osg::ShadeModel* sm = new osg::ShadeModel( osg::ShadeModel::FLAT );
    geode->getOrCreateStateSet()->setAttributeAndModes( sm, osg::StateAttribute::ON );


    osg::MatrixTransform* mt = new osg::MatrixTransform;
    osg::Matrixd mvi = osg::Matrixd::inverse( mv );
    mt->setMatrix( mvi );
    mt->addChild( geode );

    return mt;
}


int
main( int argc,
      char ** argv )
{
    osg::ArgumentParser arguments(&argc,argv);

    osg::ref_ptr< osg::Group > root = new osg::Group;

    root->addChild( makeFrustumFromCamera( NULL ) ); // Child 0
    root->addChild( makeFrustumFromCamera( NULL ) ); // Child 1
    root->addChild( makeFrustumFromCamera( NULL ) ); // Child 2

    osg::ref_ptr< osg::Node > model = osgDB::readNodeFiles( arguments );
    if( !model.valid() )
        model = osgDB::readNodeFile( "cow.osg" );

    const double d( 12. );
    osg::ref_ptr< osg::Group > sceneGroup = new osg::Group;
    osg::ref_ptr< osg::MatrixTransform > mt = new osg::MatrixTransform( osg::Matrix::translate( d, 0., 0. ) );
    mt->addChild( model.get() );
    sceneGroup->addChild( mt.get() );
    mt = new osg::MatrixTransform( osg::Matrix::translate( -d, 0., 0. ) );
    mt->addChild( model.get() );
    sceneGroup->addChild( mt.get() );
    mt = new osg::MatrixTransform( osg::Matrix::translate( 0., d, 0. ) );
    mt->addChild( model.get() );
    sceneGroup->addChild( mt.get() );
    root->addChild( sceneGroup.get() ); // Child 3

    sceneGroup->getOrCreateStateSet()->setTextureMode( 0, GL_TEXTURE_2D, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
    osg::ref_ptr< osg::LightModel > lm = new osg::LightModel;
    lm->setLocalViewer( true ); // default is false
    sceneGroup->getOrCreateStateSet()->setAttributeAndModes( lm.get() );

        // Create a mostly blue light
        osg::ref_ptr<osg::Light> light = new osg::Light;
        light->setLightNum( 0 );
        light->setPosition( osg::Vec4( d, d, d, 0.f)); // Initially a directional light
        light->setDiffuse( osg::Vec4( .5f, .5f, 1.f, 1.f));
        light->setSpecular( osg::Vec4( .8f, .8f, 1.f, 1.f));

        osg::ref_ptr<osg::LightSource> ls = new osg::LightSource;
        sceneGroup->addChild( ls.get() );
        ls->setLight( light.get() );

    LightHandler* lh = new LightHandler( ls.get(), lm.get() );

    osgViewer::CompositeViewer viewer;
    osg::DisplaySettings::instance()->setNumMultiSamples( 4 );

    // view one -- Just the scene
    {
        osgViewer::View* view = new osgViewer::View;
        view->setSceneData( sceneGroup.get() );
        view->addEventHandler( lh );
        viewer.addView( view );

        const unsigned int width(350), height(350);
        osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
        tb->setHomePosition( osg::Vec3( 0,0,0 ), osg::Vec3( 0,1,0 ), osg::Vec3( 0,0,1 ) );
        view->setCameraManipulator( tb );
        const double aspect( (double)width / (double)height );
        view->getCamera()->setProjectionMatrixAsPerspective( 90./aspect, aspect, 3., 30. );

        double rotateAngle = -osg::PI_2;
        for(int i=0; i<3; ++i)
        {
            osg::ref_ptr<osg::GraphicsContext::Traits> traits = new osg::GraphicsContext::Traits;
            traits->x = width*i;
            traits->y = 250;
            traits->width = width;
            traits->height = height;
            traits->alpha = traits->stencil = 0;
            traits->windowDecoration = true;
            traits->doubleBuffer = true;
            traits->sharedContext = 0;
            traits->sampleBuffers = 1;
            traits->samples = 4;

            osg::ref_ptr<osg::GraphicsContext> gc = osg::GraphicsContext::createGraphicsContext(traits.get());

            osg::ref_ptr<osg::Camera> camera = new osg::Camera;
            camera->setGraphicsContext(gc.get());

            osgViewer::GraphicsWindow* gw = dynamic_cast<osgViewer::GraphicsWindow*>(gc.get());
            if (!gw)
                return 1;
            gw->getEventQueue()->getCurrentEventState()->setWindowRectangle(traits->x, traits->y, traits->width, traits->height );

            camera->setViewport(new osg::Viewport(0, 0, traits->width, traits->height));

            GLenum buffer = traits->doubleBuffer ? GL_BACK : GL_FRONT;
            camera->setDrawBuffer(buffer);
            camera->setReadBuffer(buffer);

            view->addSlave(camera.get(), osg::Matrixd(), osg::Matrixd::rotate( rotateAngle, 0., 1., 0. ) );
            rotateAngle += osg::PI_2;
        }
    }
    
    // view two - scene and frustum
    {
        osgViewer::View* view = new osgViewer::View;
        viewer.addView( view );

        view->setCameraManipulator( new osgGA::TrackballManipulator );
        view->setUpViewInWindow( 0, 600, 640, 480 );
        view->setSceneData( root.get() );
    }

    while (!viewer.done())
    {
        root->removeChild( 0, 3 );
        root->insertChild( 0,
            makeFrustumFromCamera( viewer.getView( 0 )->getSlave( 0 )._camera.get() ) );
        root->insertChild( 0,
            makeFrustumFromCamera( viewer.getView( 0 )->getSlave( 1 )._camera.get() ) );
        root->insertChild( 0,
            makeFrustumFromCamera( viewer.getView( 0 )->getSlave( 2 )._camera.get() ) );
        viewer.frame();
    }
    return 0;
}

