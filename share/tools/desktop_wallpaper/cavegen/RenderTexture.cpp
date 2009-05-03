//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#include "RenderTexture.h"

#include <osgDB/ReadFile>
#include <osgViewer/Viewer>
#include <osgGA/TrackballManipulator>

#include <osg/Node>
#include <osg/Camera>
#include <osg/MatrixTransform>
#include <osg/LightSource>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Depth>
#include <osg/Texture2D>

#include <string>


RenderTexture::RenderTexture()
  : _clearColor( osg::Vec4( .2, .2, .4, 1. ) ),
    _fov( 50.f ),
    _maxWidth( 2048 ),
    _maxHeight( 2048 )
{
}

bool RenderTexture::init( int argc, char** argv )
{
    // Create a scene graph hierarchy as follows:
    // osgViewer::Viewer contains this top level camera:
    //   Camera -- will render only a single screen oriented textured quad for the final image.
    //     _root (Group) -- a parent of a fullscreen quad and a RTT camera.
    //       Geode -- fullscreen quad, displays the final image.
    //       _rttCamera -- set as a prerender camera, to render the scene into a FBO/texture
    //         _parent (Group) -- Parent of all data to be rendered into the scene.
    //           Loaded model or default scene

    _viewer = new osgViewer::Viewer;
    _viewer->setUpViewOnSingleScreen( 0 );

    // Create and configure the texture.
    _colorMap = new osg::Texture2D();
    configureTexture( _colorMap.get() );

    // The RTT Camera will render the scene to the rextures above.
    _rttCamera = new osg::Camera;
    configureCameras( getViewerCamera(), getRTTCamera() );

    // Create the scene parent. The application can add and remove scene data to/from this node.
    _parent = new osg::Group;
    _parent->setDataVariance( osg::Object::DYNAMIC );
    _parent->getOrCreateStateSet()->setDataVariance( osg::Object::DYNAMIC );
    _rttCamera->addChild( _parent.get() );

    {
        // Render the quad in eye space. Use an ABSOLUTE_RF MatrixTransform and leave its matrix as identity.
        osg::MatrixTransform* eyeSpace = new osg::MatrixTransform;
        eyeSpace->setReferenceFrame( osg::Transform::ABSOLUTE_RF );

        // Here's how we handle lighting. To mimic the osgViewer's built-in headlight, we
        // position GL_LIGHT0 in eye space at the viewer location. Since this is an infinite
        // light, the position vector is a (eye space) direction; in this case +Z.
        osg::ref_ptr<osg::Light> light = new osg::Light;
        light->setLightNum( 0 );
        light->setDiffuse( osg::Vec4( 1., 1., 1., 1. ) );
        light->setSpecular( osg::Vec4( 1., 1., 1., 1. ) );
        light->setPosition( osg::Vec4( 0., 0., 1., 0. ) );

        osg::ref_ptr<osg::LightSource> ls = new osg::LightSource;
        ls->setLight( light.get() );
        eyeSpace->addChild( ls.get() );
        _parent->addChild( eyeSpace );
    }

    osg::ArgumentParser arguments( &argc, argv );
    osg::ref_ptr< osg::Node > model;
    model = osgDB::readNodeFiles( arguments );
    if( model.get() == NULL )
    {
        osg::notify( osg::WARN ) << "Cannot load data files, or files not specified. Trying the cow..." << std::endl;
        model = osgDB::readNodeFile( "cow.osg" );
        if( model.get() == NULL )
        {
            osg::notify( osg::WARN ) << "Cannot load any data file." << std::endl;
            return( false );
        }
    }

    _parent->addChild( model.get() );

    // _root is parent of both the RTT camera and the display stage.
    _root = new osg::Group();
    _viewer->setSceneData( _root.get() );
    _root->addChild( _rttCamera.get() );

    // Create the display stage. We'll configure it later, after the viewer has been realized.
    _displayStage = new osg::Group;
    _displayStage->setDataVariance( osg::Object::DYNAMIC );
    _displayStage->addChild( createFullScreenTexturedQuad() );
    _root->addChild( _displayStage.get() );


    // To manipulate the scene beneath the RTT Camera, attach the TB manipulator
    // as a viewer event handler. In the update() function, called once per frame,
    // set the RTT Camera with the manipulator's inverse matrix.
    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
    setManipulator( tb );
    tb->home( 0. );

    // We need to look for changes in the Viewport so we can reconfigure both the
    // viewer camera's and the RTT camera's projection matrices accordingly.
    // Init the viewer, get its viewport, and set the initial projection matrices
    // so that the first frame is correct.
    _viewer->realize();
    _lastViewport = new osg::Viewport( *( getViewerCamera()->getViewport() ) );
    setProjectionMatrices( getViewerCamera(), getRTTCamera(), getViewerCamera()->getViewport() );

    // Now that the viewer is realized and we have a viewport, configure the display stage.
    // It needs the viewport to calculate xy scale uniforms, used by the shader to determine
    //   how much of the 2k by 2k texture is in use.
    configureDisplayStage();

    return( true );
}

// Call this function to set your own display stage. By default (or if you pass in
//   NULL), the display stage is a single quad oriented to face the screen.
// State is already set on the parent node (_displayStage) to disable lighting and
//   enable texture mapping, with the texture in unit 0.
void RenderTexture::setDisplayStage( osg::Node* node )
{
    unsigned int n( _displayStage->getNumChildren() );
    if( n > 0 )
        _displayStage->removeChildren( 0, n );

    if( node == NULL )
        _displayStage->addChild( createFullScreenTexturedQuad() );
    else
        _displayStage->addChild( node );
}


// This method must be called once per frame. It is responsible for modifying the RTT
// camera position based on the TB manipulator, and also checking for (and handling)
// changes to the window size / viewport. Note that we could probably catch window
// resize / viewport changes with an event handler, which might actually be more
// efficient than doing a viewport compare. Either solution is acceptable.
void RenderTexture::update()
{
    // Set the RTT camera view from the manipulator.
    _rttCamera->setViewMatrix( _manipulator->getInverseMatrix() );

    // Update the projection matrices if the viewport changed.
    osg::Viewport* currentViewport = getViewerCamera()->getViewport();
    if (*_lastViewport != *currentViewport)
    {
        setProjectionMatrices( getViewerCamera(), getRTTCamera(), currentViewport );
        _lastViewport = new osg::Viewport( *currentViewport );
    }
}

// This is the clear color of the scene.
// If you want to change the default, call this before init().
void RenderTexture::setClearColor( const osg::Vec4& c )
{
    _clearColor = c;
}

// Changes the manipulator used to control the orientation of the RTT Camera.
void RenderTexture::setManipulator( osgGA::MatrixManipulator* manip )
{
    osg::ref_ptr< osgGA::MatrixManipulator > current( _manipulator );
    _manipulator = manip;
    _manipulator->setNode( _parent.get() );
    if( current.valid() )
        _manipulator->setByMatrix( current->getMatrix() );
    
    osg::ref_ptr< osgGA::GUIEventAdapter > ea = new osgGA::GUIEventAdapter;
    _manipulator->init( *ea, *_viewer );

    if( current.valid() )
    {
#if( (OSG_MAJOR_VERSION >= 2) && (OSG_MINOR_VERSION >= 9 ) )
        _viewer->removeEventHandler( current.get() );
#else
        osgViewer::View::EventHandlers& eh = _viewer->getEventHandlers();
        osgViewer::View::EventHandlers::iterator itr = std::find( eh.begin(), eh.end(), current.get() );
        if( itr != eh.end() )
            eh.erase( itr );
#endif
    }
    _viewer->addEventHandler( _manipulator.get() );
}

// Field of view, default is 50.
// If you want to change the default, call this before init().
void RenderTexture::setFOV( float fov )
{
    _fov = fov;
}

// Convenience accessor in case app needs to get to the viewer.
osgViewer::Viewer* RenderTexture::getViewer()
{
    return _viewer.get();
}

// Convenience accessor and equivalent to getViewer()->getCamera().
osg::Camera* RenderTexture::getViewerCamera()
{
    return _viewer->getCamera();
}

// Apps probably don't need access to this, but who knows.
osg::Camera* RenderTexture::getRTTCamera()
{
    return _rttCamera.get();
}

// Apps call this to add and remove child nodes for rendering. Note that
// the clear quad is a child of this node, so don't make assumptions
// about child index numbers (just because you add a child doesn't mean
// that child is child 0).
osg::Group* RenderTexture::getSceneParent()
{
    return _parent.get();
}

// Configure the two cameras for rendering.
// Note that this code doesn't set the projection matrices. This is done in
// setProjectionMatrices, which must be called once before the first frame,
// and once again if the window is resized (viewport change).
void RenderTexture::configureCameras( osg::Camera* topCamera, osg::Camera* rttCamera )
{
    // The view matrix for the viewer camera is the identity. When we render
    // the final image as a textured quad, the quad geometry is in eye space.
    topCamera->setViewMatrix( osg::Matrix::identity() );

    // We should never see this green clear color.
    topCamera->setClearColor( osg::Vec4( 0., 1., 0., 1. ) );
    // In fact, we could disable clearing altogether, as the entire window should
    // be filled with the textured quad; don't need the depth buffer
    // at this point either. But, leave the clear on for debugging purposes (if
    // you see bright green, you know something is wrong! :-). Plus this also
    // shows the issue that we don't handle windows bigger than 1024x1024 (or
    // _maxWidth x _maxHeight). Try it -- you'll see green.


    // RTT Camera config. Set this up as a pre-render, to render the scene
    // first (so the textures have something in them when they are mapped to the
    // fullscreen quad).
    rttCamera->setRenderOrder( osg::Camera::PRE_RENDER );
    // We are absolute_rf because we don't depend on any parent transforms.
    rttCamera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
    // Initially identity, but update() should set this based on the TB manipulator.
    rttCamera->setViewMatrix( osg::Matrix::identity() );
    // Like the above comment block, we don't need to clear color, but let's leave it enabled as a debugging aid.
    rttCamera->setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    // Render to FBOs.
    rttCamera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );


    rttCamera->setClearColor( _clearColor );

    rttCamera->attach( osg::Camera::COLOR_BUFFER, _colorMap.get() );//, 0, 0, false, 4, 4 );
    // We seem to get depth and stencil by default. I'm not aware this is in the OpenGL spec,
    // so I fear it might be implementation-dependent.
}

// Configure the textures that the RTT camera will render into.
void RenderTexture::configureTexture( osg::Texture2D* colorMap )
{
    colorMap->setInternalFormat( GL_RGBA );
    colorMap->setTextureSize( _maxWidth, _maxHeight );
    colorMap->setSourceFormat( GL_RGBA );
    colorMap->setSourceType( GL_UNSIGNED_BYTE );
    colorMap->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    colorMap->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    colorMap->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    colorMap->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
}

// Set the camera's projection matrices. Must be called once before the first frame (when the
// window and viewport are known), and again if the viewport changes.
void RenderTexture::setProjectionMatrices( osg::Camera* topCamera, osg::Camera* rttCamera, const osg::Viewport* vp )
{
    // Configure the top-level viewer camera, which merely renders a screen-oriented quad with a texture
    // (to display the final image). The modelview martix for this camera is always identity, so we're always
    // rendering in eye space. Set the projection matrix to control the amount of eye space mapped into clip
    // space. In other workd, configure left, right, bottom, and top so that the view volume only contains
    // the portion of the quad that shows the region of the texture that was rendered into during the RTT pass.
    const float x = vp->x();
    const float y = vp->y();
    const float width = vp->width();
    const float height = vp->height();
    float left = x;
    float right = x+width;
    float bottom = y;
    float top = y+height;
    topCamera->setProjectionMatrixAsOrtho( left, right, bottom, top, -1, 1 );

    // Set RTT camera projection to account for aspect ratio. Near/far are ignored when
    // the camera is configured with default auto compute.
    rttCamera->setProjectionMatrixAsPerspective( _fov, width/height, .1, 100. );
}


// Create the quad used to display the final image
osg::Geode* RenderTexture::createFullScreenTexturedQuad()
{
    // The geometry is eye space (the viewer's camera has an identity modelview).
    // If the window is sized to _maxWidth x _maxHeight, then the entire -1 to 1
    // quad will be visible. When the window is smaller, the viewer camera's projection matrix is
    // modified (see setProjectionMatrices) to display the lower left corner of the quad.
    double xMin = 0;
    double xMax = _maxWidth;
    double yMin = 0;
    double yMax = _maxHeight;
    double zVal = 0;
    
    osg::Geode* quadGeode = new osg::Geode();
    osg::ref_ptr< osg::Geometry > quadGeometry = new osg::Geometry();

    osg::ref_ptr< osg::Vec3Array > quadVertices = new osg::Vec3Array();
    quadVertices->resize( 4 );
    (*quadVertices)[ 0 ].set( xMin, yMin, zVal );
    (*quadVertices)[ 1 ].set( xMax, yMin, zVal );
    (*quadVertices)[ 2 ].set( xMax, yMax, zVal );
    (*quadVertices)[ 3 ].set( xMin, yMax, zVal );
    quadGeometry->setVertexArray( quadVertices.get() );
    
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();
    quadTexCoords->resize( 4 );
    (*quadTexCoords)[ 0 ].set( 0, 0 );
    (*quadTexCoords)[ 1 ].set( 1, 0 );
    (*quadTexCoords)[ 2 ].set( 1, 1 );
    (*quadTexCoords)[ 3 ].set( 0, 1 );
    quadGeometry->setTexCoordArray( 0, quadTexCoords.get() );
    
    quadGeometry->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, 4 ) );
    
    quadGeode->addDrawable( quadGeometry.get() );
    quadGeode->setCullingActive( false );

    return quadGeode;
}

// Set up the display stage parent to be a Group with lighting
// disabled and texture mapping enabled (and in unit 0).
// Must be called after viewer.realize().
void RenderTexture::configureDisplayStage()
{
    osg::StateSet* stateset = _displayStage->getOrCreateStateSet();
    // Don't light this subgraph.
    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    // Store texture in unit 0, and set up state so that either fixed function
    //   or shader can access the texture.
    stateset->setTextureAttributeAndModes(
        0, _colorMap.get(), osg::StateAttribute::ON );
    osg::ref_ptr< osg::Uniform > colorMapUniform =
        new osg::Uniform( "colorMap", 0 );
    stateset->addUniform( colorMapUniform.get() );

    // Useful variables for the shader.

    // The fragment shader used during capture needs to know how much of the 2k by 2k texture is in use.
    //   The actual amount used varies by screen size of the system running the app. So, get the viewport
    //   to compute these scale values.
    // configureDisplayStage() must be called after viewer.realize().
    const osg::Viewport* vp = getViewerCamera()->getViewport();
    osg::ref_ptr< osg::Uniform > xScaleUniform =
        new osg::Uniform( "xScale", (float)( vp->width() / _maxWidth ) );
    stateset->addUniform( xScaleUniform.get() );
    osg::ref_ptr< osg::Uniform > yScaleUniform =
        new osg::Uniform( "yScale", (float)( vp->height() / _maxHeight ) );
    stateset->addUniform( yScaleUniform.get() );
}
