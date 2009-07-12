
#include <osg/Node>
#include <osg/Camera>
#include <osg/MatrixTransform>
#include <osg/LightSource>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/Stencil>
#include <osg/Depth>
#include <osg/CullFace>
#include <osg/Texture2D>
#include <osgDB/ReadFile>
#include <osgGA/TrackballManipulator>
#include <osgViewer/Viewer>

#include <string>



// This class manages an osgViewer::Viewer and configures it for use with RTT.
// It renders into 2 render targets, using one as a glow map, and applies the
// glow effect as it renders the final image.

class RTTGlow
{
public:
    RTTGlow();
    ~RTTGlow() {}

    // Change clear color, glow color, fov, and minZNear before calling this.
    void init( int argc, char** argv );

    // Call this once per frame.
    void update();

    // If you want to change the defaults, call these before init().
    void setClearColor( const osg::Vec4& c );
    void setGlowColor( const osg::Vec4& c );
    void setFOV( float fov );
    void setMinZNear( float z );

    // ACCESSORS
    //   Get the Viewer managed by this class.
    osgViewer::Viewer* getViewer();
    //   Equivalent to getViewer()->getCamera().
    osg::Camera* getViewerCamera();
    //   Gets the RTT Camera, the camera that renders the main scene.
    osg::Camera* getRTTCamera();
    //   Returns the parent node; add or subtract any subgraphs for rendering to/from this node.
    osg::Group* getSceneParent();

protected:
    void configureCameras( osg::Camera* topCamera, osg::Camera* rttCamera );
    void configureTextures( osg::Texture2D* colorMap, osg::Texture2D* glowMap );
    void setProjectionMatrices( osg::Camera* topCamera, osg::Camera* rttCamera, const osg::Viewport* vp );
    osg::Node* createClearQuad( const osg::Vec4& clearColor, float fov, float minZNear );
    osg::Geode* createFullScreenTexturedQuad( osg::Texture2D* colorTexture, osg::Texture2D* glowTexture );

    osg::Node* createDefaultScene( osg::Vec4 glowColor );

    osg::ref_ptr< osgViewer::Viewer > _viewer;
    osg::ref_ptr< osg::Camera > _rttCamera;
    osg::ref_ptr< osg::Group > _parent;

    osg::ref_ptr< osgGA::TrackballManipulator > _manipulator;
    osg::ref_ptr< osg::Viewport > _lastViewport;

    osg::ref_ptr< osg::Texture2D > _colorMap;
    osg::ref_ptr< osg::Texture2D > _glowMap;

    osg::Vec4 _clearColor;
    osg::Vec4 _glowColor;
    float _fov;
    float _minZNear;

    // This is the max dimensions of the RTT surface. Expanding the window size
    // beyond these dimensions will show some of the viewer camera's clear color.
    // This is bad and is a limitation of this code in its current form.
    float _maxWidth;
    float _maxHeight;
};

RTTGlow::RTTGlow()
  : _clearColor( osg::Vec4( .2, .2, .4, 1. ) ),
    _glowColor( osg::Vec4( .3, .5, .6, 1. ) ),
    _fov( 50.f ),
    _minZNear( 1.f ),
    _maxWidth( 2048 ),
    _maxHeight( 2048 )
{
}

void RTTGlow::init( int argc, char** argv )
{
    // Create a scene graph hierarchy as follows:
    // osgViewer::Viewer contains this top level camera:
    //   Camera -- will render only a single screen oriented textured quad for the final image.
    //     root (Group) -- a parent of a fullscreen quad and a RTT camera.
    //       Geode -- fullscreen quad, displays the final image.
    //       _rttCamera -- set as a prerender camera, to render the scene into the textures
    //         _parent (Group) -- Parent of all data to be rendered into the scene.
    //           Clear Quad (Geode) -- This quad actually clears the buffers (see createClearQuad for an explanation)
    //           Loaded model or default scene

    // Create the viewer. Start in a 512x512 window.
    _viewer = new osgViewer::Viewer;
    _viewer->setUpViewInWindow( 200, 200, 512, 512 );

    // Create and configure the MRTs, two textures. One is for the normal rendering,
    // the other contains the glow color for glowing objects (or black).
    _colorMap = new osg::Texture2D();
    _glowMap = new osg::Texture2D();
    configureTextures( _colorMap.get(), _glowMap.get() );

    // The RTT Camera will render the scene to the rextures above.
    _rttCamera = new osg::Camera;
    configureCameras( getViewerCamera(), getRTTCamera() );

    // Create the scene parent. The application can add and remove scene data to/from this node.
    {
        _parent = new osg::Group;
        osg::StateSet* stateSet = _parent->getOrCreateStateSet();

        // All childen will write into two color buffers. Buffer 0
        // is the normal incoming color, and buffer 1 is the glow
        // color. A child must explicitly set a glowColor uniform to
        // get a glow effect (it's zero by default).
        // ALL CHILDREN OF _parent MUST USE THIS FRAGMENT SHADER:
        std::string fragmentSource =
            "uniform vec4 glowColor; \n"
            " \n"
            "void main() \n"
            "{ \n"
            "    gl_FragData[0] = gl_Color; \n"
            "    gl_FragData[1] = glowColor; \n"
            "} \n";

        osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
        fragmentShader->setType( osg::Shader::FRAGMENT );
        fragmentShader->setShaderSource( fragmentSource );

        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( fragmentShader.get() );

        stateSet->setAttributeAndModes( program.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        // Default glow color for aNY children that don't explicitly set it.
        stateSet->addUniform( new osg::Uniform( "glowColor", osg::Vec4( 0., 0., 0., 1. ) ) );
    }
    _rttCamera->addChild( _parent.get() );

    // Must be present for proper clearing of the MRTs.
    _parent->addChild( createClearQuad( _clearColor, _fov, _minZNear ) );
    if( argc == 1 )
    {
        // No command line data, just use default scene.
        _parent->addChild( createDefaultScene( _glowColor ) );
    }
    else
    {
        // Load a model and tag it as glowing with the proper Uniform.
        osg::Node* model = osgDB::readNodeFile( std::string( argv[ 1 ] ) );
        model->getOrCreateStateSet()->addUniform(
            new osg::Uniform( "glowColor", _glowColor ) );
        _parent->addChild( model );
    }

    // Make a Group to contain the children we will add to our Viewer.
    osg::ref_ptr< osg::Group > root = new osg::Group();
    root->addChild( _rttCamera.get() );
    root->addChild( createFullScreenTexturedQuad( _colorMap.get(), _glowMap.get() ) );
    _viewer->setSceneData( root.get() );

    // To manipulate the scene beneath the RTT Camera, attach the TB manipulator
    // as a viewer event handler. In the update() function, called once per frame,
    // set the RTT Camera with the manipulator's inverse matrix.
    _manipulator = new osgGA::TrackballManipulator;
    _viewer->addEventHandler( _manipulator.get() );

    // We want the home position (space bar) to be based on the elements of our
    // scene -- just the stuff under _parent.
    _manipulator->setNode( _parent.get() );
    _manipulator->home( 0. );

    // We need to look for changes in the Viewport so we can reconfigure both the
    // viewer camera's and the RTT camera's projection matrices accordingly.
    // Init the viewer, get its viewport, and set the initial projection matrices
    // so that the first frame is correct.
    _viewer->realize();
    _lastViewport = new osg::Viewport( *( getViewerCamera()->getViewport() ) );
    setProjectionMatrices( getViewerCamera(), getRTTCamera(), getViewerCamera()->getViewport() );
}

// This method must be called once per frame. It is responsible for modifying the RTT
// camera position based on the TB manipulator, and also checking for (and handling)
// changes to the window size / viewport. Note that we could probably catch window
// resize / viewport changes with an event handler, which might actually be more
// efficient than doing a viewport compare. Either solution is acceptable.
void RTTGlow::update()
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

// This is the clear color of the scene. Note that clearing for MRTs is
// done with a fullscreen quad; see createClearQuad for an explanation.
// If you want to change the default, call this before init().
void RTTGlow::setClearColor( const osg::Vec4& c )
{
    _clearColor = c;
}

// Sets the glowColor vec4 Uniform. This is the color written into
// the glow map for glowing objects.
// If you want to change the default, call this before init().
void RTTGlow::setGlowColor( const osg::Vec4& c )
{
    _glowColor = c;
}

// Field of view, default is 50.
// If you want to change the default, call this before init().
void RTTGlow::setFOV( float fov )
{
    _fov = fov;
}

// Specifies the eye space distance to the clear quad (see createClearQuad).
// The default is 1. You probably don't want to make it smaller, but you might
// want to make it larger to increase Z buffer precision. The reason? By default,
// cameras auto compute the near/far planes. The clear quad is a child of the RTT
// camera, and therefore its location affects the computed near/far. The closer
// it is to the eye, the worse the Z precision.
// If you want to change the default, call this before init().
void RTTGlow::setMinZNear( float z )
{
    _minZNear = z;
}

// Convenience accessor in case app needs to get to the viewer.
osgViewer::Viewer* RTTGlow::getViewer()
{
    return _viewer.get();
}

// Convenience accessor and equivalent to getViewer()->getCamera().
osg::Camera* RTTGlow::getViewerCamera()
{
    return _viewer->getCamera();
}

// Apps probably don't need access to this, but who knows.
osg::Camera* RTTGlow::getRTTCamera()
{
    return _rttCamera.get();
}

// Apps call this to add and remove child nodes for rendering. Note that
// the clear quad is a child of this node, so don't make assumptions
// about child index numbers (just because you add a child doesn't mean
// that child is child 0).
osg::Group* RTTGlow::getSceneParent()
{
    return _parent.get();
}

// Configure the two cameras for rendering.
// Note that this code doesn't set the projection matrices. This is done in
// setProjectionMatrices, which must be called once before the first frame,
// and once again if the window is resized (viewport change).
void RTTGlow::configureCameras( osg::Camera* topCamera, osg::Camera* rttCamera )
{
    // The view matrix for the viewer camera is the identity. When we render
    // the final image as a textured quad, the quad geometry is in eye space.
    topCamera->setViewMatrix( osg::Matrix::identity() );

    // We should never see this green clear color.
    topCamera->setClearColor( osg::Vec4( 0., 1., 0., 1. ) );
    // In fact, we could disable clearing altogether, as the entire window should
    // be filled with the textured quad; don't need the depth or stencil buffers
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
    rttCamera->setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );
    // Render to FBOs.
    rttCamera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );

    // We should never see this red clear color.
    rttCamera->setClearColor( osg::Vec4( 1., 0., 0., 1. ) );
    // If we see red, something is wrong.
    // We don care about clearing the depth and stencil for the RTT pass, but don't need to set
    // the clear values because the defaults are correct.

    // Note that multisampling seems to disable MRTs on GeForce 6600 Windows drivers.
    // For now, do not use multisampled FBOs.
    rttCamera->attach( osg::Camera::COLOR_BUFFER0, _colorMap.get() );//, 0, 0, false, 4, 4 );
    rttCamera->attach( osg::Camera::COLOR_BUFFER1, _glowMap.get() );//, 0, 0, false, 4, 4 );
    // We seem to get depth and stencil by default. I'm not aware this is in the OpenGL spec,
    // so I fear it might be implementation-dependent.
}

// Configure the textures that the RTT camera will render into.
void RTTGlow::configureTextures( osg::Texture2D* colorMap, osg::Texture2D* glowMap )
{
    colorMap->setInternalFormat( GL_RGBA );
    colorMap->setTextureSize( _maxWidth, _maxHeight );
    colorMap->setSourceFormat( GL_RGBA );
    colorMap->setSourceType( GL_UNSIGNED_BYTE );
    colorMap->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    colorMap->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    colorMap->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    colorMap->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );

    glowMap->setInternalFormat( GL_RGBA );
    glowMap->setTextureSize( _maxWidth, _maxHeight );
    glowMap->setSourceFormat( GL_RGBA );
    glowMap->setSourceType( GL_UNSIGNED_BYTE );
    glowMap->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    glowMap->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    // We really want REPEAT otherwise the edge gets sampled by the glow shader.
    glowMap->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::REPEAT );
    glowMap->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::REPEAT );
}

// Set the camera's projection matrices. Must be called once before the first frame (when the
// window and viewport are known), and again if the viewport changes.
void RTTGlow::setProjectionMatrices( osg::Camera* topCamera, osg::Camera* rttCamera, const osg::Viewport* vp )
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

    // The RTT camera viewport needs to be twice the viewer camera viewport.
    // This means the RTT camera will render into 4 times the screen real estate of
    // the final image. The final rendered image contains 4 samples (2x2) from the
    // FBO textures combined into each pixel.
    rttCamera->setViewport( vp->x() * 2., vp->y() * 2., vp->width() * 2., vp->height() * 2. );
}

// During the RTT pass, clear by drawing a fullscreen quad. Why? As far as I know, there
// is no way to clear two color buffers (Multiple Render Targets or MRTs) to two different
// colors. However, we need _colorMap cleared to the _clearColor, and we need _glowMap
// cleared to black. To accomplish this, draw a fullscreen quad with color set to
// _clearColor and the "glowColor" Uniform set to black, then just let the fragment shader
// do its thing.
//
// Note there are issues with this approach; if fov is wide and minZNear is distant, parts
// of the FBOs will not be cleared. This is due to programmer laziness, and the dimensions of
// the clear quad could be computed to cover most any such combination, but the current implementation
// simply assumes reasonable values.
osg::Node* RTTGlow::createClearQuad( const osg::Vec4& clearColor, float fov, float minZNear )
{
    // Render the quad in eye space. Use an ABSOLUTE_RF MatrixTransform and leave its matrix as identity.
    osg::MatrixTransform* eyeSpace = new osg::MatrixTransform;
    eyeSpace->setReferenceFrame( osg::Transform::ABSOLUTE_RF );

    // Render first.
    eyeSpace->getOrCreateStateSet()->setRenderBinDetails( -9999, "RenderBin" );

    // Create the clear quad geometry.
    osg::Geode* geode = new osg::Geode;
    osg::Geometry*  geom = new osg::Geometry;

    osg::Vec3Array* v = new osg::Vec3Array();
    v->resize( 4 );
    const float dim( 1.f );
    const float z( -_minZNear );
    (*v)[ 0 ].set( -dim, -dim, z );
    (*v)[ 1 ].set( dim, -dim, z );
    (*v)[ 2 ].set( dim, dim, z );
    (*v)[ 3 ].set( -dim, dim, z );
    geom->setVertexArray( v );
    // We should use the input paramters fov and minZNear to dompute the xy dimensions
    // of this quad, but currently don't (the developer of this code is exhausted and
    // under the gun on other projects.) This is left as a trigonometry exercise for
    // the quality concious reader.
    
    osg::Vec4Array* c = new osg::Vec4Array();
    c->push_back( clearColor );
    geom->setColorArray( c );
    geom->setColorBinding( osg::Geometry::BIND_OVERALL );
    
    geom->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );
    
    geode->addDrawable( geom );
    geode->setCullingActive( false );


    osg::ref_ptr< osg::StateSet > stateset = geode->getOrCreateStateSet();

    // Don't write the depth value! The RTT camera has already cleared the depth buffer to
    // the desired value. We just want to paint the color buffers / FBOs.
    osg::Depth* depth = new osg::Depth( osg::Depth::LESS, 0., 1., false );
    stateset->setAttributeAndModes( depth );
    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    eyeSpace->addChild( geode );


    // Here's how we handle lighting. To mimic the osgViewer's built-in headlight, we
    // position GL_LIGHT0 in eye space at the viewer location. Since this is an infinite
    // light, the position vector is a (eye space) direction; in this case +Z.
    osg::ref_ptr<osg::Light> light = new osg::Light;
    light->setLightNum( 0 );
    light->setPosition( osg::Vec4( 0., 0., 1., 0. ) );

    osg::ref_ptr<osg::LightSource> ls = new osg::LightSource;
    ls->setLight( light.get() );
    eyeSpace->addChild( ls.get() );

    return eyeSpace;
}

// Create the quad used to display the final image by combining the two FBO textures and adding the glow.
osg::Geode* RTTGlow::createFullScreenTexturedQuad( osg::Texture2D* colorTexture, osg::Texture2D* glowTexture )
{
    // The geometry is eye space (the viewer's camera has an identity modelview).
    // If the window is sized to _maxWidth x _maxHeight, then the entire -1 to 1
    // quad will be visible. When the window is smaller, the viewer camera's projection matrix is
    // modified (see setProjectionMatrices) to display the lower left corner of the quad.
    double xMin = 0;
    double xMax = _maxWidth * .5;
    double yMin = 0;
    double yMax = _maxHeight * .5;
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


    // Vertex shader is unnecessary and can probably be removed.
    std::string vertexSource =
    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"
        "gl_TexCoord[ 0 ].st = gl_MultiTexCoord0.st; \n"
    "} \n";

    // The fragment shader takes multiple samples of the glowMap to create the glow effect.
    // There are probably better ways to do this. The point of this code is to show how to
    // set up cameras for RTT, not show a butt-kickin' glow effect...
    std::string fragmentSource =
    "uniform sampler2D colorMap; \n"
    "uniform sampler2D glowMap; \n"
    "void main() \n"
    "{ \n"
    "    float delta = .015; \n"
    "    vec4 glow = texture2D( glowMap, gl_TexCoord[ 0 ].st ); \n"
    "    vec2 offset = vec2( -delta, -delta ); \n"
    "    glow += texture2D( glowMap, gl_TexCoord[ 0 ].st + offset ); \n"
    "    offset.s = delta; \n"
    "    glow += texture2D( glowMap, gl_TexCoord[ 0 ].st + offset ); \n"
    "    offset.t = delta; \n"
    "    glow += texture2D( glowMap, gl_TexCoord[ 0 ].st + offset ); \n"
    "    offset.s = 0.-delta; \n"
    "    glow += texture2D( glowMap, gl_TexCoord[ 0 ].st + offset ); \n"
    "    glow /= 5.; \n"
    "\n"
    "    vec4 color = texture2D( colorMap, gl_TexCoord[ 0 ].st ); \n"
    "    gl_FragColor = color + glow; \n"
    "} \n";

    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    // Don't need to set a render bin number for ordering. This is already handled by camera nesting.
    stateset->setAttribute( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    // Don't light this quad.
    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    // Units 0 and 1 correspond to gl_FragData[ 0 or 1 ] in the fragment shader used during the RTT pass.
    stateset->setTextureAttributeAndModes(
        0, colorTexture, osg::StateAttribute::ON );
    stateset->setTextureAttributeAndModes(
        1, glowTexture, osg::StateAttribute::ON );


    // Set up sampler Uniforms. Again, note texture units.
    osg::ref_ptr< osg::Uniform > colorMapUniform =
        new osg::Uniform( "colorMap", 0 );
    stateset->addUniform( colorMapUniform.get() );
    osg::ref_ptr< osg::Uniform > glowMapUniform =
        new osg::Uniform( "glowMap", 1 );
    stateset->addUniform( glowMapUniform.get() );

    quadGeode->setStateSet( stateset.get() );

    return quadGeode;
}

// The default scene renders two boxes.
//
// The one on the left doesn't glow,
// but it does write into the stencil buffer, and the code below applies a
// blue decal quad on the side as proof that stencil is present and works.
// If you see z fighting on the blue quad, you are not getting a stencil 
// buffer in the RTT pass. Likewise, to render the image correctly, there
// must be a depth buffer present.
// 
// The box on the right uses the "glowColor" Uniform to write non-black into
// the glow map; the fullscreen quad used in the final rendering oversamples
// those values to create the cheesy glow effect.
osg::Node* RTTGlow::createDefaultScene( osg::Vec4 glowColor )
{
    osg::Group* grp = new osg::Group;

    osg::Box* box = new osg::Box( osg::Vec3( 0., 0., 0. ), 1. );
    osg::ShapeDrawable* shape = new osg::ShapeDrawable;
    shape->setColor( osg::Vec4( 0.75, 0.75, 0.75, 1. ) );
    shape->setShape( box );
    osg::Geode* boxGeode = new osg::Geode;
    boxGeode->addDrawable( shape );

    // Draw the quad on the left.
    {
        // Write 1 into the stencil buffer.
        osg::ref_ptr< osg::Stencil > stencil = new osg::Stencil();
        stencil->setFunction( osg::Stencil::ALWAYS, 1, ~0u );
        stencil->setOperation( osg::Stencil::KEEP, osg::Stencil::KEEP, osg::Stencil::REPLACE );

        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        // Note render bin is 1 for relative ordering. Must precede decal in bin 2.
        stateset->setRenderBinDetails( 1, "RenderBin" );
        stateset->setAttributeAndModes( stencil.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        osg::MatrixTransform* mt = new osg::MatrixTransform( osg::Matrix::translate( osg::Vec3( -.5, 0., 0. ) ) );
        mt->addChild( boxGeode );
        mt->setStateSet( stateset.get() );
        grp->addChild( mt );
    }

    // Apply the blue decal.
    {
        // Only write where stencil is 1.
        osg::ref_ptr< osg::Stencil > stencil = new osg::Stencil();
        stencil->setFunction( osg::Stencil::EQUAL, 1, ~0u );
        stencil->setOperation( osg::Stencil::KEEP, osg::Stencil::KEEP, osg::Stencil::KEEP );

        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        // Note bin 2 for relative ordering; must follow the box in bin 1.
        stateset->setRenderBinDetails( 2, "RenderBin" );

        // Cull when backfacing, otherwise this decal will draw (depth is off and stencil is 1).
        osg::CullFace* cf = new osg::CullFace;
        // Disable depth to avoid z fighting from coplanarity.
        osg::Depth* depth = new osg::Depth( osg::Depth::ALWAYS );

        stateset->setAttributeAndModes( stencil.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateset->setAttributeAndModes( cf,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateset->setAttributeAndModes( depth,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        osg::Geometry* decal = createTexturedQuadGeometry(
            osg::Vec3( .25, -.5, -.25 ), osg::Vec3( .5, 0., 0. ), osg::Vec3( 0., 0., .5 ) );
        osg::Vec4Array* c = new osg::Vec4Array();
        c->push_back( osg::Vec4( .2, .2, 1., 1. ) );
        decal->setColorArray( c );
        decal->setColorBinding( osg::Geometry::BIND_OVERALL );
        osg::Geode* decalGeode = new osg::Geode;
        decalGeode->addDrawable( decal );

        osg::MatrixTransform* mt = new osg::MatrixTransform( osg::Matrix::translate( osg::Vec3( -1., 0., 0. ) ) );
        mt->addChild( decalGeode );
        mt->setStateSet( stateset.get() );
        grp->addChild( mt );
    }
            
    // Draw a box with a glow effect.
    {
        // Render order is irrelevant, but add the magic glow uniform.
        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        stateset->addUniform( new osg::Uniform( "glowColor", glowColor ) );

        osg::MatrixTransform* mt = new osg::MatrixTransform( osg::Matrix::translate( osg::Vec3( .5, 0., 0. ) ) );
        mt->addChild( boxGeode );
        mt->setStateSet( stateset.get() );
        grp->addChild( mt );
    }

    return grp;
}




////////////////////////////////////////////////////////////////////////////////
int main( int argc, char** argv )
{
    RTTGlow rttGlow;
    rttGlow.init( argc, argv );

    while (!rttGlow.getViewer()->done())
    {
        rttGlow.update();
        rttGlow.getViewer()->frame();
    }
    return 0;
}
