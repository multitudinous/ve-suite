// --- OSG Includes --- //
#include <osg/Node>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Camera>
#include <osg/Stencil>
#include <osg/LineWidth>
#include <osg/Material>
#include <osg/PolygonMode>
#include <osg/Texture2D>

#include <osgDB/ReadFile>

#include <osgGA/TrackballManipulator>

#include <osgUtil/Optimizer>

#include <osgViewer/Viewer>

// --- C/C++ Libraries --- //
#include <iostream>
#include <string>
#include <map>

//jbkoch - these do not seem to be defined in osg
#ifndef GL_DEPTH_STENCIL_EXT
#define GL_DEPTH_STENCIL_EXT 0x84F9
#endif//GL_DEPTH_STENCIL_EXT
#ifndef GL_UNSIGNED_INT_24_8_EXT
#define GL_UNSIGNED_INT_24_8_EXT 0x84FA
#endif//GL_UNSIGNED_INT_24_8_EXT
#ifndef GL_DEPTH24_STENCIL8_EXT
#define GL_DEPTH24_STENCIL8_EXT 0x88F0
#endif//GL_DEPTH24_STENCIL8_EXT
#ifndef GL_TEXTURE_STENCIL_SIZE_EXT
#define GL_TEXTURE_STENCIL_SIZE_EXT 0x88F1
#endif//GL_TEXTURE_STENCIL_SIZE_EXT

////////////////////////////////////////////////////////////////////////////////
std::map< std::string, double > GetScreenCorners( osg::Matrixd& projectionMatrix )
{
    std::map< std::string, double > screenCorners;

    const double nearPlane = projectionMatrix( 3, 2 ) /
                           ( projectionMatrix( 2, 2 ) - 1.0 );

    const double nLeft =   nearPlane * ( projectionMatrix( 2, 0 ) - 1.0 ) /
                                         projectionMatrix( 0, 0 );
    const double nRight =  nearPlane * ( projectionMatrix( 2, 0 ) + 1.0 ) /
                                         projectionMatrix( 0, 0 );
    const double nTop =    nearPlane * ( projectionMatrix( 2, 1 ) + 1.0 ) /
                                         projectionMatrix( 1, 1 );
    const double nBottom = nearPlane * ( projectionMatrix( 2, 1 ) - 1.0 ) /
                                         projectionMatrix( 1, 1 );

    screenCorners[ std::string( "xmin" )] = nLeft;
    screenCorners[ std::string( "xmax" )] = nRight;
    screenCorners[ std::string( "ymin" )] = nBottom;
    screenCorners[ std::string( "ymax" )] = nTop;
    screenCorners[ std::string( "zval" )] = nearPlane;

    return screenCorners;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* CreateBox()
{
    osg::Geode* geode = new osg::Geode();
    osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();

    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    vertices->resize( 24 );
    //Left
    (*vertices)[ 0 ].set( -0.5,  0.5,  0.5 );
    (*vertices)[ 1 ].set( -0.5,  0.5, -0.5 );
    (*vertices)[ 2 ].set( -0.5, -0.5, -0.5 );
    (*vertices)[ 3 ].set( -0.5, -0.5,  0.5 );
    //Near
    (*vertices)[ 4 ].set( -0.5, -0.5,  0.5 );
    (*vertices)[ 5 ].set( -0.5, -0.5, -0.5 );
    (*vertices)[ 6 ].set(  0.5, -0.5, -0.5 );
    (*vertices)[ 7 ].set(  0.5, -0.5,  0.5 );	
    //Right
    (*vertices)[ 8 ].set( 0.5, -0.5,  0.5 );
    (*vertices)[ 9 ].set( 0.5, -0.5, -0.5 );
    (*vertices)[ 10 ].set( 0.5,  0.5, -0.5 );
    (*vertices)[ 11 ].set( 0.5,  0.5,  0.5 );
    //Far
    (*vertices)[ 12 ].set(  0.5, 0.5,  0.5 );
    (*vertices)[ 13 ].set(  0.5, 0.5, -0.5 );
    (*vertices)[ 14 ].set( -0.5, 0.5, -0.5 );
    (*vertices)[ 15 ].set( -0.5, 0.5,  0.5 );	
    //Top
    (*vertices)[ 16 ].set( -0.5,  0.5, 0.5 );
    (*vertices)[ 17 ].set( -0.5, -0.5, 0.5 );
    (*vertices)[ 18 ].set(  0.5, -0.5, 0.5 );
    (*vertices)[ 19 ].set(  0.5,  0.5, 0.5 );
    //Bottom
    (*vertices)[ 20 ].set( -0.5, -0.5, -0.5 );
    (*vertices)[ 21 ].set( -0.5,  0.5, -0.5 );
    (*vertices)[ 22 ].set(  0.5,  0.5, -0.5 );
    (*vertices)[ 23 ].set(  0.5, -0.5, -0.5 );
    geometry->setVertexArray( vertices.get() );

    osg::ref_ptr< osg::Vec3Array > normals = new osg::Vec3Array();
    normals->resize( 6 );
    //Left
    (*normals)[ 0 ].set( -1.0,  0.0,  0.0 );
    //Near
    (*normals)[ 1 ].set(  0.0, -1.0,  0.0 );
    //Right
    (*normals)[ 2 ].set(  1.0,  0.0,  0.0 );
    //Far
    (*normals)[ 3 ].set(  0.0,  1.0,  0.0 );
    //Top
    (*normals)[ 4 ].set(  0.0,  0.0,  1.0 );
    //Bottom
    (*normals)[ 5 ].set(  0.0,  0.0, -1.0 );
    geometry->setNormalArray( normals.get() );
    geometry->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    geometry->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, vertices->size() ) );
    geode->addDrawable( geometry.get() );

    return geode;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* CreateSAQuad(
    std::map< std::string, double >& screenCorners,
    osg::Texture2D* colorTexture, osg::Texture2D* depthStencilTexture  )
{
    double xMin = screenCorners.find( "xmin" )->second;
    double xMax = screenCorners.find( "xmax" )->second;
    double yMin = screenCorners.find( "ymin" )->second;
    double yMax = screenCorners.find( "ymax" )->second;
    double zVal = screenCorners.find( "zval" )->second;
    
    osg::Geode* quadGeode = new osg::Geode();
    osg::ref_ptr< osg::Geometry > quadGeometry = new osg::Geometry();

    osg::ref_ptr< osg::Vec3Array > quadVertices = new osg::Vec3Array();
    quadVertices->resize( 4 );
    (*quadVertices)[ 0 ].set( xMin, -zVal, yMin );
    (*quadVertices)[ 1 ].set( xMax, -zVal, yMin );
    (*quadVertices)[ 2 ].set( xMax, -zVal, yMax );
    (*quadVertices)[ 3 ].set( xMin, -zVal, yMax );
    quadGeometry->setVertexArray( quadVertices.get() );
    
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();
    quadTexCoords->resize( 4 );
    (*quadTexCoords)[ 0 ].set( 0, 0 );
    (*quadTexCoords)[ 1 ].set( 1, 0 );
    (*quadTexCoords)[ 2 ].set( 1, 1 );
    (*quadTexCoords)[ 3 ].set( 0, 1 );
    quadGeometry->setTexCoordArray( 0, quadTexCoords.get() );
    
    quadGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );
    
    quadGeode->addDrawable( quadGeometry.get() );
    quadGeode->setCullingActive( false );
   

    std::string vertexSource =
    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "gl_TexCoord[ 0 ].st = gl_MultiTexCoord0.st; \n"
    "} \n";

    std::string fragmentSource =
    "uniform sampler2D colorMap; \n"
    "uniform sampler2D depthStencilMap; \n"

    "void main() \n"
    "{ \n"
        "vec4 color = texture2D( colorMap, gl_TexCoord[ 0 ].st ); \n"

        "gl_FragColor = color; \n"
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
    stateset->setRenderBinDetails( 9, std::string( "RenderBin" ) );
    stateset->setAttribute(
        program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setTextureAttributeAndModes(
        0, colorTexture, osg::StateAttribute::ON );
    stateset->setTextureAttributeAndModes(
        1, depthStencilTexture, osg::StateAttribute::ON );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

    osg::ref_ptr< osg::Uniform > colorMapUniform =
        new osg::Uniform( "colorMap", 0 );
    stateset->addUniform( colorMapUniform.get() );

    osg::ref_ptr< osg::Uniform > depthStencilMapUniform =
        new osg::Uniform( "depthStencilMap", 1 );
    stateset->addUniform( depthStencilMapUniform.get() );

    quadGeode->setStateSet( stateset.get() );

    return quadGeode;
}
////////////////////////////////////////////////////////////////////////////////
osg::Camera* CreateStencilScene( osg::Texture2D* colorTexture, osg::Texture2D* depthStencilTexture )
{
    osg::Camera* camera = new osg::Camera();
    camera->setReferenceFrame( osg::Camera::RELATIVE_RF );
    camera->setViewMatrix( osg::Matrix::identity() );
    camera->setProjectionMatrix( osg::Matrix::identity() );
    camera->setRenderOrder( osg::Camera::POST_RENDER );
    camera->setClearMask( 
        GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );
    camera->setClearColor( osg::Vec4( 0.0, 0.0, 1.0, 1.0 ) );
    camera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );

    colorTexture->setInternalFormat( GL_RGBA16F_ARB );
    colorTexture->setTextureSize( 512, 512 );
    colorTexture->setSourceFormat( GL_RGBA );
    colorTexture->setSourceType( GL_FLOAT );
    colorTexture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    colorTexture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    colorTexture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    colorTexture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );

    camera->attach( osg::Camera::COLOR_BUFFER, colorTexture, 0, 0, false, 8, 8 );

    //Use renderbuffers to get a depth and stencil buffer
    //camera->attach( osg::Camera::COLOR_BUFFER, GL_RGBA );

    depthStencilTexture->setInternalFormat( GL_DEPTH24_STENCIL8_EXT );
    depthStencilTexture->setTextureSize( 512, 512 );
    depthStencilTexture->setSourceFormat( GL_DEPTH_STENCIL_EXT );
    depthStencilTexture->setSourceType( GL_UNSIGNED_INT_24_8_EXT );
    depthStencilTexture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
    depthStencilTexture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );
    depthStencilTexture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    depthStencilTexture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );

    //Use an interleaved depth/stencil texture to get a depth and stencil buffer
    camera->attach( osg::Camera::DEPTH_BUFFER, depthStencilTexture );//, 0, 0, false, 8, 8 );
    //camera->attach( osg::Camera::STENCIL_BUFFER, depthStencilTexture );//, 0, 0, false, 8, 8 );

    //Use renderbuffers to get a depth and stencil buffer
    //camera->attach( osg::Camera::DEPTH_BUFFER, GL_DEPTH_COMPONENT24 );
    //camera->attach( osg::Camera::STENCIL_BUFFER, GL_STENCIL_INDEX8_EXT  );
    camera->setClearStencil( 0 );
    //glStencilMask(0xFFFFFFFF);

    //Implement pass #1
    {
        osg::ref_ptr< osg::Stencil > stencil = new osg::Stencil();
        stencil->setFunction( osg::Stencil::ALWAYS,    //comparison function
                              1,                       //reference value
                              ~0u );                   //comparison mask
        stencil->setOperation( osg::Stencil::KEEP,     //stencil fail
                               osg::Stencil::KEEP,     //stencil pass/depth fail
                               osg::Stencil::REPLACE );//stencil pass/depth pass

        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        stateset->setRenderBinDetails( 1, "RenderBin" );
        stateset->setMode( GL_STENCIL_TEST,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateset->setAttributeAndModes( stencil.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        osg::ref_ptr< osg::Geode > geode = CreateBox();
        geode->setStateSet( stateset.get() );
        camera->addChild( geode.get() );
    }

    //Implement pass #2
    {
        osg::ref_ptr< osg::Stencil > stencil = new osg::Stencil();
        stencil->setFunction( osg::Stencil::NOTEQUAL,  //comparison function
                              1,                       //reference value
                              ~0u );                   //comparison mask
        stencil->setOperation( osg::Stencil::KEEP,     //stencil fail
                               osg::Stencil::KEEP,     //stencil pass/depth fail
                               osg::Stencil::REPLACE );//stencil pass/depth pass

        osg::ref_ptr< osg::LineWidth > linewidth = new osg::LineWidth();
        linewidth->setWidth( 10.0 );

        osg::ref_ptr< osg::Material > material = new osg::Material();
        material->setColorMode( osg::Material::EMISSION );
        material->setEmission(
            osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );

        osg::ref_ptr< osg::PolygonMode > polymode = new osg::PolygonMode();
        polymode->setMode(
            osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );

        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        stateset->setRenderBinDetails( 2, "RenderBin" );
        stateset->setMode( GL_LIGHTING,
            osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
        stateset->setMode( GL_STENCIL_TEST,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateset->setAttributeAndModes( stencil.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateset->setAttributeAndModes( linewidth.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateset->setAttributeAndModes( material.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateset->setAttributeAndModes( polymode.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        osg::ref_ptr< osg::Geode > geode = CreateBox();
        geode->setStateSet( stateset.get() );
        camera->addChild( geode.get() );
    }
            
    return camera;
}
////////////////////////////////////////////////////////////////////////////////
void main( int argc, char** argv )
{
    osg::ArgumentParser arguments( &argc, argv );
    osgViewer::Viewer viewer( arguments );

    viewer.setUpViewInWindow( 200, 200, 512, 512 );
    osg::DisplaySettings::instance()->setMinimumNumStencilBits( 8 );

    osg::Matrixd initialViewMatrix;
    initialViewMatrix.makeLookAt( osg::Vec3( 0, 0, 0 ),
                                  osg::Vec3( 0, 1, 0 ),
                                  osg::Vec3( 0, 0, 1 ) );
    viewer.getCamera()->setViewMatrix( initialViewMatrix );
    //viewer.getCamera()->setProjectionMatrixAsPerspective( 40.0, 1.0, 1.0, 50.0 );
    viewer.getCamera()->setClearColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
    
    std::map< std::string, double > screenCorners =
        GetScreenCorners( viewer.getCamera()->getProjectionMatrix() );

    osg::ref_ptr< osg::Texture2D > colorTexture = new osg::Texture2D();
    osg::ref_ptr< osg::Texture2D > depthStencilTexture = new osg::Texture2D();
    
    osg::ref_ptr< osg::Group > root = new osg::Group();
    osg::ref_ptr< osg::Camera > camera =
        CreateStencilScene( colorTexture.get(), depthStencilTexture.get() );
    osg::ref_ptr< osg::Geode > saQuad =
        CreateSAQuad( screenCorners, colorTexture.get(), depthStencilTexture.get() );

    root->addChild( camera.get() );
    root->addChild( saQuad.get() );

    osgUtil::Optimizer optimizer;
    optimizer.optimize( root.get() );

    viewer.setSceneData( root.get() );

    viewer.run();
}
////////////////////////////////////////////////////////////////////////////////
