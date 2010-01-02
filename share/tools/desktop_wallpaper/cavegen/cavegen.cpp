//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osg/Version>
#include <osgDB/WriteFile>
#include <osgGA/GUIEventHandler>
#include <osgGA/TrackballManipulator>
#include <osgGA/TerrainManipulator>
#include <osgViewer/Viewer>
#include <osg/Node>
#include <osg/Camera>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/Image>
#include <osg/Depth>
#include <osg/BlendColor>
#include <osg/BlendFunc>

#include "RenderTexture.h"
#include "Wall.h"

#include <osgDB/FileUtils>
#include <osg/io_utils>
#include <iostream>
#include <sstream>
#include <vector>
#include <string>


// Global variables.
std::string g_prefix;
osg::ref_ptr< osg::MatrixTransform > g_wallGroup;
osg::ref_ptr< osg::BlendColor > g_BlendColor;


struct CaptureCB : public osg::Camera::DrawCallback
{
public:
    CaptureCB( const Wall& wall, int w, int h )
      : wall_( wall ),
        w_( w ),
        h_( h )
    {}

    virtual void operator()( osg::RenderInfo& ri ) const
    {
        osg::Image* image = new osg::Image;
        std::string fName( g_prefix + wall_.suffix_ + std::string( ".png" ) );

        osg::notify( osg::ALWAYS ) << "Reading image for file " << fName << " ... " << std::endl;
        image->readPixels( 0, 0, w_, h_, GL_RGBA, GL_UNSIGNED_BYTE );

        osg::notify( osg::ALWAYS ) << "  Writing file " << fName << " ... " << std::endl;
        osgDB::writeImageFile( *image, fName );

        osg::notify( osg::ALWAYS ) << "  Capture complete." << std::endl;
    }

protected:
    const Wall& wall_;
    int w_, h_;
};

class KeyHandler : public osgGA::GUIEventHandler
{
public:
    KeyHandler( RenderTexture& rtt, WallList& wl )
      : rtt_( rtt ),
        wl_( wl ),
        frameCount_( -1 ),
        done_( false )
    {
        _captureGroup = new osg::Group;

        {
            osg::StateSet* ss = _captureGroup->getOrCreateStateSet();
            ss->setTextureMode( 1, GL_TEXTURE_2D,
                osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );

            std::string fragmentSource =
            "uniform sampler2D colorMap; \n"
            "uniform float xScale; \n"
            "uniform float yScale; \n"
            "void main() \n"
            "{ \n"
            "    vec4 t = gl_TexCoord[ 0 ];\n"
            "    vec2 tc = vec2( (t.x/t.w + 1.) * .5 * xScale, (t.t/t.w + 1.) * .5 * yScale );\n"
            "    vec4 color = texture2D( colorMap, tc ); \n"
            "    gl_FragColor = color; \n"
            "} \n";

            osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
            fragmentShader->setType( osg::Shader::FRAGMENT );
            fragmentShader->setShaderSource( fragmentSource );

            osg::ref_ptr< osg::Program > program = new osg::Program();
            program->addShader( fragmentShader.get() );

            ss->setAttribute( program.get(),
                osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        }

        int count( 10000 );
        WallList::iterator wit;
        for( wit=wl_.begin(); wit!=wl_.end(); wit++ )
        {
            osg::Camera* cam = new osg::Camera;
            cam->getOrCreateStateSet()->setRenderBinDetails( count++, "RenderBin" );
            cam->setReferenceFrame( osg::Transform::ABSOLUTE_RF );
            cam->setProjectionMatrix( wit->getProj() );
            cam->setViewMatrix( wit->getView() );

            const osg::Viewport* vp = rtt.getViewer()->getCamera()->getViewport();
            cam->setPostDrawCallback( new CaptureCB( *wit, vp->width(), vp->height() ) );

            osg::Geode* geode( wit->getGeode() );
            geode->setCullingActive( false );

            cam->addChild( geode );
            _captureGroup->addChild( cam );
        }
    }

    bool handle( const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& aa )
   {
        switch( ea.getEventType() )
        {
            case( osgGA::GUIEventAdapter::KEYUP ):
            {
                if( ea.getKey()=='1' )
                {
                    rtt_.setManipulator( new osgGA::TrackballManipulator );
                    return( true );
                }
                if( ea.getKey()=='4' )
                {
                    osgGA::TerrainManipulator* tm = new osgGA::TerrainManipulator;
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 9 ) && ( OSG_VERSION_PATCH >= 6 ) )
                    ;
#else
                    tm->setRotationMode( osgGA::TerrainManipulator::ELEVATION_AZIM_ROLL );
#endif
                    rtt_.setManipulator( tm );
                    return( true );
                }
                if( (ea.getKey()=='D') || (ea.getKey()=='d') )
                {
                    osg::Vec4 c( g_BlendColor->getConstantColor() );
                    if( c[3] == 0. )
                        c[3] = .2;
                    else
                        c[3] = 0.;
                    g_BlendColor->setConstantColor( c );
                    return( true );
                }
                if( (ea.getKey()=='G') || (ea.getKey()=='g') )
                {
                    frameCount_ = 1;
                    rtt_.setDisplayStage( _captureGroup.get() );
                    g_BlendColor->setConstantColor( osg::Vec4( 0., 0., 0., 0. ) );
                    return( true );
                }
                if( (ea.getKey()=='L') || (ea.getKey()=='l') )
                {
                    osg::StateSet* ss = rtt_.getSceneParent()->getOrCreateStateSet();
                    osg::StateAttribute::GLModeValue mode = ss->getMode( GL_LIGHTING );
                    if( mode & osg::StateAttribute::ON )
                        ss->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
                    else
                        ss->setMode( GL_LIGHTING, osg::StateAttribute::ON );
                    return( true );
                }
                return( false );
            }
            case( osgGA::GUIEventAdapter::FRAME ):
            {
                if( frameCount_ >= 0 )
                {
                    if( frameCount_-- == 0 )
                    {
                        rtt_.setDisplayStage( NULL );
                        done_ = true;
                    }
                    return( true );
                }
                return( false );
            }
            default:
				break;
        }
        return( false );
    }

    bool isDone() const
    {
        return( done_ );
    }

protected:
    RenderTexture& rtt_;
    WallList& wl_;
    int frameCount_;

    osg::ref_ptr< osg::Group > _captureGroup;

    bool done_;
};


int main( int argc, char** argv )
{
    osg::ArgumentParser arguments( &argc, argv );

    arguments.getApplicationUsage()->setApplicationName( arguments.getApplicationName() );
    arguments.getApplicationUsage()->setDescription( arguments.getApplicationName() + " creates desktop wallpaper for a 4-wall cave system." );
    arguments.getApplicationUsage()->setCommandLineUsage( arguments.getApplicationName() + " [options] filename\n" +
        "\t<filename> is the model to display.\n" +
        "\t'G/g': generate desktop images and exit.\n" +
        "\t'D/d': Toggle debug overlay.\n" +
        "\t'L/l': Toggle lighting.\n" +
        "\t'1': TrackballManipulator (default).\n" +
        "\t'1': TerrainManipulator (default).\n"
        );
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
    WallList wallList = readWallFile( config, viewPos, g_prefix );
    if( wallList.empty() )
        return( 1 );


    // Create the RTT object.
    RenderTexture rtt;
    rtt.setClearColor( osg::Vec4( 0., 0., 0., 1. ) );
    rtt.setFOV( 70.f );
    if( !rtt.init( argc, argv ) )
        return( 1 );

    const osg::Viewport* vp = rtt.getViewer()->getCamera()->getViewport();
    float windowWidth = (float)( vp->width() );
    float windowHeight = (float)( vp->height() );

    // View and projection matrices for the wall
    osg::Matrix wallView( osg::Matrix::lookAt( viewPos, osg::Vec3( 0., 0., 1. ), osg::Vec3( 0., 0., 1. ) ) );
    osg::Matrix wallProjection( osg::Matrix::perspective( 70., windowWidth/windowHeight, 1., 40000000. ) );

    // Add each wall geometry to the wallGroup.
    g_wallGroup = new osg::MatrixTransform( wallView );
    g_wallGroup->setReferenceFrame( osg::Transform::ABSOLUTE_RF );

    WallList::iterator wit;
    for( wit=wallList.begin(); wit!=wallList.end(); wit++ )
    {
        wit->createGeode( wallView * wallProjection );
        g_wallGroup->addChild( wit->getGeode() );
    }
    g_wallGroup->setCullingActive( false );
    rtt.getSceneParent()->addChild( g_wallGroup.get() );

    // Set the wallGroup StateSet.
    {
        osg::StateSet* ss = g_wallGroup->getOrCreateStateSet();
        ss->setAttributeAndModes( new osg::Depth( osg::Depth::ALWAYS ) ); 
        g_BlendColor = new osg::BlendColor( osg::Vec4( 1., 0., 1., .2 ) );
        ss->setAttributeAndModes( g_BlendColor.get() );
        ss->setAttributeAndModes( new osg::BlendFunc( osg::BlendFunc::CONSTANT_ALPHA, osg::BlendFunc::ONE_MINUS_CONSTANT_ALPHA ) );
    }
    rtt.getRTTCamera()->setProjectionMatrix( wallProjection );
    rtt.getRTTCamera()->setComputeNearFarMode( osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR );

    KeyHandler* kh = new KeyHandler( rtt, wallList );
    rtt.getViewer()->addEventHandler( kh );


    while (!rtt.getViewer()->done() && !kh->isDone() )
    {
        rtt.update();
        rtt.getViewer()->frame();
    }
    return 0;
}
