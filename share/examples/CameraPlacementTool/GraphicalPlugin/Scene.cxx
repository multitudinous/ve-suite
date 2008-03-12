// --- My Includes --- //
#include "Scene.h"
#include "Camera.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Texture2D>
#include <osg/TexGenNode>
#include <osg/CullFace>

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>

#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>
#include <gmtl/Matrix.h>
#include <gmtl/Vec.h>

// --- Bullet Includes --- //

// --- C/C++ Libraries --- //

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
Scene::Scene( ves::xplorer::scenegraph::DCS* pluginDCS )
:
m_pluginDCS( pluginDCS ),
m_torus( new ves::xplorer::scenegraph::DCS() ),
m_camera( new cpt::Camera( pluginDCS ) )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
Scene::~Scene()
{
    delete m_camera;
}
////////////////////////////////////////////////////////////////////////////////
void Scene::Initialize()
{
    m_camera->SetNameAndDescriptions( std::string( "Camera" ) );

    m_torus->setPosition( osg::Vec3d( 0, 10, 0 ) );
    osg::ref_ptr< osg::Node > node= osgDB::readNodeFile( std::string( "Models/torus.osg" ) );
    m_torus->addChild( node.get() );
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    m_torus->setDescriptions( descriptorsList );
    m_torus->setName( std::string( "Torus" ) );
    m_pluginDCS->AddChild( m_torus.get() );

    CreateProjectionTexture();
}
////////////////////////////////////////////////////////////////////////////////
void Scene::CreateProjectionTexture()
{
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    osg::ref_ptr< osg::Texture2D > texture2D = new osg::Texture2D();
    osg::ref_ptr< osg::TexGenNode > texGenNode = new osg::TexGenNode();
    
    //Create a 2D texture
    texture2D->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    texture2D->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    texture2D->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_BORDER );
    texture2D->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_BORDER );

    osg::ref_ptr< osg::Image > image2D = osgDB::readImageFile( "Textures/image.jpg" );
    texture2D->setImage( image2D.get() );
    //osgDB::writeImageFile( *image2D.get(), std::string( "Textures/texture2D.jpg" ) );

    /*
    const unsigned int size = 128;
    //texture2D->setTextureSize( size, size );

    //Then create the 2D image to fill with data
    osg::ref_ptr< osg::Image > image2D = new osg::Image();
    unsigned char* data2D = new unsigned char[ 4 * size * size ];

    long count = 0;
    for( unsigned int t = 0; t < size; ++t )
    {
        for( unsigned int s = 0; s < size; ++s )
        {
            //store unsigned values [ 0, 255 ]
            //Red
            data2D[ count++ ] = 0;
            //Green
            data2D[ count++ ] = 255;
            //Blue
            data2D[ count++ ] = 0;
            //Alpha
            data2D[ count++ ] = 255;

        }
    }

    //Use GL_RGBA4 which should be cross platform.
    image2D->setImage( size, size, 0, GL_RGBA4, GL_RGBA, GL_UNSIGNED_BYTE, data2D, osg::Image::USE_NEW_DELETE );
    texture2D->setImage( image2D.get() );

    //osgDB::writeImageFile( *image2D.get(), std::string( "Textures/texture2D.jpg" ) );
*/

    //Create the texture

    //texture2D->setInternalFormat( GL_DEPTH_COMPONENT );
    //texture2D->setSourceType( GL_UNSIGNED_INT );

    //texture2D->setShadowComparison( true );
    //texture2D->setShadowCompareFunc( osg::Texture::LEQUAL );

    //texture2D->setShadowTextureMode( osg::Texture::LUMINANCE );


    //Set up the "render to texture" camera
    //{
        //Create the camera
        //m_camera->setClearMask( GL_DEPTH_BUFFER_BIT );
        //m_camera->setClearColor( osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        //m_camera->setComputeNearFarMode( osg::Camera::DO_NOT_COMPUTE_NEAR_FAR );

        //Set viewport
        //m_camera->setViewport( 0, 0, texWidth, texHeight );

        //osg::ref_ptr< osg::StateSet > localStateset = m_camera->getOrCreateStateSet();
        //localStateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

        //float factor = 2.0f;
        //float units = 4.0f;

        //osg::ref_ptr< osg::PolygonOffset > polygonOffset = new osg::PolygonOffset();
        //polygonOffset->setFactor( factor );
        //polygonOffset->setUnits( units );
        //localStateset->setAttribute( polygonOffset.get(), osg::StateAttribute::ON );
        //localStateset->setMode( GL_POLYGON_OFFSET_FILL, osg::StateAttribute::ON );

        //osg::ref_ptr< osg::CullFace > cullFace = new osg::CullFace();
        //cullFace->setMode( osg::CullFace::FRONT );
        //localStateset->setAttribute( cullFace.get(), osg::StateAttribute::ON );
        //localStateset->setMode( GL_CULL_FACE, osg::StateAttribute::ON );

        //Set the camera to render before the main camera
        //m_camera->setRenderOrder( osg::Camera::PRE_RENDER );

        //Tell the camera to use OpenGL frame buffer object where supported
        //m_camera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );

        //Attach the texture and use it as the color buffer
        //m_camera->attach( osg::Camera::DEPTH_BUFFER, texture2D.get() );

        //Add subgraph to render
        //m_camera->addChild( textureedScene.get() );

        //Create the texgen node to project the tex coords onto the subgraph
        texGenNode->setTextureUnit( 0 );

        osg::BoundingSphere bs = m_torus->getBound();
        //for( unsigned int i = 0; i < m_camera->getNumChildren(); ++i )
        //{
            //bs.expandBy( m_camera->getChild( i )->getBound() );
        //}

        
        double* temp = m_camera->GetDCS()->GetVETranslationArray();
        osg::Vec3 position( temp[ 0 ], temp[ 1 ], temp[ 2 ] );
        float centerDistance = ( position - bs.center() ).length();

        float znear = centerDistance - bs.radius();
        float zfar = centerDistance + bs.radius();
        float zNearRatio = 0.001f;
        if( znear < zfar * zNearRatio )
        {
            znear = zfar * zNearRatio;
        }

        float top = ( bs.radius() / centerDistance ) * znear;
        float right = top;

        //m_camera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
        //m_camera->setProjectionMatrixAsFrustum( -right, right, -top, top, znear, zfar );
        //m_camera->setViewMatrixAsLookAt( position, bs.center(), osg::Vec3( 0.0f, 1.0f, 0.0f ) );

        //Compute the matrix which takes a vertex from local coords into tex coords
        osg::Matrixd modelViewMatrix;
        osg::Matrixd projectionMatrix;
        //modelViewMatrix.set( m_torus->GetMat().getData() );
        //osg::Matrixd::invert( modelViewMatrix );
        //projectionMatrix.makeFrustum( -right, right, -top, top, znear, zfar );
        //modelViewMatrix.m
        modelViewMatrix.makeLookAt( position, osg::Vec3( 0, 7.5, 0 ), osg::Vec3( 0.0f, 0.0f, 1.0f ) );
        osg::Matrix MVPT = modelViewMatrix *
                           //projectionMatrix *
                           //m_camera->GetModelViewMatrix() *
                           m_camera->GetProjectionMatrix() *
                           osg::Matrix::translate( 1.0f, 1.0f, 1.0f ) *
                           osg::Matrix::scale( 0.5f, 0.5f, 0.5f );

        //Texture Generation
        texGenNode->getTexGen()->setMode( osg::TexGen::EYE_LINEAR );
        texGenNode->getTexGen()->setPlanesFromMatrix( MVPT );
        m_pluginDCS->addChild( texGenNode.get() );
    //}

    //m_pluginDCS->addChild( m_camera.get() );
    
    //osg::ref_ptr< osg::CullFace > cullFace = new osg::CullFace();
    //cullFace->setMode( osg::CullFace::BACK );
    //stateset->setAttribute( cullFace.get(), osg::StateAttribute::ON );
    //stateset->setMode( GL_CULL_FACE, osg::StateAttribute::ON );

    stateset->setTextureAttributeAndModes( 0, texture2D.get(), osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_S, osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_T, osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_R, osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON );

    
    m_torus->setStateSet( stateset.get() );

    //osg::ref_ptr< osg::Uniform > shadowMap = new osg::Uniform( "shadowMap", 0 );
    //stateset->addUniform( shadowMap.get() );
}
////////////////////////////////////////////////////////////////////////////////
