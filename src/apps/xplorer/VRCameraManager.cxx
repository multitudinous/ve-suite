#include "VRCameraManager.h"

#include <osg/FrameBufferObject>
#include <osg/LightSource>
#include <osg/LightModel>

#include <ves/xplorer/scenegraph/SceneManager.h>

using namespace ves::xplorer;

VRCameraManager::VRCameraManager() {}

void VRCameraManager::Initialize( std::vector< osvr::renderkit::RenderInfo >& render_info )
{
    for( std::size_t i = 0; i < render_info.size(); i++ ) {
        osg::Camera* camera = SetupRTTCamera( render_info[i] );

        m_cameras.push_back( camera );
        
        osg::Camera::BufferAttachmentMap map = camera->getBufferAttachmentMap();
        osg::Texture* texture_ptr = map[osg::Camera::COLOR_BUFFER]._texture.get();
        m_colorTextures.push_back( dynamic_cast< osg::Texture2D* >( texture_ptr ) );
    }
}

osg::Camera* VRCameraManager::SetupRTTCamera( osvr::renderkit::RenderInfo& render_info )
{
    osg::Camera* camera = new osg::Camera;

    camera->setClearColor( osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
    camera->setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    camera->setReferenceFrame( osg::Transform::ABSOLUTE_RF );
    camera->setRenderOrder( osg::Camera::POST_RENDER );
    camera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT, osg::Camera::FRAME_BUFFER_OBJECT );
    camera->setThreadSafeRefUnref( true );
    camera->setViewport( 0, 0, render_info.viewport.width, render_info.viewport.height );

    GLdouble projection[16];
    osvr::renderkit::OSVR_Projection_to_OpenGL(
        projection,
        render_info.projection
    );
    osg::Matrix osg_projection( projection );

    camera->setProjectionMatrix( osg_projection );
    camera->setViewMatrix( osg::Matrix::identity() );

    osg::ref_ptr< osg::Texture2D > color_texture = new osg::Texture2D;

    color_texture->setTextureSize( render_info.viewport.width, render_info.viewport.height );
    color_texture->setInternalFormat( GL_RGBA );
    color_texture->setSourceFormat( GL_RGBA );
    color_texture->setSourceType( GL_UNSIGNED_BYTE );
    color_texture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    color_texture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    color_texture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    color_texture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );

    camera->attach( osg::Camera::COLOR_BUFFER, color_texture, 0, 0, false, 0, 0 );

    //m_colorTextures.push_back( color_texture );

    osg::ref_ptr< osg::Texture2D > depth_texture = new osg::Texture2D;

    depth_texture->setTextureSize( render_info.viewport.width, render_info.viewport.height );
    depth_texture->setInternalFormat( GL_DEPTH24_STENCIL8_EXT );
    depth_texture->setSourceFormat( GL_DEPTH_STENCIL_EXT );
    depth_texture->setSourceType( GL_UNSIGNED_INT_24_8_EXT );
    depth_texture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
    depth_texture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );
    depth_texture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    depth_texture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );

    camera->attach( osg::Camera::PACKED_DEPTH_STENCIL_BUFFER, depth_texture,
                    0, 0, false, 0, 0 );

    osg::ref_ptr< osg::Light > light = new osg::Light();
    light->setLightNum( 0 );
    light->setAmbient( osg::Vec4( 0.36862, 0.36842, 0.36842, 1.0 ) );
    light->setDiffuse( osg::Vec4( 0.88627, 0.88500, 0.88500, 1.0 ) );
    light->setSpecular( osg::Vec4( 0.49019, 0.48872, 0.48872, 1.0 ) );
    //We are in openGL space
    light->setPosition( osg::Vec4( 0.0, 10000.0, 10000.0, 0.0 ) );

    osg::ref_ptr< osg::LightSource > lightSource = new osg::LightSource();
    lightSource->setLight( light.get() );
    lightSource->setReferenceFrame( osg::LightSource::RELATIVE_RF );
    camera->addChild( lightSource.get() );

    osg::ref_ptr< osg::LightModel > lightModel = new osg::LightModel();
    lightModel->setAmbientIntensity( osg::Vec4( 0.1, 0.1, 0.1, 1.0 ) );
    //Get correct specular lighting across pipes
    //See http://www.ds.arch.tue.nl/General/Staff/Joran/osg/osg_specular_problem.htm
    lightModel->setLocalViewer( true );

    //Setup the light
    osg::ref_ptr< osg::StateSet > stateset = camera->getOrCreateStateSet();
    stateset->setAssociatedModes(
        light.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setAttributeAndModes(
        lightModel.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    return camera;
}

GLuint VRCameraManager::GetColorBufferID( std::size_t eye_pos, const unsigned int context_id )
{
    osg::Texture::TextureObject* texture_obj;
    texture_obj = m_colorTextures[eye_pos]->getTextureObject( context_id );
    std::cout << "ATTEMPTING TO ACCESS TEXTURE OBJECT ID" << std::endl << std::flush;
    return texture_obj->_id;
}

osg::Camera* VRCameraManager::GetRTTCamera( std::size_t eye_pos )
{
    return m_cameras[eye_pos].get();
}

bool VRCameraManager::ColorBuffersReady( const unsigned int context_id )
{
    osg::Texture::TextureObject* texture_obj;
    texture_obj = m_colorTextures[0]->getTextureObject( context_id );

    if( texture_obj )
    {
        std::cout << "VRCameraManager: m_colorTextures[0] OK!" << std::endl << std::flush;
    }
    else
    {
        std::cout << "VRCameraManager: m_colorTextures[0] ***NOT OK***" << std::endl << std::flush;
    }
    bool left_ready = texture_obj != NULL ? true : false;
    texture_obj = m_colorTextures[1]->getTextureObject( context_id );

    if( texture_obj )
    {
        std::cout << "VRCameraManager: m_colorTextures[1] OK!" << std::endl << std::flush;
    }
    else
    {
        std::cout << "VRCameraManager: m_colorTextures[1] ***NOT OK***" << std::endl << std::flush;
    }
    bool right_ready = texture_obj != NULL ? true : false;

    std::cout << "ColorBuffersReady() accessed texture objects" << std::endl << std::flush;
    return left_ready && right_ready;
}

void VRCameraManager::InitScene()
{
    osg::Group* root = ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode();
    osg::Group* model_root = ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot();

    osg::Camera* left = GetRTTCamera( 0 );
    osg::Camera* right = GetRTTCamera( 1 );

    left->addChild( model_root );
    right->addChild( model_root );

    //osg::Group* vr_camera_group = new osg::Group;
    //vr_camera_group->addChild( left );
    //vr_camera_group->addChild( right );

    root->addChild( left );
    root->addChild( right );
}
