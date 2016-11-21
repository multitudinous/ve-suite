#include "VRCameraManager.h"

#include <osg/FrameBufferObject>

using namespace ves::xplorer;

VRCameraManager::VRCameraManager() {}

void VRCameraManager::Initialize( std::vector< osvr::renderkit::RenderInfo >& render_info )
{
    for( std::size_t i = 0; i < render_info.size(); i++ ) {
        osg::Camera* camera = SetupRTTCamera( render_info[i] );

        m_cameras.push_back( camera );
        
        osg::Camera::BufferAttachmentMap map = camera->getBufferAttachmentMap();
        osg::Texture* texture_ptr = map[osg::Camera::COLOR_BUFFER]._texture.get();
        m_colorTextures.push_back( static_cast< osg::Texture2D* >( texture_ptr ) );
    }
}

osg::Camera* VRCameraManager::SetupRTTCamera( osvr::renderkit::RenderInfo& render_info )
{
    osg::Camera* camera = new osg::Camera;

    camera->setClearColor( osg::Vec4( 1.0f, 0.0f, 0.0f, 1.0f ) );
    camera->setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    camera->setReferenceFrame( osg::Transform::ABSOLUTE_RF );
    camera->setRenderOrder( osg::Camera::PRE_RENDER );
    camera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );
    camera->setViewport( 0, 0, render_info.viewport.width, render_info.viewport.height );

    GLdouble projection[16];
    osvr::renderkit::OSVR_Projection_to_OpenGL(
        projection,
        render_info.projection
    );
    osg::Matrix osg_projection( projection );

    camera->setProjectionMatrix( osg_projection );
    camera->setViewMatrix( osg::Matrix::identity() );

    osg::Texture2D* color_texture = new osg::Texture2D;

    color_texture->setTextureSize( render_info.viewport.width, render_info.viewport.height );
    color_texture->setInternalFormat( GL_RGBA );
    color_texture->setSourceFormat( GL_RGBA );
    color_texture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    color_texture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    color_texture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    color_texture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );

    camera->attach( osg::Camera::COLOR_BUFFER, color_texture, 0, 0, false, 0, 0 );

    osg::Texture2D* depth_texture = new osg::Texture2D;

    depth_texture->setTextureSize( render_info.viewport.width, render_info.viewport.height );
    depth_texture->setInternalFormat( GL_DEPTH24_STENCIL8_EXT );
    depth_texture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
    depth_texture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );
    depth_texture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    depth_texture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );

    camera->attach( osg::Camera::PACKED_DEPTH_STENCIL_BUFFER, depth_texture,
                    0, 0, false, 0, 0 );

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
