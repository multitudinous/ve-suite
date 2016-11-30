#ifndef VES_VR_CAMERA_MANAGER
#define VES_VR_CAMERA_MANAGER

#include <osvr/RenderKit/RenderManager.h>

#include <osg/Camera>
#include <osg/Texture2D>

#include <vector>

namespace osg {
    class Camera;
}

namespace ves {
namespace xplorer {

class VRCameraManager {
public:
    VRCameraManager();
  
    void Initialize( std::vector< osvr::renderkit::RenderInfo >& render_info );
    void InitScene();
    void ContextInit( osg::State& state );

    GLuint GetColorBufferID( std::size_t eye_pos, const unsigned int context_id );
    osg::Camera* GetRTTCamera( std::size_t eye_pos );
    bool ColorBuffersReady( const unsigned int context_id );
private:
    osg::Camera* SetupRTTCamera( osvr::renderkit::RenderInfo& render_info );
  
    std::vector< osg::ref_ptr< osg::Camera > > m_cameras;
    std::vector< osg::ref_ptr< osg::Texture2D > > m_colorTextures;
    //std::vector< GLuint > m_colorBufferIDs;
};

} // namespace xplorer
} // namespace ves

#endif
