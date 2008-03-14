#ifndef CAMERA_ENTITY_H
#define CAMERA_ENTITY_H

// --- VE-Suite Includes --- //
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class DCS;
}
}
}

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
    class Camera;
    class Geode;
    class TexGenNode;
}

// --- C/C++ Libraries --- //
#include <string>

namespace cpt
{
// --- My Includes --- //
class CameraEntityCallback;

/*----------------------------------------------------------------------------*/
class CameraEntity
{
public:
    CameraEntity( ves::xplorer::scenegraph::DCS* parentDCS );
    ~CameraEntity();

    void SetNameAndDescriptions( const std::string& name );
    void DrawViewFrustum( bool onOff );
    
    ves::xplorer::scenegraph::DCS* GetDCS();

    osg::TexGenNode* GetTexGenNode();

protected:
    
private:
    void Initialize( ves::xplorer::scenegraph::DCS* parentDCS );
    void CreateViewFrustumGeometry();

    osg::ref_ptr< osg::Camera > m_camera;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_dcs;

    osg::ref_ptr< osg::Geode > m_frustum;
    osg::ref_ptr< osg::TexGenNode > m_texGenNode;

    osg::ref_ptr< cpt::CameraEntityCallback > m_cameraEntityCallback;
};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_ENTITY_H