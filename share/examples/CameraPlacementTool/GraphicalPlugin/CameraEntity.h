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
#include <osg/Camera>

namespace osg
{
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
class CameraEntity : public osg::Camera
{
public:
    CameraEntity();
    CameraEntity( ves::xplorer::scenegraph::DCS* parentDCS );
    CameraEntity( const CameraEntity& cameraEntity,
                  const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Node( cpt, CameraEntity );

    void CalculateMatrixMVPT();

    void DrawCameraGeometry( bool onOff );
    void DrawViewFrustum( bool onOff );
    
    ves::xplorer::scenegraph::DCS* GetDCS();

    osg::TexGenNode* GetTexGenNode();

    osg::Matrixd GetMatrixMVPT();

    void SetNameAndDescriptions( const std::string& name );

protected:
    virtual ~CameraEntity();

private:
    void CreateViewFrustumGeode();
    void Initialize( ves::xplorer::scenegraph::DCS* parentDCS );

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mDCS;

    osg::ref_ptr< osg::Node > mCameraGeometry;
    osg::ref_ptr< osg::Geode > mFrustumGeode;
    osg::ref_ptr< osg::TexGenNode > mTexGenNode;

    osg::ref_ptr< cpt::CameraEntityCallback > mCameraEntityCallback;

    osg::Matrixd mMVPT;
};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_ENTITY_H
