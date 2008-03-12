#ifndef CAMERA_H
#define CAMERA_H

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

// --- vrJuggler Includes --- //
//#include <gmtl/Matrix.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Matrix>

namespace osg
{
    class Node;
}

// --- C/C++ Libraries --- //
#include <string>

namespace cpt
{

/*----------------------------------------------------------------------------*/
class Camera
{
public:
    Camera( ves::xplorer::scenegraph::DCS* parentDCS );
    virtual ~Camera();

    void Initialize();

    void SetNameAndDescriptions( const std::string& name );

    void DrawViewFrustum();

    ves::xplorer::scenegraph::DCS* GetDCS();

    osg::Matrixd GetModelViewMatrix();
    osg::Matrixd GetProjectionMatrix();

protected:
    
private:
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_dcs;

    osg::Matrixd m_modelViewMatrix;
    osg::Matrixd m_projectionMatrix;
};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_H