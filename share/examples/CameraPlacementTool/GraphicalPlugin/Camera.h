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
#include <gmtl/Matrix.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
    class Node;
}

// --- C/C++ Libraries --- //

namespace cpt
{
/*----------------------------------------------------------------------------*/
class Camera
{
public:
    Camera( ves::xplorer::scenegraph::DCS* parentDCS );
    virtual ~Camera();

    void Initialize();

    void DrawViewFrustum();

protected:
    
private:
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_dcs;

    gmtl::Matrix44d m_projectionMatrix;

};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_H