#ifndef SCENE_H
#define SCENE_H

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

// --- C/C++ Includes --- //

namespace cpt
{
// --- My Includes --- //
class Camera;

/*----------------------------------------------------------------------------*/
class Scene
{
public:
    Scene( ves::xplorer::scenegraph::DCS* pluginDCS );
    ~Scene();

private:
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_pluginDCS;

    cpt::Camera* m_camera;
};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //SCENE_H
