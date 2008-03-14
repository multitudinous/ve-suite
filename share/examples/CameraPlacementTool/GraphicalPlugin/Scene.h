#ifdef SCENE_H
#define _I_DONT_KNOW_HOW_TO_MAKE_GOOD_DEFINES_OR_ADD_COPYRIGHTS

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
class CameraEntity;

/*----------------------------------------------------------------------------*/
class Scene
{
public:
    Scene( ves::xplorer::scenegraph::DCS* pluginDCS );
    ~Scene();

    void Initialize();

private:
    void CreateProjectionTexture();

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mTorus;

    cpt::CameraEntity* mCamera;


};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //SCENE_H
