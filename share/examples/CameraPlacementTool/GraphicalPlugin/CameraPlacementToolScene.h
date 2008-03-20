#ifndef CAMERA_PLACEMENT_TOOL_SCENE_H
#define CAMERA_PLACEMENT_TOOL_SCENE_H

// --- My Includes --- //
#include "CameraPlacementToolScenePtr.h"
#include "CameraPlacementToolShadersPtr.h"

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
class CameraPlacementToolScene
{
public:
    CameraPlacementToolScene( ves::xplorer::scenegraph::DCS* pluginDCS );
    ~CameraPlacementToolScene();

    cpt::CameraEntity* GetActiveCameraEntity();

private:
    void Initialize();

    cpt::CameraPlacementToolShadersPtr mShaders;
    osg::ref_ptr< cpt::CameraEntity > mCameraEntity;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mTorus;

};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_SCENE_H
