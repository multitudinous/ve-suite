#ifndef CAMERA_PLACEMENT_TOOL_GP_H
#define CAMERA_PLACEMENT_TOOL_GP_H

// --- My Includes --- //
#include "CameraPlacementToolScenePtr.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>

namespace cpt
{
class VE_USER_PLUGIN_EXPORTS CameraPlacementToolGP :
    public ves::xplorer::plugin::PluginBase
{
public:
    CameraPlacementToolGP();
    virtual ~CameraPlacementToolGP();
    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );

protected:
    void UpdateParams();

private:
    cpt::CameraPlacementToolScenePtr mScene;
};
CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( CameraPlacementToolGP )
} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_GP_H
