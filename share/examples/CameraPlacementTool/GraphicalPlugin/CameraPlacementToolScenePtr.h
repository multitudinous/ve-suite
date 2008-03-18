#ifndef CAMERA_PLACEMENT_TOOL_SCENE_PTR_H
#define CAMERA_PLACEMENT_TOOL_SCENE_PTR_H

#include <ves/util/PointerTypes.h>

namespace cpt
{

/*----------------------------------------------------------------------------*/
class CameraPlacementToolScene;

//Typedef for a SmartPtr type for the CameraPlacementToolScene
typedef ves::util::ClassPtrDef< CameraPlacementToolScene >::type  CameraPlacementToolScenePtr;
typedef ves::util::SharedPtrDef< CameraPlacementToolScene >::type CameraPlacementToolSharedPtr;
typedef ves::util::WeakPtrDef< CameraPlacementToolScene >::type   CameraPlacementToolWeakPtr;
typedef ves::util::ScopedPtrDef< CameraPlacementToolScene >::type CameraPlacementToolScopedPtr;
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_SCENE_PTR_H
