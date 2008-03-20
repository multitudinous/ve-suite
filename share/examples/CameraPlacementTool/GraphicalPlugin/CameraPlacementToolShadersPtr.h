#ifndef CAMERA_PLACEMENT_TOOL_SHADERS_PTR_H
#define CAMERA_PLACEMENT_TOOL_SHADERS_PTR_H

#include <ves/util/PointerTypes.h>

namespace cpt
{

/*----------------------------------------------------------------------------*/
class CameraPlacementToolShaders;

//Typedef for a SmartPtr type for the CameraPlacementToolScene
typedef ves::util::ClassPtrDef< CameraPlacementToolShaders >::type
    CameraPlacementToolShadersPtr;
typedef ves::util::SharedPtrDef< CameraPlacementToolShaders >::type
    CameraPlacementToolShadersSharedPtr;
typedef ves::util::WeakPtrDef< CameraPlacementToolShaders >::type
    CameraPlacementToolShadersWeakPtr;
typedef ves::util::ScopedPtrDef< CameraPlacementToolShaders >::type
    CameraPlacementToolShadersScopedPtr;
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_SHADERS_PTR_H
