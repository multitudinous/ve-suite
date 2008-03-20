#ifndef CAMERA_PLACEMENT_TOOL_SHADERS_H
#define CAMERA_PLACEMENT_TOOL_SHADERS_H

// --- My Includes --- //
#include "CameraPlacementToolShadersPtr.h"

// --- C/C++ Includes --- //
#include <string>

namespace cpt
{
/*----------------------------------------------------------------------------*/
class CameraPlacementToolShaders
{
public:
    CameraPlacementToolShaders();
    ~CameraPlacementToolShaders();

private:
    void CreateShaderSources();
    void CreateShaderPrograms();

    std::string mProjectionVertexSource;
    std::string mProjectionFragmentSource;

};
/*----------------------------------------------------------------------------*/
} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_SHADERS_H
