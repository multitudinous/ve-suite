// --- My Includes --- //
#include "Scene.h"
#include "Camera.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- osgAL Includes --- //

// --- Bullet Includes --- //

// --- C/C++ Libraries --- //

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
Scene::Scene( ves::xplorer::scenegraph::DCS* pluginDCS )
:
m_pluginDCS( pluginDCS ),
m_camera( new cpt::Camera( pluginDCS ) )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Scene::~Scene()
{
    delete m_camera;
}
////////////////////////////////////////////////////////////////////////////////
