#ifndef WORLD_H
#define WORLD_H

// --- VE-Suite Includes --- //
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class DCS;
    class CADEntity;
    class PhysicsSimulator;
}
}
}

// --- osgAL Includes --- //
#ifdef VE_SOUND
namespace osgAL
{
    class SoundManager;
}
#endif

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
    class TextureCubeMap;
}

namespace demo
{
class FunnelEntity;
class MarbleEntity;
class QuarterEntity;
class RailingEntity;
class SlideEntity;
class WaterEntity;

class World
{
public:
    World( ves::xplorer::scenegraph::DCS* pluginDCS,
           ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
#ifdef VE_SOUND
           , osgAL::SoundManager* soundManager
#endif
           );

    ~World();

    void Initialize();
    void PreFrameUpdate();

    ves::xplorer::scenegraph::DCS* GetPluginDCS();

private:
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_pluginDCS;

    ves::xplorer::scenegraph::PhysicsSimulator* m_physicsSimulator;

#ifdef VE_SOUND
    osgAL::SoundManager* m_soundManager;
#endif

    osg::ref_ptr< osg::TextureCubeMap > m_tcm;

    demo::FunnelEntity* m_funnelEntity;
    demo::MarbleEntity* m_marbleEntity;
    demo::QuarterEntity* m_quarterEntity;
    demo::RailingEntity* m_railingEntity;
    demo::SlideEntity* m_slideEntity;
    demo::WaterEntity* m_waterEntity;
};
}

#endif // end WORLD_H
