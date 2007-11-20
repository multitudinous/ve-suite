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
}
}
}

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
class RampEntity;
class SlideEntity;
class WaterEntity;

class World
{
public:
    World( ves::xplorer::scenegraph::DCS* pluginDCS );
    ~World();

    void Initialize();
    void PreFrameUpdate();

    ves::xplorer::scenegraph::DCS* GetPluginDCS();

private:
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_pluginDCS;

    osg::ref_ptr< osg::TextureCubeMap > m_tcm;

    demo::FunnelEntity* m_funnelEntity;
    demo::MarbleEntity* m_marbleEntity;
    demo::QuarterEntity* m_quarterEntity;
    demo::RampEntity* m_rampEntity;
    demo::SlideEntity* m_slideEntity;
    demo::WaterEntity* m_waterEntity;
};
}

#endif // end WORLD_H
