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

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- C/C++ Includes --- //
#include <vector>
#include <map>

namespace Construction
{
    class GridEntity;
    class BlockEntity;
    class AgentEntity;
}

namespace Construction
{
class World
{
public:
    World( int worldScale,
           ves::xplorer::scenegraph::DCS* pluginDCS,
           ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    ~World();

    void PreFrameUpdate();

    ves::xplorer::scenegraph::DCS* GetPluginDCS();

private:
    void InitFramework();
    void CreateRandomPositions( int gridSize );

    void CommunicatingBlocksAlgorithm();

    bool m_structureNotComplete;

    ves::xplorer::scenegraph::PhysicsSimulator* m_physicsSimulator;

    std::map< std::string, ves::xplorer::scenegraph::CADEntity* > m_entities;
    Construction::GridEntity* m_grid;
    std::vector< Construction::BlockEntity* > m_blocks;
    std::vector< Construction::AgentEntity* > m_agents;
    Construction::BlockEntity* m_startBlock;

    int m_worldScale;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_pluginDCS;
};
}

#endif //WORLD_H
