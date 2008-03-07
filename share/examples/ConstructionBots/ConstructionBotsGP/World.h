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
#ifdef VE_SOUND
    class Sound;
#endif
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
           ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
#ifdef VE_SOUND
         , osgAL::SoundManager* soundManager
#endif
           );

    ~World();

    void PreFrameUpdate();

private:
    void InitFramework();
    void CreateRandomPositions( int gridSize );

    void CommunicatingBlocksAlgorithm();

    bool m_structureNotComplete;

    int m_worldScale;

    ves::xplorer::scenegraph::PhysicsSimulator* m_physicsSimulator;

    std::map< std::string, ves::xplorer::scenegraph::CADEntity* > m_entities;
    Construction::GridEntity* m_grid;
    std::vector< Construction::BlockEntity* > m_blocks;
    std::vector< Construction::AgentEntity* > m_agents;
    Construction::BlockEntity* m_startBlock;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_pluginDCS;

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* m_ambientSound;
#endif
};
}

#endif //WORLD_H
