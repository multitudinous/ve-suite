#ifndef AGENT_ENTITY_H
#define AGENT_ENTITY_H

// --- My Includes --- //
namespace Construction
{
    class Agent;
    class BlockEntity;
    class ObstacleSensor;
    class BlockSensor;
    class SiteSensor;
    class HoldBlockSensor;
}

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- Bullet Includes --- //
class btGeneric6DofConstraint;

// --- C/C++ Libraries --- //

namespace Construction
{
class AgentEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    AgentEntity( osg::ref_ptr< Construction::Agent > agent,
                 ves::xplorer::scenegraph::DCS* pluginDCS,
                 ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~AgentEntity();

    //The agent behaviors
    void AvoidObstacle();
    void WanderAround();
    void GoToBlock();
    void PickUpBlock( Construction::BlockEntity* blockEntity );
    void GoToSite();
    void Build();

    void SetNameAndDescriptions( int number );
    void SetConstraints( int gridSize );
    void SetTargetDCS( ves::xplorer::scenegraph::DCS* targetDCS );

    bool IsBuilding();

	ves::xplorer::scenegraph::DCS* GetPluginDCS();
	ves::xplorer::scenegraph::DCS* GetTargetDCS();

    Construction::ObstacleSensor* GetObstacleSensor();
    Construction::BlockSensor* GetBlockSensor();
    Construction::SiteSensor* GetSiteSensor();
    Construction::HoldBlockSensor* GetHoldBlockSensor();

private:
    //void Set
    bool m_buildMode;

	osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_pluginDCS;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_targetDCS;

    btGeneric6DofConstraint* m_constraint;

    osg::ref_ptr< Construction::Agent > m_geometry;

    //The agent sensors
    Construction::ObstacleSensor* m_obstacleSensor;
    Construction::BlockSensor* m_blockSensor;
    Construction::SiteSensor* m_siteSensor;
    Construction::HoldBlockSensor* m_holdBlockSensor;
};
}

#endif //AGENT_ENTITY_H
