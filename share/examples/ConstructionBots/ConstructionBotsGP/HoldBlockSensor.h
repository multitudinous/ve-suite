#ifndef HOLD_BLOCK_SENSOR_H
#define HOLD_BLOCK_SENSOR_H

// --- My Includes --- //
#include "Sensor.h"

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/LineSegment>

#include <osgUtil/IntersectVisitor>

//Detects if an agent is holding a block
namespace Construction
{
class HoldBlockSensor : public Sensor
{
public:
    HoldBlockSensor( Construction::AgentEntity* agentEntity );

    virtual ~HoldBlockSensor();

    virtual void CollectInformation();

    bool HoldingBlock();

private:
    bool m_holdingBlock;

    double m_range;

    osg::ref_ptr< osg::LineSegment > beamLineSegment;

    osg::Vec3d startPoint;
    osg::Vec3d endPoint;
};
}

#endif //HOLD_BLOCK_SENSOR_H
