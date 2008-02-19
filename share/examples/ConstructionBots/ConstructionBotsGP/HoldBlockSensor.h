#ifndef HOLD_BLOCK_SENSOR_H
#define HOLD_BLOCK_SENSOR_H

// --- My Includes --- //
#include "Sensor.h"

// --- OSG Includes --- //
#include <osg/ref_ptr>

//Simulates a 3D ring of ultrasound sensors for obstacle detection
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

};
}

#endif //HOLD_BLOCK_SENSOR_H
