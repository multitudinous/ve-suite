#ifndef SENSOR_H
#define SENSOR_H

namespace Construction
{
    class AgentEntity;
}

namespace Construction
{
class Sensor
{
public:
    Sensor( Construction::AgentEntity* agentEntity );
    virtual ~Sensor();

    virtual void CollectInformation() = 0;

protected:
    Construction::AgentEntity* m_agentEntity;
};
}

#endif //SENSOR_H
