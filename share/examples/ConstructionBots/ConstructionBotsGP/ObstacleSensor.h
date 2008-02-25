#ifndef OBSTACLE_SENSOR_H
#define OBSTACLE_SENSOR_H

// --- My Includes --- //
#include "Sensor.h"

// --- OSG Includes --- //
#include <osg/ref_ptr>

#include <osgUtil/IntersectVisitor>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

// --- C/C++ Libraries --- //
#include <vector>

//Simulates a 3D ring of ultrasound sensors for obstacle detection
namespace Construction
{
class ObstacleSensor : public Sensor
{
public:
    ObstacleSensor( Construction::AgentEntity* agentEntity );

    virtual ~ObstacleSensor();

    virtual void CollectInformation();

    void CalculateResultantForce();

	void SetAngleIncrement( double angleIncrement );
	void SetRange( double range );

    bool ObstacleDetected();
    btVector3 GetResultantForce();

private:
    bool m_obstacleDetected;

    double m_angleIncrement;
    double m_range;
    double m_forceRepellingConstant;
    double m_forceAttractionConstant;

    btVector3 m_resultantForce;

    std::vector< osgUtil::Hit > m_obstacleHits;

    osg::ref_ptr< osg::LineSegment > m_beamLineSegment;
};
}

#endif //OBSTACLE_SENSOR_H
