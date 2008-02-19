#ifndef SITE_SENSOR_H
#define SITE_SENSOR_H

// --- My Includes --- //
#include "Sensor.h"

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/LineSegment>

#include <osgUtil/IntersectVisitor>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

// --- C/C++ Libraries --- //
#include <vector>

//Simulates an optical type sensor
namespace Construction
{
class SiteSensor : public Sensor
{
public:
    SiteSensor( Construction::AgentEntity* agentEntity );

    virtual ~SiteSensor();

    virtual void CollectInformation();

    void DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint );

    bool BlockInView();
    double GetBlockDistance();
    double* GetBlockPosition();
    btVector3 GetNormalizedBlockVector();

private:
    bool m_blockInView;

    double m_angle;
    double m_angleInc;
    double m_range;
    double m_blockDistance;

    double* m_blockPosition;

    btVector3 m_normalizedBlockVector;

    osg::ref_ptr< osg::Geode > beamGeode;
    osg::ref_ptr< osg::LineSegment > beamLineSegment;

    osg::Vec3d startPoint;
    osg::Vec3d endPoint;
};
}

#endif //SITE_SENSOR_H
