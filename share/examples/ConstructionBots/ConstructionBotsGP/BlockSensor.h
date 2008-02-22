#ifndef BLOCK_SENSOR_H
#define BLOCK_SENSOR_H

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
class BlockSensor : public Sensor
{
public:
    BlockSensor( Construction::AgentEntity* agentEntity );

    virtual ~BlockSensor();

    virtual void CollectInformation();

    void DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint );

    bool BlockInView();
    bool CloseToBlock();

    btVector3 GetNormalizedBlockVector();

    void SetRange( double range );

private:
    void Initialize();

    bool m_blockInView;
    bool m_closeToBlock;

    double m_angle;
    double m_angleInc;
    double m_range;

    btVector3 m_normalizedBlockVector;

    osg::ref_ptr< osg::Geometry > m_line;
    osg::ref_ptr< osg::Geode > m_beamGeode;
    osg::ref_ptr< osg::LineSegment > m_beamLineSegment;
};
}

#endif //BLOCK_SENSOR_H
