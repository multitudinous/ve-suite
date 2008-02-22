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

    bool SiteInView();
    bool CloseToSite();

    btVector3 GetNormalizedSiteVector();

    void SetRange( double range );

private:
    bool m_siteInView;
    bool m_closeToSite;

    double m_angle;
    double m_angleInc;
    double m_range;

    double* m_blockPosition;

    btVector3 m_normalizedSiteVector;

    osg::ref_ptr< osg::Geode > m_beamGeode;
    osg::ref_ptr< osg::LineSegment > m_beamLineSegment;

    osg::Vec3d startPoint;
    osg::Vec3d endPoint;
};
}

#endif //SITE_SENSOR_H
