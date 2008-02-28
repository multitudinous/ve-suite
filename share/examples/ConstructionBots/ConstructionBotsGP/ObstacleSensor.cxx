// --- My Includes --- //
#include "ObstacleSensor.h"
#include "AgentEntity.h"

// --- VE-Suite Includes --- //

// --- OSG Includes --- //
#include <osg/Geometry>

#include <osgUtil/IntersectVisitor>

// --- C/C++ Libraries --- //
#include <vector>
#include <cmath>

using namespace Construction;

const double PI = 3.14159265358979323846;

////////////////////////////////////////////////////////////////////////////////
ObstacleSensor::ObstacleSensor( Construction::AgentEntity* agentEntity )
:
Sensor( agentEntity ),
m_obstacleDetected( false ),
m_angleIncrement( 30 ),
m_range( 2.0 ),
m_forceRepellingConstant( 1.0 ),
m_forceAttractionConstant( 1.0 ),
m_resultantForce( 0, 0, 0 ),
m_obstacleHits(),
m_beamLineSegment( new osg::LineSegment() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ObstacleSensor::~ObstacleSensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::CollectInformation()
{
    //Get the DCSs
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = m_agentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = m_agentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS = m_agentEntity->GetTargetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();

    //Reset results from last frame
    m_obstacleHits.clear();

    osg::Vec3d startPoint, endPoint;
    startPoint.set( agentPosition[ 0 ], agentPosition[ 1 ], agentPosition[ 2 ] );
    for( double angle = 0; angle < 360; )
    {
        endPoint.set( agentPosition[ 0 ] + m_range * cos( angle ), 
                      agentPosition[ 1 ] + m_range * sin( angle ), 
                      agentPosition[ 2 ] );

        m_beamLineSegment->set( startPoint, endPoint );

        osgUtil::IntersectVisitor intersectVisitor;
        intersectVisitor.addLineSegment( m_beamLineSegment.get() );

        pluginDCS->RemoveChild( targetDCS.get() );
        pluginDCS->accept( intersectVisitor );
        pluginDCS->AddChild( targetDCS.get() );

        osgUtil::IntersectVisitor::HitList hitList = intersectVisitor.getHitList( m_beamLineSegment.get() );

        if( hitList.size() > 1 )
        {
            //Get the next hit excluding the agent itself
            m_obstacleHits.push_back( hitList.at( 1 ) );
        }

        angle += m_angleIncrement;
    }

	if( !m_obstacleHits.empty() )
	{
		m_obstacleDetected = true;
	}
	else
	{
		m_obstacleDetected = false;
	}
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::CalculateResultantForce( bool buildMode )
{
    if( buildMode )
    {
        WallFollowing();
    }
    else
    {
        VirtualForceField();
    }
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::VirtualForceField()
{
    // ------------------ Virtual Force Field (VFF) Method ------------------ //
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = m_agentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS = m_agentEntity->GetTargetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();
    
    btVector3 totalForce( 0, 0, 0 );

    //Calculate the target force
    if( targetDCS.valid() )
    {
        double* targetPosition = targetDCS->GetVETranslationArray();
        btVector3 targetForce( targetPosition[ 0 ] - agentPosition[ 0 ],
                               targetPosition[ 1 ] - agentPosition[ 1 ], 0 );

        targetForce /= targetForce.length();
        targetForce *= m_forceAttractionConstant;

        totalForce += targetForce;
    }

    //Calculate the total repulsive force
    for( size_t i = 0; i < m_obstacleHits.size(); ++i )
    {
        osg::ref_ptr< osg::Geode > geode = m_obstacleHits.at( i ).getGeode();

        if( geode.valid() )
        {
            osg::Vec3d intersect = m_obstacleHits.at( i ).getWorldIntersectPoint();
            btVector3 repulsiveForce( intersect.x() - agentPosition[ 0 ],
                                      intersect.y() - agentPosition[ 1 ], 0 );

            double variables = m_forceRepellingConstant / repulsiveForce.length2();
            repulsiveForce /= repulsiveForce.length();
            repulsiveForce *= variables;

            totalForce -= repulsiveForce;
        }
    }

    m_resultantForce = totalForce.normalize();
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::WallFollowing()
{
    // ----------------------- Wall Following Method ------------------------ //
    // x' = x * cos( theta ) - y * sin( theta );
    // y' = x * sin( theta ) + y * cos( theta );
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::SetAngleIncrement( double angleIncrement )
{
	m_angleIncrement = angleIncrement;
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::SetRange( double range )
{
	m_range = range;
}
////////////////////////////////////////////////////////////////////////////////
bool ObstacleSensor::ObstacleDetected()
{
    return m_obstacleDetected;
}
////////////////////////////////////////////////////////////////////////////////
btVector3 ObstacleSensor::GetResultantForce()
{
    return m_resultantForce;
}
////////////////////////////////////////////////////////////////////////////////
