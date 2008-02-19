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

typedef std::pair< double, double > Pair;

const double PI = 3.14159265358979323846;

////////////////////////////////////////////////////////////////////////////////
ObstacleSensor::ObstacleSensor( Construction::AgentEntity* agentEntity )
:
Sensor( agentEntity ),
m_obstacleDetected( false ),
m_angleIncrement( 30 ),
m_range( 5.0 ),
m_forceRepellingConstant( 1.0 ),
m_forceAttractionConstant( 2.0 ),
m_resultantForce( 0, 0, 0 ),
m_obstacleHits()
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
	std::vector< osgUtil::Hit > obstacleHits;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = m_agentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = m_agentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS = m_agentEntity->GetTargetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();

	osg::ref_ptr< osg::LineSegment > m_beamLineSegment = new osg::LineSegment();

    for( double angle = 0; angle < 360; )
    {
        osg::Vec3d startPoint( agentPosition[ 0 ], agentPosition[ 1 ], agentPosition[ 2 ] );
        osg::Vec3d endPoint( agentPosition[ 0 ] + m_range * cos( angle ), 
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
            obstacleHits.push_back( hitList.at( 1 ) );
        }

        angle += m_angleIncrement;
    }

	if( !obstacleHits.empty() )
	{
		m_obstacleDetected = true;
	}
	else
	{
		m_obstacleDetected = false;
	}

	m_obstacleHits = obstacleHits;
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::CalculateResultantForce()
{
    Pair totalForce( 0, 0 );

    //osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = m_agentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = m_agentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS = m_agentEntity->GetTargetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();
    
    //Calculate the target force
    if( targetDCS.valid() )
    {
        double* targetPosition = targetDCS->GetVETranslationArray();
        btVector3 targetVector( targetPosition[ 0 ] - agentPosition[ 0 ], targetPosition[ 1 ] - agentPosition[ 1 ], 0 );
        double targetDistance = targetVector.length();
        Pair targetForce( m_forceAttractionConstant * targetVector[ 0 ] / targetDistance, 
                          m_forceAttractionConstant * targetVector[ 1 ] / targetDistance );

        totalForce.first += targetForce.first;
        totalForce.second += targetForce.second;
    }

    //Calculate the total repulsive force
    for( size_t i = 0; i < m_obstacleHits.size(); ++i )
    {
        osg::ref_ptr< osg::Geode > geode = m_obstacleHits.at( i ).getGeode();

        if( geode.valid() )
        {
            osg::Vec3d intersect = m_obstacleHits.at( i ).getWorldIntersectPoint();
            btVector3 hitVector( intersect.x() - agentPosition[ 0 ], intersect.y() - agentPosition[ 1 ], 0 );
            double distance = hitVector.length();
            double distanceSquared = hitVector.length2();

            double variables = m_forceRepellingConstant / distanceSquared;
            Pair repulsiveForce( variables * hitVector[ 0 ] / distance, variables * hitVector[ 1 ] / distance );

            totalForce.first -= repulsiveForce.first;
            totalForce.second -= repulsiveForce.second;
        }
    }

    m_obstacleHits.clear();

    btVector3 temp( totalForce.first, totalForce.second, 0 );

    m_resultantForce = temp.normalize();
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
