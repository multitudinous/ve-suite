// --- My Includes --- //
#include "HoldBlockSensor.h"
#include "AgentEntity.h"

// --- VE-Suite Includes --- //


// --- OSG Includes --- //
#include <osg/Geometry>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace Construction;

////////////////////////////////////////////////////////////////////////////////
HoldBlockSensor::HoldBlockSensor( Construction::AgentEntity* agentEntity )
:
Sensor( agentEntity ),
m_holdingBlock( false ),
m_range( 1.6 ),
beamLineSegment( new osg::LineSegment() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
HoldBlockSensor::~HoldBlockSensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HoldBlockSensor::CollectInformation()
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = m_agentEntity->GetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();
    startPoint.set( agentPosition[ 0 ], agentPosition[ 1 ], agentPosition[ 2 ] );
    endPoint.set( agentPosition[ 0 ], agentPosition[ 1 ], agentPosition[ 2 ] + m_range );
    beamLineSegment->set( startPoint, endPoint );

    osgUtil::IntersectVisitor intersectVisitor;
    intersectVisitor.addLineSegment( beamLineSegment.get() );
    m_agentEntity->GetPluginDCS()->accept( intersectVisitor );

    osgUtil::IntersectVisitor::HitList hitList = intersectVisitor.getHitList( beamLineSegment.get() );

    if( hitList.size() > 2 )
    {
        //Get the next hit excluding the agent itself
        osgUtil::Hit firstHit = hitList.at( 2 );

        osg::ref_ptr< osg::Geode > geode = firstHit.getGeode();

        if( geode.valid() )
        {
            osg::ref_ptr< osg::Vec4Array > color_array = static_cast< osg::Vec4Array* >
                ( geode->getDrawable( 0 )->asGeometry()->getColorArray() );

            if( color_array.valid() )
            {
                if( color_array->at( 0 ).r() == 1.0 &&
                    color_array->at( 0 ).g() == 1.0 &&
                    color_array->at( 0 ).b() == 1.0 )
                {
                    m_holdingBlock = true;
                }
            }
        }
    }

}
////////////////////////////////////////////////////////////////////////////////
bool HoldBlockSensor::HoldingBlock()
{
    return m_holdingBlock;
}
////////////////////////////////////////////////////////////////////////////////