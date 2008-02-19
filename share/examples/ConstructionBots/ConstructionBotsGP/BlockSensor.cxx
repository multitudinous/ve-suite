// --- My Includes --- //
#include "BlockSensor.h"
#include "AgentEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/LineWidth>

// --- C/C++ Libraries --- //
#include <iostream>
#include <cmath>

using namespace Construction;

const double PI = 3.14159265358979323846;

////////////////////////////////////////////////////////////////////////////////
BlockSensor::BlockSensor( Construction::AgentEntity* agentEntity )
:
Sensor( agentEntity ),
m_blockInView( false ),
m_angle( 0 ), 
m_angleInc( 0.1 ),
m_range( 10 ),
m_blockDistance( 0 ),
m_normalizedBlockVector( 0, 0, 0 ),
beamGeode( 0 ),
beamLineSegment( new osg::LineSegment() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
BlockSensor::~BlockSensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::CollectInformation()
{
    if( !m_blockInView )
    {
        m_angle += m_angleInc;
    }

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = m_agentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = m_agentEntity->GetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();

    osg::Vec3d startPoint( agentPosition[ 0 ], agentPosition[ 1 ], agentPosition[ 2 ] );
    osg::Vec3d endPoint( agentPosition[ 0 ] + m_range * cos( m_angle ), agentPosition[ 1 ] + m_range * sin( m_angle ), agentPosition[ 2 ] );

    beamLineSegment->set( startPoint, endPoint );

    DrawLine( startPoint, endPoint );

    osgUtil::IntersectVisitor intersectVisitor;
    intersectVisitor.addLineSegment( beamLineSegment.get() );

    pluginDCS->accept( intersectVisitor );

    osgUtil::IntersectVisitor::HitList hitList = intersectVisitor.getHitList( beamLineSegment.get() );

    if( hitList.size() > 1 )
    {
        //Get the next hit excluding the agent itself
        osgUtil::Hit firstHit = hitList.at( 1 );

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
                    //std::cout << "Sensor Found Block!" << std::endl;

                    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor( geode.get() );
                    osg::ref_ptr< osg::Node > parentNode = parentVisitor.GetParentNode();
                    if( parentNode.valid() )
                    {
                        m_blockPosition = 
                            static_cast< ves::xplorer::scenegraph::DCS* >( parentNode.get() )->GetVETranslationArray();
                    }

					osg::ref_ptr< ves::xplorer::scenegraph::DCS > tempDCS = static_cast< ves::xplorer::scenegraph::DCS* >( parentNode.get() );
					m_agentEntity->SetTargetDCS( tempDCS.get() );

                    btVector3 temp( m_blockPosition[ 0 ] - agentPosition[ 0 ], m_blockPosition[ 1 ] - agentPosition[ 1 ], 0 );

                    m_blockDistance = temp.length();

                    m_normalizedBlockVector = temp.normalize();

                    m_blockInView = true;

                    if( beamGeode.valid() )
                    {
                        //pluginDCS->removeChild( beamGeode.get() );
                    }
                }
                else
                {
                    m_blockInView = false; 
                }
            }
        }
    }
    else
    {
        m_blockInView = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = m_agentEntity->GetPluginDCS();

    if( beamGeode.valid() )
    {
        pluginDCS->removeChild( beamGeode.get() );
    }

    beamGeode = new osg::Geode();
    beamGeode->setName( "Optical Sensor" );

    osg::ref_ptr< osg::Geometry > line = new osg::Geometry();
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array();
    osg::ref_ptr< osg::Vec3Array > lineNormals = new osg::Vec3Array();
    osg::ref_ptr< osg::StateSet > m_stateset = new osg::StateSet();

    vertices->push_back( startPoint );
    vertices->push_back( endPoint );
    line->setVertexArray( vertices.get() );

    colors->push_back( osg::Vec4( 0.0f, 1.0f, 1.0f, 1.0f ) );
    line->setColorArray( colors.get() );
    line->setColorBinding( osg::Geometry::BIND_OVERALL );

    lineNormals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
    line->setNormalArray( lineNormals.get() );
    line->setNormalBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::LineWidth > line_width = new osg::LineWidth();
    line_width->setWidth( 1.0f );
    m_stateset->setAttribute( line_width.get() );
    line->setStateSet( m_stateset.get() );

    line->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, vertices->size() ) );

    beamGeode->addDrawable( line.get() );      

    pluginDCS->addChild( beamGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
bool BlockSensor::BlockInView()
{
    return m_blockInView;
}
////////////////////////////////////////////////////////////////////////////////
double BlockSensor::GetBlockDistance()
{
    return m_blockDistance;
}
////////////////////////////////////////////////////////////////////////////////
double* BlockSensor::GetBlockPosition()
{
    return m_blockPosition;
}
////////////////////////////////////////////////////////////////////////////////
btVector3 BlockSensor::GetNormalizedBlockVector()
{
    return m_normalizedBlockVector;
}
////////////////////////////////////////////////////////////////////////////////
