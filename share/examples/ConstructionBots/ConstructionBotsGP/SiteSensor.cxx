// --- My Includes --- //
#include "SiteSensor.h"
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
SiteSensor::SiteSensor( Construction::AgentEntity* agentEntity )
:
Sensor( agentEntity ),
m_siteInView( false ),
m_closeToSite( false ),
m_angle( 0 ), 
m_angleInc( 0.1 ),
m_range( 4 ),
m_normalizedSiteVector( 0, 0, 0 ),
m_beamGeode( 0 ),
m_beamLineSegment( new osg::LineSegment() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SiteSensor::~SiteSensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::CollectInformation()
{
    if( !m_siteInView )
    {
        m_angle += m_angleInc;
    }

    //Reset results from last frame
    m_siteInView = false;
    m_closeToSite = false;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = m_agentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = m_agentEntity->GetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();

    osg::Vec3d startPoint( agentPosition[ 0 ],
                           agentPosition[ 1 ],
                           agentPosition[ 2 ] );
    osg::Vec3d endPoint( agentPosition[ 0 ] + m_range * cos( m_angle ),
                         agentPosition[ 1 ] + m_range * sin( m_angle ),
                         agentPosition[ 2 ] );

    m_beamLineSegment->set( startPoint, endPoint );

    DrawLine( startPoint, endPoint );

    osgUtil::IntersectVisitor intersectVisitor;
    intersectVisitor.addLineSegment( m_beamLineSegment.get() );

    pluginDCS->accept( intersectVisitor );

    osgUtil::IntersectVisitor::HitList hitList = intersectVisitor.getHitList( m_beamLineSegment.get() );

    if( hitList.size() > 2 )
    {
        //Get the next hit excluding the agent itself
        osgUtil::Hit firstHit = hitList.at( 2 );

        osg::ref_ptr< osg::Geode > geode = firstHit.getGeode();

        if( geode.valid() )
        {
            osg::ref_ptr< osg::Vec4Array > colorArray = static_cast< osg::Vec4Array* >
                ( geode->getDrawable( 0 )->asGeometry()->getColorArray() );

            if( colorArray.valid() )
            {
                if( colorArray->at( 0 ).r() == 0.0 &&
                    colorArray->at( 0 ).g() == 0.0 &&
                    colorArray->at( 0 ).b() == 0.0 )
                {
                    //std::cout << "Sensor Found Site!" << std::endl;

                    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor( geode.get() );
                    osg::ref_ptr< osg::Node > parentNode = parentVisitor.GetParentNode();
                    if( parentNode.valid() )
                    {
                        double* sitePosition = static_cast< ves::xplorer::scenegraph::DCS* >
                            ( parentNode.get() )->GetVETranslationArray();

                        osg::ref_ptr< ves::xplorer::scenegraph::DCS > tempDCS = 
                            static_cast< ves::xplorer::scenegraph::DCS* >( parentNode.get() );
                        m_agentEntity->SetTargetDCS( tempDCS.get() );

                        btVector3 siteVector( sitePosition[ 0 ] - agentPosition[ 0 ],
                                              sitePosition[ 1 ] - agentPosition[ 1 ],
                                              0 );

                        if( siteVector.length() < 1.415 )//sqrt( 2 * blockScale )
                        {
                            m_closeToSite = true;
                        }

                        m_normalizedSiteVector = siteVector.normalize();

                        m_siteInView = true;
                    }
                }
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = m_agentEntity->GetPluginDCS();

    if( m_beamGeode.valid() )
    {
        pluginDCS->removeChild( m_beamGeode.get() );
    }

    m_beamGeode = new osg::Geode();
    m_beamGeode->setName( "Optical Sensor" );

    osg::ref_ptr< osg::Geometry > line = new osg::Geometry();
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array();
    osg::ref_ptr< osg::Vec3Array > lineNormals = new osg::Vec3Array();
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    vertices->push_back( startPoint );
    vertices->push_back( endPoint );
    line->setVertexArray( vertices.get() );

    colors->push_back( osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
    line->setColorArray( colors.get() );
    line->setColorBinding( osg::Geometry::BIND_OVERALL );

    lineNormals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
    line->setNormalArray( lineNormals.get() );
    line->setNormalBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::LineWidth > line_width = new osg::LineWidth();
    line_width->setWidth( 2.0f );
    stateset->setAttribute( line_width.get() );
    line->setStateSet( stateset.get() );

    line->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, vertices->size() ) );

    m_beamGeode->addDrawable( line.get() );      

    pluginDCS->addChild( m_beamGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
bool SiteSensor::SiteInView()
{
    return m_siteInView;
}
////////////////////////////////////////////////////////////////////////////////
bool SiteSensor::CloseToSite()
{
    return m_closeToSite;
}
////////////////////////////////////////////////////////////////////////////////
btVector3 SiteSensor::GetNormalizedSiteVector()
{
    return m_normalizedSiteVector;
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::SetRange( double range )
{
    m_range = range;
}
////////////////////////////////////////////////////////////////////////////////
