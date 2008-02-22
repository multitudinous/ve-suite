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
m_closeToBlock( false ),
m_angle( 0 ), 
m_angleInc( 0.1 ),
m_range( 4 ),
m_normalizedBlockVector( 0, 0, 0 ),
m_line( new osg::Geometry() ),
m_beamGeode( new osg::Geode() ),
m_beamLineSegment( new osg::LineSegment() )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
BlockSensor::~BlockSensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::Initialize()
{
    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array();
    osg::ref_ptr< osg::Vec3Array > lineNormals = new osg::Vec3Array();
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    colors->push_back( osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    m_line->setColorArray( colors.get() );
    m_line->setColorBinding( osg::Geometry::BIND_OVERALL );

    lineNormals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
    m_line->setNormalArray( lineNormals.get() );
    m_line->setNormalBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 2.0f );
    stateset->setAttribute( lineWidth.get() );
    m_line->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::CollectInformation()
{
    if( !m_blockInView )
    {
        m_angle += m_angleInc;
    }

    //Reset results from last frame
    m_blockInView = false;
    m_closeToBlock = false;

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
                if( colorArray->at( 0 ).r() == 1.0 &&
                    colorArray->at( 0 ).g() == 1.0 &&
                    colorArray->at( 0 ).b() == 1.0 )
                {
                    //std::cout << "Sensor Found Block!" << std::endl;

                    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor( geode.get() );
                    osg::ref_ptr< osg::Node > parentNode = parentVisitor.GetParentNode();
                    if( parentNode.valid() )
                    {
                        double* blockPosition = static_cast< ves::xplorer::scenegraph::DCS* >
                            ( parentNode.get() )->GetVETranslationArray();
                        
                        osg::ref_ptr< ves::xplorer::scenegraph::DCS > tempDCS = 
                            static_cast< ves::xplorer::scenegraph::DCS* >( parentNode.get() );
                        m_agentEntity->SetTargetDCS( tempDCS.get() );

                        btVector3 blockVector( blockPosition[ 0 ] - agentPosition[ 0 ],
                                               blockPosition[ 1 ] - agentPosition[ 1 ],
                                               0 );

                        if( blockVector.length() < 1.415 )//sqrt( 2 * blockScale )
                        {
                            m_closeToBlock = true;
                        }

                        m_normalizedBlockVector = blockVector.normalize();

                        m_blockInView = true;
                    }
                }
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = m_agentEntity->GetPluginDCS();

    if( m_beamGeode.valid() )
    {
        m_line->removePrimitiveSet( 0 );
        m_beamGeode->removeDrawable( m_line.get() );
        pluginDCS->removeChild( m_beamGeode.get() );
    }

    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    vertices->push_back( startPoint );
    vertices->push_back( endPoint );
    m_line->setVertexArray( vertices.get() );

    m_line->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, vertices->size() ) );
    m_beamGeode->addDrawable( m_line.get() );      
    pluginDCS->addChild( m_beamGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
bool BlockSensor::BlockInView()
{
    return m_blockInView;
}
////////////////////////////////////////////////////////////////////////////////
bool BlockSensor::CloseToBlock()
{
    return m_closeToBlock;
}
////////////////////////////////////////////////////////////////////////////////
btVector3 BlockSensor::GetNormalizedBlockVector()
{
    return m_normalizedBlockVector;
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::SetRange( double range )
{
    m_range = range;
}
////////////////////////////////////////////////////////////////////////////////
