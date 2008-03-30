/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

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

using namespace bots;

const double PI = 3.14159265358979323846;

////////////////////////////////////////////////////////////////////////////////
SiteSensor::SiteSensor( bots::AgentEntity* agentEntity )
:
Sensor( agentEntity ),
m_siteInView( false ),
m_closeToSite( false ),
m_angle( 0 ), 
m_angleInc( 0.1 ),
m_range( 0 ),
m_normalizedSiteVector( 0, 0, 0 ),
m_line( new osg::Geometry() ),
m_beamGeode( new osg::Geode() ),
m_beamLineSegment( new osg::LineSegment() )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
SiteSensor::~SiteSensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::Initialize()
{
    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array();
    osg::ref_ptr< osg::Vec3Array > lineNormals = new osg::Vec3Array();
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    colors->push_back( osg::Vec4( 0.0f, 1.0f, 1.0f, 1.0f ) );
    m_line->setColorArray( colors.get() );
    m_line->setColorBinding( osg::Geometry::BIND_OVERALL );

    lineNormals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
    m_line->setNormalArray( lineNormals.get() );
    m_line->setNormalBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 1.0f );
    stateset->setAttribute( lineWidth.get() );
    m_line->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::CollectInformation()
{
    //Get the DCSs
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = m_agentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = m_agentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS = m_agentEntity->GetTargetDCS();

    osg::Vec3d startPoint, endPoint;
    double* agentPosition = agentDCS->GetVETranslationArray();
    startPoint.set( agentPosition[ 0 ],
                    agentPosition[ 1 ],
                    agentPosition[ 2 ] );
    if( targetDCS.valid() )
    {
        double* targetPosition = targetDCS->GetVETranslationArray();
        endPoint.set( targetPosition[ 0 ],
                      targetPosition[ 1 ],
                      0.5 );
    }
    else
    {
        Rotate();
        endPoint.set( agentPosition[ 0 ] + m_range * cos( m_angle ),
                      agentPosition[ 1 ] + m_range * sin( m_angle ),
                      agentPosition[ 2 ] );
    }

    //Reset results from last frame
    m_siteInView = false;
    m_closeToSite = false;
    targetDCS = NULL;

    m_beamLineSegment->set( startPoint, endPoint );
    DrawLine( startPoint, endPoint );

    osgUtil::IntersectVisitor intersectVisitor;
    intersectVisitor.addLineSegment( m_beamLineSegment.get() );
    pluginDCS->accept( intersectVisitor );

    osgUtil::IntersectVisitor::HitList hitList = intersectVisitor.getHitList( m_beamLineSegment.get() );
    if( hitList.size() > 1 )
    {
        //Get the next hit excluding the agent itself
        osgUtil::Hit firstHit = hitList.at( 1 );

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
                    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor( geode.get() );
                    targetDCS = static_cast< ves::xplorer::scenegraph::DCS* >( parentVisitor.GetParentNode() );
                    if( targetDCS.valid() )
                    {
                        double* sitePosition = targetDCS->GetVETranslationArray();
                        btVector3 siteVector( sitePosition[ 0 ] - agentPosition[ 0 ],
                                              sitePosition[ 1 ] - agentPosition[ 1 ],
                                              0 );

                        if( siteVector.length() < 1.415 )//sqrt( 2 * 0.5 )
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

    m_agentEntity->SetTargetDCS( targetDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::Rotate()
{
    m_angle += m_angleInc;
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = m_agentEntity->GetPluginDCS();

    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    vertices->push_back( startPoint );
    vertices->push_back( endPoint );
    m_line->setVertexArray( vertices.get() );

    m_line->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, vertices->size() ) );
    m_beamGeode->addDrawable( m_line.get() );      
    pluginDCS->addChild( m_beamGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::RemoveLine()
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = m_agentEntity->GetPluginDCS();
    unsigned int numPrimitives = m_line->getNumPrimitiveSets();
    if( numPrimitives == 1 )
    {
        m_line->removePrimitiveSet( 0 );
    }
    m_beamGeode->removeDrawable( m_line.get() );
    pluginDCS->removeChild( m_beamGeode.get() );
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

