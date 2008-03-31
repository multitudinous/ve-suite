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

#include <osgUtil/IntersectVisitor>

// --- C/C++ Libraries --- //
#include <iostream>
#include <cmath>

using namespace bots;

const double PI = 3.14159265358979323846;

////////////////////////////////////////////////////////////////////////////////
SiteSensor::SiteSensor( bots::AgentEntity* agentEntity )
:
Sensor( agentEntity ),
mSiteInView( false ),
mCloseToSite( false ),
mAngle( 0 ), 
mAngleInc( 0.1 ),
mRange( 0 ),
mNormalizedSiteVector( 0, 0, 0 ),
mGeode( 0 ),
mGeometry( 0 ),
mVertexArray( 0 ),
mLineSegment( 0 )
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
    mLineSegment = new osg::LineSegment();
    mGeode = new osg::Geode();
    mGeometry = new osg::Geometry();
    mVertexArray = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colorArray = new osg::Vec4Array();
    osg::ref_ptr< osg::StateSet > stateset = mGeode->getOrCreateStateSet();

    //Only need 2 vertices for the line
    mVertexArray->resize( 2 );
    mGeometry->setVertexArray( mVertexArray.get() );

    colorArray->push_back( osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
    mGeometry->setColorArray( colorArray.get() );
    mGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

    mGeode->addDrawable( mGeometry.get() );

    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 1.0f );
    stateset->setAttribute( lineWidth.get() );

    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

    mGeode->setStateSet( stateset.get() );

    mAgentEntity->GetPluginDCS()->addChild( mGeode.get() );

    DisplayLine( false );
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::CollectInformation()
{
    //Get the DCSs
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS =
        mAgentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS =
        mAgentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS =
        mAgentEntity->GetTargetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();
    (*mVertexArray)[ 0 ].set( agentPosition[ 0 ],
                              agentPosition[ 1 ],
                              0.5 );
    if( targetDCS.valid() )
    {
        double* targetPosition = targetDCS->GetVETranslationArray();
        (*mVertexArray)[ 1 ].set( targetPosition[ 0 ],
                                  targetPosition[ 1 ],
                                  0.5 );
    }
    else
    {
        Rotate();
        (*mVertexArray)[ 1 ].set( agentPosition[ 0 ] + mRange * cos( mAngle ),
                                  agentPosition[ 1 ] + mRange * sin( mAngle ),
                                  0.5 );
    }

    mLineSegment->set( (*mVertexArray)[ 0 ], (*mVertexArray)[ 1 ] );

    //Reset results from last frame
    mSiteInView = false;
    mCloseToSite = false;
    targetDCS = NULL;
    mGeometry->dirtyDisplayList();
    mGeometry->dirtyBound();

    osgUtil::IntersectVisitor intersectVisitor;
    intersectVisitor.addLineSegment( mLineSegment.get() );
    pluginDCS->accept( intersectVisitor );

    osgUtil::IntersectVisitor::HitList hitList = intersectVisitor.getHitList( mLineSegment.get() );
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
                                              0.0 );

                        if( siteVector.length() < 1.415 )//sqrt( 2 * 0.5 )
                        {
                            mCloseToSite = true;
                        }

                        mNormalizedSiteVector = siteVector.normalize();

                        mSiteInView = true;
                    }
                }
            }
        }
    }

    mAgentEntity->SetTargetDCS( targetDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::Rotate()
{
    mAngle += mAngleInc;
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::DisplayLine( bool onOff )
{
    mGeode->setNodeMask( onOff );
}
////////////////////////////////////////////////////////////////////////////////
bool SiteSensor::SiteInView()
{
    return mSiteInView;
}
////////////////////////////////////////////////////////////////////////////////
bool SiteSensor::CloseToSite()
{
    return mCloseToSite;
}
////////////////////////////////////////////////////////////////////////////////
btVector3 SiteSensor::GetNormalizedSiteVector()
{
    return mNormalizedSiteVector;
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::SetRange( double range )
{
    mRange = range;
}
////////////////////////////////////////////////////////////////////////////////

