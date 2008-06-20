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
#include "ObstacleSensor.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>

#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>

#include <osgUtil/IntersectionVisitor>

// --- C/C++ Libraries --- //
#include <iostream>
#include <cmath>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
SiteSensor::SiteSensor( bots::AgentEntity* agentEntity )
    :
    Sensor( agentEntity ),
    mSiteInView( false ),
    mCloseToSite( false ),
    mAngle( 0.0 ), 
    mAngleInc( 0.1 ),
    mRange( 0.0 ),
    mNormalizedSiteVector( 0.0, 0.0, 0.0 )
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
    mLineSegmentIntersector = new osgUtil::LineSegmentIntersector(
        osg::Vec3( 0, 0, 0 ), osg::Vec3( 0, 0, 0 ) );

    mGeode = new osg::Geode();
    mGeometry = new osg::Geometry();
    mVertexArray = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colorArray = new osg::Vec4Array();

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

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    stateset->setAttribute( lineWidth.get() );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    mGeode->setStateSet( stateset.get() );

    mAgentEntity->GetPluginDCS()->addChild( mGeode.get() );

    DisplayGeometry( false );
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

    (*mVertexArray)[ 0 ] = agentDCS->getPosition();

    if( targetDCS.valid() )
    {
        (*mVertexArray)[ 1 ] = targetDCS->getPosition();
        (*mVertexArray)[ 1 ].z() = (*mVertexArray)[ 0 ].z();
    }
    else
    {
        Rotate();
        (*mVertexArray)[ 1 ] = (*mVertexArray)[ 0 ];
        (*mVertexArray)[ 1 ].x() += mRange * cos( mAngle );
        (*mVertexArray)[ 1 ].y() += mRange * sin( mAngle );
    }

    //Reset results from last frame
    mSiteInView = false;
    mCloseToSite = false;
    mNormalizedSiteVector.setValue( 0.0, 0.0, 0.0 );

    if( mGeode->getNodeMask() )
    {
        mGeometry->dirtyDisplayList();
        mGeometry->dirtyBound();
    }

    mLineSegmentIntersector->reset();
    mLineSegmentIntersector->setStart( (*mVertexArray)[ 0 ] );
    mLineSegmentIntersector->setEnd( (*mVertexArray)[ 1 ] );

    osgUtil::IntersectionVisitor intersectionVisitor(
        mLineSegmentIntersector.get() );
    pluginDCS->RemoveChild( agentDCS.get() );
    pluginDCS->accept( intersectionVisitor );
    pluginDCS->AddChild( agentDCS.get() );

    bool goToSite( false );
    if( mLineSegmentIntersector->containsIntersections() )
    {
        osg::ref_ptr< osg::Drawable > drawable =
            mLineSegmentIntersector->getFirstIntersection().drawable;

        osg::Vec4 color = static_cast< osg::Vec4Array* >(
            drawable->asGeometry()->getColorArray() )->at( 0 );
        if( color.length() == 1.0 )
        {
            mSiteInView = true;

            if( !targetDCS.valid() )
            {
                ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
                    drawable->getParent( 0 ) );
                targetDCS = static_cast< ves::xplorer::scenegraph::DCS* >(
                    parentVisitor.GetParentNode() );

                mAgentEntity->SetTargetDCS( targetDCS.get() );

                goToSite = true;
            }
        }
    }

    double* sitePosition( 0 );
    btVector3 siteVector( 0.0, 0.0, 0.0 );
    if( targetDCS.valid() )
    {
        sitePosition = targetDCS->GetVETranslationArray();
        siteVector.setValue(
            sitePosition[ 0 ] - (*mVertexArray)[ 0 ].x(),
            sitePosition[ 1 ] - (*mVertexArray)[ 0 ].y(), 0.0 );
        if( siteVector.length() <= 3.0 )
        {
            mCloseToSite = true;

            double normalizeDistance = siteVector.length() / 3.0;
            //std::cout << normalizeDistance << std::endl;

            double forceAttractionConstant = 1 - 4.0 * log( normalizeDistance );
            mAgentEntity->mObstacleSensor->SetForceAttractionConstant(
                forceAttractionConstant );
            //std::cout << forceAttractionConstant << std::endl;
        }
    }

    mNormalizedSiteVector = siteVector.normalize();

    if( goToSite )
    {
        mAgentEntity->GetPhysicsRigidBody()->setLinearVelocity(
            mNormalizedSiteVector * mAgentEntity->mMaxSpeed );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::Rotate()
{
    mAngle += mAngleInc;
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
const btVector3& SiteSensor::GetNormalizedSiteVector()
{
    return mNormalizedSiteVector;
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::SetRange( double range )
{
    mRange = range;
}
////////////////////////////////////////////////////////////////////////////////
