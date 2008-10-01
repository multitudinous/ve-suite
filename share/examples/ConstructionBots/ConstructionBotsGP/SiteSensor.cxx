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
#include "PerimeterSensor.h"
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
#include <osgUtil/LineSegmentIntersector>

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
    mAngleInc( 0.0 ),
    mAnglePerFrame( 0.2 ),
    mAngleLeftover( 0.0 ),
    mRotationsPerFrame( 0.0 ),
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
        osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) );
    mGeode = new osg::Geode();
    mGeometry = new osg::Geometry();
    mVertexArray = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colorArray = new osg::Vec4Array();

    mVertexArray->resize( 2 );
    mGeometry->setVertexArray( mVertexArray.get() );

    colorArray->push_back( mAgentEntity->mSiteColor );
    mGeometry->setColorArray( colorArray.get() );
    mGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

    mGeode->addDrawable( mGeometry.get() );

    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 1.0 );

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    stateset->setAttribute( lineWidth.get() );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    mGeode->setStateSet( stateset.get() );

    mAgentEntity->GetPluginDCS()->addChild( mGeode.get() );

    DisplayGeometry( false );

    //Calculate the min angle needed to hit the site at max length for gridSize
    unsigned int gridSize = 51;
    //Assumes site is at the center of the grid
    double maxLength = sqrt( 2.0 ) * 0.5 * gridSize;
    double maxLengthOfBlock = sqrt( 2.0 );
    mAngleInc = acos( 1.0 - ( pow( maxLengthOfBlock, 2.0 ) /
                            ( 2.0 * pow( maxLength, 2.0 ) ) ) );

    double rotationsPerFrame = mAnglePerFrame / mAngleInc;
    mRotationsPerFrame = static_cast< unsigned int >( rotationsPerFrame );
    mAngleLeftover = mAngleInc * ( rotationsPerFrame - mRotationsPerFrame );
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::CollectInformation()
{
    //Get the DCSs
    ves::xplorer::scenegraph::DCS* const pluginDCS =
        mAgentEntity->GetPluginDCS();
    ves::xplorer::scenegraph::DCS* const agentDCS =
        mAgentEntity->GetDCS();
    ves::xplorer::scenegraph::DCS* targetDCS =
        mAgentEntity->GetTargetDCS();

    (*mVertexArray)[ 0 ] = agentDCS->getPosition();

    bool checkTargetLine( false );
    if( targetDCS )
    {
        (*mVertexArray)[ 1 ] = targetDCS->getPosition();
        (*mVertexArray)[ 1 ].z() = (*mVertexArray)[ 0 ].z();
        checkTargetLine = true;
    }
    else
    {
        Rotate( true );
    }

    //Reset results from last frame
    //targetDCS = NULL;
    mSiteInView = false;
    mCloseToSite = false;

    if( mGeode->getNodeMask() )
    {
        mGeometry->dirtyDisplayList();
        mGeometry->dirtyBound();
    }

    pluginDCS->RemoveChild( agentDCS );

    int rotations( 0 );
    bool firstTimeThrough( true );
    do
    {
        if( !firstTimeThrough )
        {
            if( checkTargetLine )
            {
                Rotate( true );
                checkTargetLine = false;
            }
            else
            {
                Rotate();
                ++rotations;
            }
        }
        else
        {
            firstTimeThrough = false;
        }

        mLineSegmentIntersector->reset();
        mLineSegmentIntersector->setStart( (*mVertexArray)[ 0 ] );
        mLineSegmentIntersector->setEnd( (*mVertexArray)[ 1 ] );

        osgUtil::IntersectionVisitor intersectionVisitor(
            mLineSegmentIntersector.get() );
        pluginDCS->accept( intersectionVisitor );

        if( mLineSegmentIntersector->containsIntersections() )
        {
            osg::Drawable* drawable =
                mLineSegmentIntersector->getFirstIntersection().drawable.get();
            osg::Array* tempArray = drawable->asGeometry()->getColorArray();
            if( tempArray )
            {
                const osg::Vec4& color =
                    static_cast< osg::Vec4Array* >( tempArray )->at( 0 );
                if( color == mAgentEntity->mSiteColor )
                {
                    mSiteInView = true;
                    
                    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
                        drawable->getParent( 0 ) );
                    targetDCS = static_cast< ves::xplorer::scenegraph::DCS* >(
                        parentVisitor.GetParentNode() );

                    break;
                }
            }
        }

        if( targetDCS )
        {
            break;
        }
    }
    while( rotations < static_cast< int >( mRotationsPerFrame ) );

    pluginDCS->AddChild( agentDCS );

    mAgentEntity->SetTargetDCS( targetDCS );

    btVector3 siteVector( 0.0, 0.0, 0.0 );
    if( targetDCS )
    {
        double* sitePosition = targetDCS->GetVETranslationArray();
        siteVector.setValue(
            sitePosition[ 0 ] - (*mVertexArray)[ 0 ].x(),
            sitePosition[ 1 ] - (*mVertexArray)[ 0 ].y(), 0.0 );
        //Use 2.0 > sqrt( 2 ) + mPerimeterSensor->Range( 0.2 ) = 1.614
        if( siteVector.length() <= 1.614 )
        {
            mCloseToSite = true;
        }

        double forceAttractionConstant =
            1.0 + ( 8.0 / pow( siteVector.length(), 2 ) );
        mAgentEntity->mObstacleSensor->SetForceAttractionConstant(
            forceAttractionConstant );
    }

    mNormalizedSiteVector.setValue( 0.0, 0.0, 0.0 );
    if( siteVector.length() != 0.0 )
    {
        mNormalizedSiteVector = siteVector.normalize();
    }
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::Rotate( bool leftover )
{
    if( !leftover )
    {
        mAngle += mAngleInc;
    }
    else
    {
        mAngle += mAngleLeftover;
    }

    (*mVertexArray)[ 1 ] = (*mVertexArray)[ 0 ];
    (*mVertexArray)[ 1 ].x() += mRange * cos( mAngle );
    (*mVertexArray)[ 1 ].y() += mRange * sin( mAngle );
}
////////////////////////////////////////////////////////////////////////////////
const bool SiteSensor::SiteInView() const
{
    return mSiteInView;
}
////////////////////////////////////////////////////////////////////////////////
const bool SiteSensor::CloseToSite() const
{
    return mCloseToSite;
}
////////////////////////////////////////////////////////////////////////////////
const btVector3& SiteSensor::GetNormalizedSiteVector() const
{
    return mNormalizedSiteVector;
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::Reset()
{
    mAngle = 90 * ( rand() % 4 );
    mAngle = osg::DegreesToRadians( mAngle );

    mSiteInView = false;
}
////////////////////////////////////////////////////////////////////////////////
void SiteSensor::SetRange( double range )
{
    mRange = range;
}
////////////////////////////////////////////////////////////////////////////////
