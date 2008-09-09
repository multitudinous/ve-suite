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
#include "PerimeterSensor.h"
#include "AgentEntity.h"
#include "BlockEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>

#include <osgUtil/IntersectionVisitor>
#include <osgUtil/LineSegmentIntersector>

// --- c/C++ Includes --- //
#include <iostream>

using namespace bots;

const double piDivOneEighty = 0.0174532925;

////////////////////////////////////////////////////////////////////////////////
PerimeterSensor::PerimeterSensor( bots::AgentEntity* agentEntity )
    :
    Sensor( agentEntity ),
    mAligned( false ),
    mPerimeterDetected( false ),
    mLastClockWiseDetection( 0 ),
    mPreviousDrawable( 0 ),
    mRange( 0.1 ),
    mResultantForce( 0, 0, 0 ),
    mQueriedConnection( 0 ),
    mIntersectorGroup( 0 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
PerimeterSensor::~PerimeterSensor()
{
    if( mLastClockWiseDetection )
    {
        delete mLastClockWiseDetection;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PerimeterSensor::Initialize()
{
    mIntersectorGroup = new osgUtil::IntersectorGroup();
    mGeode = new osg::Geode();
    mGeometry = new osg::Geometry();
    mVertexArray = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colorArray = new osg::Vec4Array();

    mVertexArray->resize( 16 );
    mGeometry->setVertexArray( mVertexArray.get() );

    colorArray->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    mGeometry->setColorArray( colorArray.get() );
    mGeometry->setColorBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES,  0, 2 ) );
    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES,  2, 2 ) );
    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES,  4, 2 ) );
    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES,  6, 2 ) );
    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES,  8, 2 ) );
    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 10, 2 ) );
    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 12, 2 ) );
    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 14, 2 ) );

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

    CalculateLocalPositions();
}
////////////////////////////////////////////////////////////////////////////////
void PerimeterSensor::CalculateLocalPositions()
{
    double blockHalfWidth = 0.5;
    osg::Vec3 rightStartPoint, rightEndPoint;
    rightStartPoint.set( blockHalfWidth, -blockHalfWidth, 0 );
    rightEndPoint.set( blockHalfWidth + mRange, -blockHalfWidth, 0 );

    osg::Vec3 leftStartPoint, leftEndPoint;
    leftStartPoint.set( blockHalfWidth, blockHalfWidth, 0 );
    leftEndPoint.set( blockHalfWidth + mRange, blockHalfWidth, 0 );

    //Rotate vector about point( 0, 0, 0 ) by theta
    //x' = x * cos( theta ) - y * sin( theta );
    //y' = x * sin( theta ) + y * cos( theta );
    for( int i = 0; i < 4; ++i )
    {
        double x, y, xNew, yNew;
        double cosTheta = cos( i * 90 * piDivOneEighty );
        double sinTheta = sin( i * 90 * piDivOneEighty );

        x = rightStartPoint.x();
        y = rightStartPoint.y();
        xNew = ( x * cosTheta ) - ( y * sinTheta );
        yNew = ( x * sinTheta ) + ( y * cosTheta );
        (*mVertexArray)[ i * 4 ] = osg::Vec3( xNew, yNew, 0 );

        x = rightEndPoint.x();
        y = rightEndPoint.y();
        xNew = ( x * cosTheta ) - ( y * sinTheta );
        yNew = ( x * sinTheta ) + ( y * cosTheta );
        (*mVertexArray)[ i * 4 + 1 ] = osg::Vec3( xNew, yNew, 0 );

        x = leftStartPoint.x();
        y = leftStartPoint.y();
        xNew = ( x * cosTheta ) - ( y * sinTheta );
        yNew = ( x * sinTheta ) + ( y * cosTheta );
        (*mVertexArray)[ i * 4 + 2 ] = osg::Vec3( xNew, yNew, 0 );

        x = leftEndPoint.x();
        y = leftEndPoint.y();
        xNew = ( x * cosTheta ) - ( y * sinTheta );
        yNew = ( x * sinTheta ) + ( y * cosTheta );
        (*mVertexArray)[ i * 4 + 3 ] = osg::Vec3( xNew, yNew, 0 );

        mGeometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, i * 4, 2 ) );
        mGeometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES,  i * 4 + 2, 2 ) );

        mIntersectorGroup->addIntersector( new osgUtil::LineSegmentIntersector(
            osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0,0.0,0.0 ) ) );
        mIntersectorGroup->addIntersector( new osgUtil::LineSegmentIntersector(
            osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0,0.0,0.0 ) ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PerimeterSensor::CollectInformation()
{
    //Get the DCSs
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS =
        mAgentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS =
        mAgentEntity->GetDCS();

    osg::Vec3 agentPosition = agentDCS->getPosition();
    osgUtil::IntersectorGroup::Intersectors& intersectors =
        mIntersectorGroup->getIntersectors();

    //Reset
    mAligned = false;
    mIntersections.clear();
    mQueriedConnection = NULL;
    bool previousSensor( false );
    for( int i = 7; i > -1; --i )
    {
        osg::Vec3 startPoint = (*mVertexArray)[ i * 2 ];
        osg::Vec3 endPoint = (*mVertexArray)[ i * 2 + 1 ];

        startPoint += agentPosition;
        endPoint += agentPosition;

        mLineSegmentIntersector =
            static_cast< osgUtil::LineSegmentIntersector* >(
                intersectors.at( i ).get() );
        mLineSegmentIntersector->reset();
        mLineSegmentIntersector->setStart( startPoint );
        mLineSegmentIntersector->setEnd( endPoint );
    }

    pluginDCS->RemoveChild( agentDCS.get() );
    //This is an expensive call
    //Try to only call once by using group intersector
    osgUtil::IntersectionVisitor intersectionVisitor( mIntersectorGroup.get() );
    pluginDCS->accept( intersectionVisitor );
    pluginDCS->AddChild( agentDCS.get() );

    for( int i = 7; i > -1; --i )
    {
        mLineSegmentIntersector =
            static_cast< osgUtil::LineSegmentIntersector* >(
                intersectors.at( i ).get() );
        if( mLineSegmentIntersector->containsIntersections() )
        {
            mIntersections.push_back(
                mLineSegmentIntersector->getFirstIntersection() );
            osg::ref_ptr< osg::Drawable > currentDrawable =
                mIntersections.back().drawable;
            osg::Array* tempArray =
                currentDrawable->asGeometry()->getColorArray();
            if( tempArray )
            {
                osg::Vec4* color =
                    &( static_cast< osg::Vec4Array* >( tempArray )->at( 0 ) );

                if( *color != mAgentEntity->mSiteColor )
                {
                    mAgentEntity->mBuildMode = false;
                    mAgentEntity->mPerimeterSensor->Reset();
                    std::cout << "PerimeterSensor: " 
                              << mAgentEntity->GetDCS()->GetName()
                              << std::endl << std::endl;
                    return;
                }
            }

            if( mLastClockWiseDetection )
            {
                unsigned int modulusTest = ( *mLastClockWiseDetection + i ) % 4;

                int cornerValue = *mLastClockWiseDetection - i;
                bool cornerTest( false );
                if( cornerValue == 1 || cornerValue == -7 )
                {
                    cornerTest = true;
                }

                bool drawableTest( false );
                if( mPreviousDrawable.valid() )
                {
                    drawableTest = ( mPreviousDrawable == currentDrawable );
                }

                if( ( modulusTest == 3 && cornerTest ) ||
                    ( modulusTest == 1 && drawableTest ) )
                {
                    mAligned = true;
                    mQueriedConnection = currentDrawable.get();
                }
            }

            if( !mLastClockWiseDetection )
            {
                mLastClockWiseDetection = new int();
            }
            *mLastClockWiseDetection = i;

            if( !mPreviousDrawable.valid() )
            {
                mPreviousDrawable = new osg::Geometry();
            }
            mPreviousDrawable = currentDrawable;

            previousSensor = true;
        }
        else
        {
            if( previousSensor )
            {
                break;
            }
        }
    }

    if( !mPerimeterDetected )
    {
        mPerimeterDetected = !mIntersections.empty();
        if( mPerimeterDetected )
        {
            mAgentEntity->InitiateBuildMode();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PerimeterSensor::Reset()
{
    delete mLastClockWiseDetection;
    mLastClockWiseDetection = NULL;
    mPreviousDrawable = NULL;

    mPerimeterDetected = false;
    mAligned = false;
}
////////////////////////////////////////////////////////////////////////////////
const btVector3& PerimeterSensor::GetNormalizedResultantForceVector()
{
    //Calculate the total resultant force
    osg::Vec3* startPoint =
        &mVertexArray->at( *mLastClockWiseDetection * 2 );
    osg::Vec3* endPoint =
        &mVertexArray->at( *mLastClockWiseDetection * 2 + 1 );

    double x = endPoint->x() - startPoint->x();
    double y = endPoint->y() - startPoint->y();
    if( mIntersections.empty() )
    {
        mResultantForce.setX( x );
        mResultantForce.setY( y );
    }
    else
    {
        mResultantForce.setX(  y );
        mResultantForce.setY( -x );
    }

    if( mResultantForce.length() != 0.0 )
    {
        mResultantForce = mResultantForce.normalize();
    }

    return mResultantForce;
}
////////////////////////////////////////////////////////////////////////////////
osg::Drawable* PerimeterSensor::GetQueriedConnection()
{
    return mQueriedConnection;
}
////////////////////////////////////////////////////////////////////////////////
void PerimeterSensor::SetRange( double range )
{
    mRange = range;
}
////////////////////////////////////////////////////////////////////////////////
bool PerimeterSensor::Aligned()
{
    return mAligned;
}
////////////////////////////////////////////////////////////////////////////////
bool PerimeterSensor::PerimeterDetected()
{
    return mPerimeterDetected;
}
////////////////////////////////////////////////////////////////////////////////
