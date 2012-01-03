/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#include "SiteSensor.h"
#include "AgentEntity.h"
#include "BlockEntity.h"
#include "ObstacleSensor.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>
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
    mLastClockWiseDetection( NULL ),
    mPreviousDrawable( NULL ),
    mRange( 0.2 ),
    mResultantForce( 0.0, 0.0, 0.0 ),
    mQueriedConnection( NULL ),
    mIntersectorGroup( NULL )
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
const bool PerimeterSensor::Aligned() const
{
    return mAligned;
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
const bool PerimeterSensor::CollisionTest()
{
    bool collision( false );
    std::map< std::string, bots::BlockEntity* >::const_iterator
        itr = mAgentEntity->mBlockEntityMap->find(
            mAgentEntity->GetTargetDCS()->GetName() );
    if( itr != mAgentEntity->mBlockEntityMap->end() )
    {
        //collision = mAgentEntity->GetPhysicsRigidBody()->CollisionInquiry( itr->second->GetPhysicsRigidBody() );
        if( collision )
        {
            mAgentEntity->mBuildMode = false;
            mAgentEntity->SetTargetDCS( NULL );
            mAgentEntity->mSiteSensor->Reset();
            Reset();
            mAgentEntity->mObstacleSensor->Reset();

            std::cout << "Collision: " << mAgentEntity->GetDCS()->getName()
                      << std::endl;
        }
    }

    return collision;
}
////////////////////////////////////////////////////////////////////////////////
void PerimeterSensor::CollectInformation()
{
    //Get the DCSs
    ves::xplorer::scenegraph::DCS* const pluginDCS =
        mAgentEntity->GetPluginDCS();
    ves::xplorer::scenegraph::DCS* const agentDCS =
        mAgentEntity->GetDCS();

    const osg::Vec3d& agentPosition = agentDCS->getPosition();
    const osgUtil::IntersectorGroup::Intersectors& intersectors =
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

    pluginDCS->RemoveChild( agentDCS );
    //This is an expensive call; Try to only call once using group intersector
    osgUtil::IntersectionVisitor intersectionVisitor( mIntersectorGroup.get() );
    pluginDCS->accept( intersectionVisitor );
    pluginDCS->AddChild( agentDCS );

    for( int i = 7; i > -1; --i )
    {
        mLineSegmentIntersector =
            static_cast< osgUtil::LineSegmentIntersector* >(
                intersectors.at( i ).get() );
        if( mLineSegmentIntersector->containsIntersections() )
        {
            const osgUtil::LineSegmentIntersector::Intersection& intersection =
                mLineSegmentIntersector->getFirstIntersection();
            osg::Drawable* const currentDrawable = intersection.drawable.get();
            const osg::Vec4Array* const tempArray =
                static_cast< const osg::Vec4Array* >(
                    currentDrawable->asGeometry()->getColorArray() );
            if( tempArray )
            {
                const osg::Vec4& color = tempArray->at( 0 );
                if( color == mAgentEntity->mSiteColor )
                {        
                    mIntersections.push_back( intersection );
                    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
                        currentDrawable->getParent( 0 ) );
                    ves::xplorer::scenegraph::DCS* const dcs =
                        static_cast< ves::xplorer::scenegraph::DCS* >(
                            parentVisitor.GetParentNode() );
                    mAgentEntity->SetTargetDCS( dcs );

                    if( !mPerimeterDetected )
                    {
                        mPerimeterDetected = true;
                        mAgentEntity->mBuildMode = true;
                    }
                }
                else
                {
                    mAgentEntity->mBuildMode = false;
                    mAgentEntity->SetTargetDCS( NULL );
                    mAgentEntity->mSiteSensor->Reset();
                    Reset();
                    mAgentEntity->mObstacleSensor->Reset();

                    std::cout << agentDCS->getName() << std::endl;

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
                    mQueriedConnection = currentDrawable;
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

    //Test for collision with the target DCS
    if( CollisionTest() )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
const btVector3& PerimeterSensor::GetNormalizedResultantForceVector()
{
    //Calculate the total resultant force
    const osg::Vec3* const startPoint =
        &mVertexArray->at( *mLastClockWiseDetection * 2 );
    const osg::Vec3* const endPoint =
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
osg::Drawable* const PerimeterSensor::GetQueriedConnection() const
{
    return mQueriedConnection.get();
}
////////////////////////////////////////////////////////////////////////////////
const double& PerimeterSensor::GetRange() const
{
    return mRange;
}
////////////////////////////////////////////////////////////////////////////////
const bool PerimeterSensor::PerimeterDetected() const
{
    return mPerimeterDetected;
}
////////////////////////////////////////////////////////////////////////////////
void PerimeterSensor::Reset()
{
    if( mLastClockWiseDetection )
    {
        delete mLastClockWiseDetection;
    }
    mLastClockWiseDetection = NULL;
    mPreviousDrawable = NULL;
    mQueriedConnection = NULL;

    mPerimeterDetected = false;
    mAligned = false;
}
////////////////////////////////////////////////////////////////////////////////
void PerimeterSensor::SetRange( double range )
{
    mRange = range;
}
////////////////////////////////////////////////////////////////////////////////
