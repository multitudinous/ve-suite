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

// --- OSG Includes --- //
#include <osgUtil/IntersectionVisitor>

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
    mPreviousSensor( NULL ),
    mRange( 0.05 ),
    mResultantForce( 0, 0, 0 ),
    mQueriedConnection( 0 ),
    mLastDetectionCCW( osg::Vec3d( 0, 0, 0 ), osg::Vec3d( 0, 0, 0 ) ),
    mLocalPositions( 0 ),
    mLineSegmentIntersector( 0 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
PerimeterSensor::~PerimeterSensor()
{
    if( mPreviousSensor )
    {
        delete mPreviousSensor;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PerimeterSensor::Initialize()
{
    CalculateLocalPositions();
}
////////////////////////////////////////////////////////////////////////////////
void PerimeterSensor::CalculateLocalPositions()
{
    mLineSegmentIntersector = new osgUtil::LineSegmentIntersector(
        osg::Vec3( 0, 0, 0 ), osg::Vec3( 0, 0, 0 ) );

    mLocalPositions = new osg::Vec3Array();
    mLocalPositions->resize( 16 );

    double blockHalfWidth = 0.5;
    osg::Vec3d rightStartPoint, rightEndPoint;
    rightStartPoint.set( blockHalfWidth - mRange, -blockHalfWidth, 0 );
    rightEndPoint.set( blockHalfWidth + mRange, -blockHalfWidth, 0 );

    osg::Vec3d leftStartPoint, leftEndPoint;
    leftStartPoint.set( blockHalfWidth - mRange, blockHalfWidth, 0 );
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
        (*mLocalPositions)[ i * 4 ] = osg::Vec3( xNew, yNew, 0 );

        x = rightEndPoint.x();
        y = rightEndPoint.y();
        xNew = ( x * cosTheta ) - ( y * sinTheta );
        yNew = ( x * sinTheta ) + ( y * cosTheta );
        (*mLocalPositions)[ i * 4 + 1 ] = osg::Vec3( xNew, yNew, 0 );

        x = leftStartPoint.x();
        y = leftStartPoint.y();
        xNew = ( x * cosTheta ) - ( y * sinTheta );
        yNew = ( x * sinTheta ) + ( y * cosTheta );
        (*mLocalPositions)[ i * 4 + 2 ] = osg::Vec3( xNew, yNew, 0 );

        x = leftEndPoint.x();
        y = leftEndPoint.y();
        xNew = ( x * cosTheta ) - ( y * sinTheta );
        yNew = ( x * sinTheta ) + ( y * cosTheta );
        (*mLocalPositions)[ i * 4 + 3 ] = osg::Vec3( xNew, yNew, 0 );
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

    //Reset
    mIntersections.clear();
    mQueriedConnection = NULL;
    mAligned = false;

    bool lastCCW( false );
    bool previousDetection( false );
    osg::Vec3d agentPosition = agentDCS->getPosition();
    for( unsigned int i = 0; i < 8; ++i )
    {
        osg::Vec3d startPoint = (*mLocalPositions)[ i * 2 ];
        osg::Vec3d endPoint = (*mLocalPositions)[ i * 2 + 1 ];

        startPoint += agentPosition;
        endPoint += agentPosition;

        mLineSegmentIntersector->reset();
        mLineSegmentIntersector->setStart( startPoint );
        mLineSegmentIntersector->setEnd( endPoint );

        osgUtil::IntersectionVisitor intersectionVisitor(
            mLineSegmentIntersector.get() );

        pluginDCS->RemoveChild( agentDCS.get() );
        pluginDCS->accept( intersectionVisitor );
        pluginDCS->AddChild( agentDCS.get() );

        if( mLineSegmentIntersector->containsIntersections() )
        {
            mIntersections.push_back(
                mLineSegmentIntersector->getFirstIntersection() );

            previousDetection = true;

            //Store the current detection
            if( !lastCCW )
            {
                mLastDetectionCCW.first = startPoint;
                mLastDetectionCCW.second = endPoint;
                
                unsigned int modulusTest( 0 );
                if( mPreviousSensor )
                {
                    modulusTest = ( *mPreviousSensor + i ) % 4;
                }
                if( modulusTest == 1  )
                {
                    mAligned = true;
                    mQueriedConnection = mIntersections.back().drawable.get();

                    delete mPreviousSensor;
                    mPreviousSensor = NULL;

                    break;
                }
                else
                {
                    if( !mPreviousSensor )
                    {
                        mPreviousSensor = new unsigned int();
                    }

                    *mPreviousSensor = i;
                }
            }
        }
        else
        {
            if( previousDetection )
            {
                lastCCW = true;
            }

            previousDetection = false;
        }
    }

    if( !mIntersections.empty() )
    {
        mPerimeterDetected = true;
    }
    else
    {
        mPerimeterDetected = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
const btVector3& PerimeterSensor::GetNormalizedResultantForceVector()
{
    //Calculate the total resultant force
    double x = mLastDetectionCCW.first.x() - mLastDetectionCCW.second.x();
    double y = mLastDetectionCCW.first.y() - mLastDetectionCCW.second.y();
    if( mIntersections.empty() )
    {
        mResultantForce.setX( -x );
        mResultantForce.setY( -y );
    }
    else if( mIntersections.size() == 1 )
    {
        mResultantForce.setX( -y );
        mResultantForce.setY(  x );
    }
    else
    {
        mResultantForce.setX( x );
        mResultantForce.setY( y );

        mAligned = true;
        mQueriedConnection = mIntersections.back().drawable.get();
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
bool PerimeterSensor::IsAligned()
{
    return mAligned;
}
////////////////////////////////////////////////////////////////////////////////
bool PerimeterSensor::PerimeterDetected()
{
    return mPerimeterDetected;
}
////////////////////////////////////////////////////////////////////////////////
