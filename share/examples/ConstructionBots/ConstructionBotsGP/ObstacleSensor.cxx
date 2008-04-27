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
#include "ObstacleSensor.h"
#include "AgentEntity.h"
#include "PerimeterSensor.h"

// --- OSG Includes --- //
#include <osg/Geometry>

#include <osgUtil/IntersectionVisitor>

// --- c/C++ Includes --- //
#include <iostream>

using namespace bots;

const double piDivOneEighty = 0.0174532925;

////////////////////////////////////////////////////////////////////////////////
ObstacleSensor::ObstacleSensor( bots::AgentEntity* agentEntity )
    :
    Sensor( agentEntity ),
    mObstacleDetected( false ),
    mAngleIncrement( 20 ),
    mRange( 0 ),
    mForceRepellingConstant( 1.0 ),
    mForceAttractionConstant( 1.0 ),
    mResultantForce( 0, 0, 0 ),
    mLineSegmentIntersector( new osgUtil::LineSegmentIntersector(
                                 osg::Vec3( 0, 0, 0 ), osg::Vec3( 0, 0, 0 ) ) )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ObstacleSensor::~ObstacleSensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::CollectInformation()
{
    //Get the DCSs
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS =
        mAgentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS =
        mAgentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS =
        mAgentEntity->GetTargetDCS();

    //Reset results from last frame
    mIntersections.clear();
    mWallIntersections.clear();

    osg::Vec3d startPoint, endPoint;
    startPoint = agentDCS->getPosition();
    mLineSegmentIntersector->setStart( startPoint );
    for( double angle = 0; angle < 360; )
    {
        double range = mRange;
        if( mAgentEntity->mBuildMode )
        {
            range = 1.0;
        }
        endPoint.set( startPoint.x() + range * cos( angle * piDivOneEighty ), 
                      startPoint.y() + range * sin( angle * piDivOneEighty ), 
                      startPoint.z() );

        mLineSegmentIntersector->reset();
        mLineSegmentIntersector->setEnd( endPoint );

        osgUtil::IntersectionVisitor intersectionVisitor(
            mLineSegmentIntersector.get() );

        pluginDCS->RemoveChild( agentDCS.get() );
        pluginDCS->RemoveChild( targetDCS.get() );
        pluginDCS->accept( intersectionVisitor );
        pluginDCS->AddChild( agentDCS.get() );
        pluginDCS->AddChild( targetDCS.get() );

        if( mLineSegmentIntersector->containsIntersections() )
        {
            osgUtil::LineSegmentIntersector::Intersection intersection =
                mLineSegmentIntersector->getFirstIntersection();
            osg::Drawable* drawable = intersection.drawable.get();
            if( drawable->getName() == "Wall" )
            {
                mWallIntersections.push_back( intersection );
            }
            else
            {
                mIntersections.push_back( intersection );
            }

            if( mAgentEntity->mBuildMode )
            {
                osg::Vec4* color = &( static_cast< osg::Vec4Array* >(
                    drawable->asGeometry()->getColorArray() )->at( 0 ) );
                if( color->length() == 1.0 )
                {
                    mIntersections.pop_back();
                }
                else
                {
                    angle = 0;
                    mAgentEntity->mBuildMode = false;
                    mAgentEntity->mPerimeterSensor->Reset();
                }
            }
        }

        angle += mAngleIncrement;
    }

    if( !mIntersections.empty() || !mWallIntersections.empty() )
    {
        mObstacleDetected = true;
    }
    else
    {
        mObstacleDetected = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
const btVector3& ObstacleSensor::GetNormalizedResultantForceVector()
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS =
        mAgentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS =
        mAgentEntity->GetTargetDCS();
    double* agentPosition = agentDCS->GetVETranslationArray();

    //
    btVector3 totalForce( 0, 0, 0 );

    //Get the repulsive force
    btVector3 repulsiveForce( 0, 0, 0 );
    for( size_t i = 0; i < mIntersections.size(); ++i )
    {
        osg::Vec3d intersect = mIntersections.at( i ).getWorldIntersectPoint();
        btVector3 deltaForce( intersect.x() - agentPosition[ 0 ],
                              intersect.y() - agentPosition[ 1 ], 0 );

        double variables = mForceRepellingConstant / deltaForce.length2();
        deltaForce /= deltaForce.length();
        deltaForce *= variables;

        repulsiveForce -= deltaForce;
    }
    totalForce += repulsiveForce;

    //Get the wall repulsive force
    btVector3 wallRepulsiveForce( 0, 0, 0 );
    for( size_t i = 0; i < mWallIntersections.size(); ++i )
    {
        osg::Vec3d intersect =
            mWallIntersections.at( i ).getWorldIntersectPoint();
        btVector3 deltaForce( intersect.x() - agentPosition[ 0 ],
                              intersect.y() - agentPosition[ 1 ], 0 );

        double variables = mForceRepellingConstant / deltaForce.length2();
        deltaForce /= deltaForce.length();
        deltaForce *= variables;

        wallRepulsiveForce -= deltaForce;
    }
    totalForce += wallRepulsiveForce;

    //Calculate the target force
    btVector3 targetForce( 0, 0, 0 );
    if( targetDCS.valid() )
    {
        double* targetPosition = targetDCS->GetVETranslationArray();
        targetForce.setValue( targetPosition[ 0 ] - agentPosition[ 0 ],
                              targetPosition[ 1 ] - agentPosition[ 1 ], 0 );

        targetForce /= targetForce.length();
        targetForce *= mForceAttractionConstant;
    }
    else if( !mWallIntersections.empty() )
    {
        double x = wallRepulsiveForce.x();
        double y = wallRepulsiveForce.y();

        double cosTheta = cos( 145 * piDivOneEighty );
        double sinTheta = sin( 145 * piDivOneEighty );

        double xNew = ( x * cosTheta ) - ( y * sinTheta );
        double yNew = ( x * sinTheta ) + ( y * cosTheta );

        targetForce.setValue( xNew, yNew, 0 );
    }
    totalForce += targetForce;

    mResultantForce = totalForce;
    if( mResultantForce.length() != 0.0 )
    {
        mResultantForce = mResultantForce.normalize();
    }
    return mResultantForce;
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::SetAngleIncrement( double angleIncrement )
{
    mAngleIncrement = angleIncrement;
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::SetRange( double range )
{
    mRange = range;
}
////////////////////////////////////////////////////////////////////////////////
bool ObstacleSensor::ObstacleDetected()
{
    return mObstacleDetected;
}
////////////////////////////////////////////////////////////////////////////////
