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

// --- OSG Includes --- //
#include <osg/Geometry>

#include <osgUtil/IntersectionVisitor>

using namespace bots;

const double PI = 3.14159265358979323846;

////////////////////////////////////////////////////////////////////////////////
ObstacleSensor::ObstacleSensor( bots::AgentEntity* agentEntity )
:
Sensor( agentEntity ),
mObstacleDetected( false ),
mAngleIncrement( 30 ),
mRange( 2.0 ),
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

    double* agentPosition = agentDCS->GetVETranslationArray();

    //Reset results from last frame
    mIntersections.clear();

    osg::Vec3d startPoint, endPoint;
    startPoint.set( agentPosition[ 0 ],
                    agentPosition[ 1 ],
                    agentPosition[ 2 ] );
    mLineSegmentIntersector->setStart( startPoint );
    for( double angle = 0; angle < 360; )
    {
        endPoint.set( agentPosition[ 0 ] + mRange * cos( angle ), 
                      agentPosition[ 1 ] + mRange * sin( angle ), 
                      agentPosition[ 2 ] );

        mLineSegmentIntersector->reset();
        mLineSegmentIntersector->setEnd( endPoint );

        osgUtil::IntersectionVisitor intersectionVisitor(
            mLineSegmentIntersector.get() );

        if( mAgentEntity->IsBuilding() )
        {
            pluginDCS->accept( intersectionVisitor );

            angle += 90;
        }
        else
        {
            pluginDCS->RemoveChild( targetDCS.get() );
            pluginDCS->accept( intersectionVisitor );
            pluginDCS->AddChild( targetDCS.get() );

            angle += mAngleIncrement;
        }

        osgUtil::LineSegmentIntersector::Intersections& intersections =
            mLineSegmentIntersector->getIntersections();
        if( intersections.size() > 1 )
        {
            osgUtil::LineSegmentIntersector::Intersections::iterator itr =
                intersections.begin(); itr++;

            //Get the next hit excluding the agent itself
            mIntersections.push_back( *itr );
        }
    }

    if( !mIntersections.empty() )
    {
        mObstacleDetected = true;
    }
    else
    {
        mObstacleDetected = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
btVector3 ObstacleSensor::GetResultantForce()
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS =
        mAgentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS =
        mAgentEntity->GetTargetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();
    if( targetDCS.valid() )
    {
        double* targetPosition = targetDCS->GetVETranslationArray();
        VirtualForceField( agentPosition, targetPosition );
    }
    else
    {
        WallFollowing( agentPosition );
    }

    return mResultantForce;
}
////////////////////////////////////////////////////////////////////////////////
btVector3 ObstacleSensor::GetRepulsiveForce( double* agentPosition )
{
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

        if( mAgentEntity->IsBuilding() )
        {
            return repulsiveForce;
        }
    }

    return repulsiveForce;
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::VirtualForceField(
    double* agentPosition, double* targetPosition )
{
    btVector3 totalForce( 0, 0, 0 );

    //Calculate the total repulsive force
    totalForce = GetRepulsiveForce( agentPosition );

    //Calculate the target force        
    btVector3 targetForce( targetPosition[ 0 ] - agentPosition[ 0 ],
                           targetPosition[ 1 ] - agentPosition[ 1 ], 0 );

    targetForce /= targetForce.length();
    targetForce *= mForceAttractionConstant;

    totalForce += targetForce;

    mResultantForce = totalForce.normalize();
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::WallFollowing( double* agentPosition )
{
    btVector3 totalForce( 0, 0, 0 );

    //Calculate the total repulsive force
    totalForce = GetRepulsiveForce( agentPosition );

    //Calculate the target force
    //2D code to rotate vector about point( 0, 0, 0 ) by theta
    //x' = x * cos( theta ) - y * sin( theta );
    //y' = x * sin( theta ) + y * cos( theta );

    //Grab the repulsive force vector from last calculation
    double x = totalForce.x();
    double y = totalForce.y();

    double angle( 110 );
    if( mAgentEntity->IsBuilding() )
    {
        angle = 90;
    }
    double cosTheta = cos( angle * ( PI / 180 ) );
    double sinTheta = sin( angle * ( PI / 180 ) );
    
    double xNew = ( x * cosTheta ) - ( y * sinTheta );
    double yNew = ( x * sinTheta ) + ( y * cosTheta );
    
    btVector3 targetForce( xNew, yNew, 0 );
    totalForce += targetForce;

    mResultantForce = totalForce.normalize();
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
