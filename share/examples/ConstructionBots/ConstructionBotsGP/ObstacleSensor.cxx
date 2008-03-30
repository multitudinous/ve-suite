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

// --- VE-Suite Includes --- //

// --- OSG Includes --- //
#include <osg/Geometry>

#include <osgUtil/IntersectVisitor>

// --- C/C++ Libraries --- //
#include <vector>
#include <cmath>

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
mBeamLineSegment( new osg::LineSegment() )
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
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = mAgentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = mAgentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS = mAgentEntity->GetTargetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();

    //Reset results from last frame
    mObstacleHits.clear();

    osg::Vec3d startPoint, endPoint;
    startPoint.set( agentPosition[ 0 ], agentPosition[ 1 ], agentPosition[ 2 ] );
    for( double angle = 0; angle < 360; )
    {
        endPoint.set( agentPosition[ 0 ] + mRange * cos( angle ), 
                      agentPosition[ 1 ] + mRange * sin( angle ), 
                      agentPosition[ 2 ] );

        mBeamLineSegment->set( startPoint, endPoint );

        osgUtil::IntersectVisitor intersectVisitor;
        intersectVisitor.addLineSegment( mBeamLineSegment.get() );

        if( mAgentEntity->IsBuilding() )
        {
            pluginDCS->accept( intersectVisitor );
        }
        else
        {
            pluginDCS->RemoveChild( targetDCS.get() );
            pluginDCS->accept( intersectVisitor );
            pluginDCS->AddChild( targetDCS.get() );
        }

        osgUtil::IntersectVisitor::HitList hitList = intersectVisitor.getHitList( mBeamLineSegment.get() );

        if( hitList.size() > 1 )
        {
            //Get the next hit excluding the agent itself
            mObstacleHits.push_back( hitList.at( 1 ) );
        }

        angle += mAngleIncrement;
    }

	if( !mObstacleHits.empty() )
	{
		mObstacleDetected = true;
	}
	else
	{
		mObstacleDetected = false;
	}
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::CalculateResultantForce( bool buildMode )
{
    if( buildMode )
    {
        WallFollowing();
    }
    else
    {
        VirtualForceField();
    }
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::VirtualForceField()
{
    // ------------------ Virtual Force Field (VFF) Method ------------------ //
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = mAgentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS = mAgentEntity->GetTargetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();
    
    btVector3 totalForce( 0, 0, 0 );

    //Calculate the total repulsive force
    for( size_t i = 0; i < mObstacleHits.size(); ++i )
    {
        osg::ref_ptr< osg::Geode > geode = mObstacleHits.at( i ).getGeode();

        if( geode.valid() )
        {
            osg::Vec3d intersect = mObstacleHits.at( i ).getWorldIntersectPoint();
            btVector3 repulsiveForce( intersect.x() - agentPosition[ 0 ],
                                      intersect.y() - agentPosition[ 1 ], 0 );

            double variables = mForceRepellingConstant / repulsiveForce.length2();
            repulsiveForce /= repulsiveForce.length();
            repulsiveForce *= variables;

            totalForce -= repulsiveForce;
        }
    }

    //Calculate the target force
    if( targetDCS.valid() )
    {
        double* targetPosition = targetDCS->GetVETranslationArray();
        btVector3 targetForce( targetPosition[ 0 ] - agentPosition[ 0 ],
                               targetPosition[ 1 ] - agentPosition[ 1 ], 0 );

        targetForce /= targetForce.length();
        targetForce *= mForceAttractionConstant;

        totalForce += targetForce;
    }

    mResultantForce = totalForce.normalize();
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::WallFollowing()
{
    // ----------------------- Wall Following Method ------------------------ //
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = mAgentEntity->GetDCS();

    double* agentPosition = agentDCS->GetVETranslationArray();
    
    btVector3 totalForce( 0, 0, 0 );

    //Calculate the total repulsive force
    for( size_t i = 0; i < mObstacleHits.size(); ++i )
    {
        osg::ref_ptr< osg::Geode > geode = mObstacleHits.at( i ).getGeode();

        if( geode.valid() )
        {
            osg::Vec3d intersect = mObstacleHits.at( i ).getWorldIntersectPoint();
            btVector3 repulsiveForce( intersect.x() - agentPosition[ 0 ],
                                      intersect.y() - agentPosition[ 1 ], 0 );

            double variables = mForceRepellingConstant / repulsiveForce.length2();
            repulsiveForce /= repulsiveForce.length();
            repulsiveForce *= variables;

            totalForce -= repulsiveForce;
        }
    }

    //Calculate the target force
    {
        //2D code to rotate vector about point (0, 0, 0) by theta
        //x' = x * cos( theta ) - y * sin( theta );
        //y' = x * sin( theta ) + y * cos( theta );

        //Grab the repulsive force vector from last calculation
        double x = totalForce.x();
        double y = totalForce.y();
        double cosTheta = cos( 175 * ( PI / 180 ) );
        double sinTheta = sin( 175 * ( PI / 180 ) );
        
        double xNew = ( x * cosTheta ) - ( y * sinTheta );
        double yNew = ( x * sinTheta ) + ( y * cosTheta );
        
        btVector3 targetForce( xNew, yNew, 0 );
        totalForce += targetForce;
    }

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
btVector3 ObstacleSensor::GetResultantForce()
{
    return mResultantForce;
}
////////////////////////////////////////////////////////////////////////////////
