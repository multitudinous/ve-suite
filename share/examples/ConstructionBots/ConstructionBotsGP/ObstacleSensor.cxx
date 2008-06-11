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
#include "HoldBlockSensor.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>

#include <osgUtil/IntersectionVisitor>

// --- c/C++ Includes --- //
#include <iostream>

using namespace bots;

const double piDivOneEighty = 0.0174532925;
const double oneEightyDivPI = 57.2957795;

////////////////////////////////////////////////////////////////////////////////
ObstacleSensor::ObstacleSensor( bots::AgentEntity* agentEntity )
    :
    Sensor( agentEntity ),
    mObstacleDetected( false ),
    mAngleIncrement( 20 ),
    mRange( 0 ),
    mForceAttractionConstant( 1.0 ),
    mForceRepellingConstant( 1.0 ),
    mResultantForce( 0, 0, 0 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
ObstacleSensor::~ObstacleSensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::Initialize()
{
    mLineSegmentIntersector = new osgUtil::LineSegmentIntersector(
        osg::Vec3( 0, 0, 0 ), osg::Vec3( 0, 0, 0 ) );
    mGeode = new osg::Geode();
    mGeometry = new osg::Geometry();
    mVertexArray = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colorArray = new osg::Vec4Array();

    mVertexArray->resize( 6 );
    mGeometry->setVertexArray( mVertexArray.get() );

    colorArray->push_back( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );
    colorArray->push_back( osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );
    colorArray->push_back( osg::Vec4( 1.0, 1.0, 0.0, 1.0 ) );
    mGeometry->setColorArray( colorArray.get() );
    mGeometry->setColorBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );
    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 2, 2 ) );
    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 4, 2 ) );

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

    DisplayGeometry( true );
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
    mObstacleDetected = false;

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
            osg::Vec4* color = &( static_cast< osg::Vec4Array* >(
                drawable->asGeometry()->getColorArray() )->at( 0 ) );
            if( !mAgentEntity->mBuildMode )
            {
                mIntersections.push_back( intersection );
            }
            else if( color->length() != 1.0 )
            {
                mIntersections.push_back( intersection );         
            }
        }

        angle += mAngleIncrement;
    }

    if( !mIntersections.empty() )
    {
        mObstacleDetected = true;
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

    (*mVertexArray)[ 0 ] = (*mVertexArray)[ 1 ] =
    (*mVertexArray)[ 2 ] = (*mVertexArray)[ 3 ] =
    (*mVertexArray)[ 4 ] = (*mVertexArray)[ 5 ] = agentDCS->getPosition();

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

    //Calculate the target force
    btVector3 targetForce( 0.0, 0.0, 0.0 );
    if( targetDCS.valid() )
    {
        double* targetPosition = targetDCS->GetVETranslationArray();
        targetForce.setValue( targetPosition[ 0 ] - agentPosition[ 0 ],
                              targetPosition[ 1 ] - agentPosition[ 1 ], 0.0 );

        //A . B = |A||B|cos( theta )
        //theta = acos[ ( A . B ) / |A||B| ]
        btVector3 currentForce =
            mAgentEntity->GetPhysicsRigidBody()->getLinearVelocity();
        //currentForce /= currentForce.length();
        //currentForce *= mForceAttractionConstant;

        double theta = acos( targetForce.dot( currentForce ) /
            ( targetForce.length() * currentForce.length() ) );
        theta *= oneEightyDivPI;

        std::cout << "theta: " << theta << std::endl;

        if( theta > 80.0 )
        {
            double x = repulsiveForce.x();
            double y = repulsiveForce.y();

            double cosTheta = cos( 145.0 * piDivOneEighty );
            double sinTheta = sin( 145.0 * piDivOneEighty );

            double xNew = ( x * cosTheta ) - ( y * sinTheta );
            double yNew = ( x * sinTheta ) + ( y * cosTheta );

            //totalForce -= targetForce;
            targetForce.setValue( xNew, yNew, 0.0 );
            /*
            if( targetForce.length() != 0.0 )
            {
                targetForce /= targetForce.length();
                targetForce *= mForceAttractionConstant;
            }
            totalForce += targetForce;
            */

            //std::cout << "Here" << std::endl;
        }

    }
    else
    {
        //Drive straight
        targetForce =
            mAgentEntity->GetPhysicsRigidBody()->getLinearVelocity();
    }

    if( targetForce.length() != 0.0 )
    {
        targetForce /= targetForce.length();
        targetForce *= mForceAttractionConstant;
    }
    totalForce += repulsiveForce;
    totalForce += targetForce;

    (*mVertexArray)[ 1 ].x() += repulsiveForce.x() * 10.0;
    (*mVertexArray)[ 1 ].y() += repulsiveForce.y() * 10.0;
    (*mVertexArray)[ 1 ].z() += repulsiveForce.z();

    (*mVertexArray)[ 3 ].x() += targetForce.x() * 10.0;
    (*mVertexArray)[ 3 ].y() += targetForce.y() * 10.0;
    (*mVertexArray)[ 3 ].z() += targetForce.z();

    mGeometry->dirtyDisplayList();
    mGeometry->dirtyBound();

    mResultantForce = totalForce;
    if( mResultantForce.length() != 0.0 )
    {
        mResultantForce = mResultantForce.normalize();
    }

    (*mVertexArray)[ 5 ].x() += mResultantForce.x() * 10.0;
    (*mVertexArray)[ 5 ].y() += mResultantForce.y() * 10.0;
    (*mVertexArray)[ 5 ].z() += mResultantForce.z();

    return mResultantForce;
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::SetAngleIncrement( double angleIncrement )
{
    mAngleIncrement = angleIncrement;
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::SetForceAttractionConstant(
    double forceAttractionConstant )
{
    mForceAttractionConstant = forceAttractionConstant;
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::SetForceRepellingConstant( double forceRepellingConstant )
{
    mForceRepellingConstant = forceRepellingConstant;
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
