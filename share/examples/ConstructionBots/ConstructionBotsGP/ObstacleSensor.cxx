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
#include "BlockEntity.h"

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
    mNumForceLines( 3 ),
    mNumDetectors( 0 ),
    mAngleIncrement( 20 ),
    mRange( 0 ),
    mForceAttractionConstant( 1.0 ),
    mForceRepellingConstant( 1.0 ),
    mDetectorGeometry( 0 ),
    mDetectorVertexArray( 0 ),
    mResultantForce( 0, 0, 0 ),
    mIntersectorGroup( 0 )
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
    mIntersectorGroup = new osgUtil::IntersectorGroup();
    mGeode = new osg::Geode();
    mGeometry = new osg::Geometry();
    mDetectorGeometry = new osg::Geometry();
    mVertexArray = new osg::Vec3Array();
    mDetectorVertexArray = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colorArray = new osg::Vec4Array();
    osg::ref_ptr< osg::Vec4Array > detectorColorArray = new osg::Vec4Array();

    mNumDetectors = static_cast< int >( 360 ) / mAngleIncrement;
    
    mVertexArray->resize( 2 * mNumForceLines );
    mDetectorVertexArray->resize( 2 * mNumDetectors );

    mGeometry->setVertexArray( mVertexArray.get() );
    mDetectorGeometry->setVertexArray( mDetectorVertexArray.get() );

    CalculateLocalPositions();

    colorArray->push_back( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );
    colorArray->push_back( osg::Vec4( 0.0, 0.0, 1.0, 1.0 ) );
    colorArray->push_back( osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );
    mGeometry->setColorArray( colorArray.get() );
    mGeometry->setColorBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    detectorColorArray->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    mDetectorGeometry->setColorArray( detectorColorArray.get() );
    mDetectorGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    mGeode->addDrawable( mGeometry.get() );
    //mGeode->addDrawable( mDetectorGeometry.get() );

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
}
////////////////////////////////////////////////////////////////////////////////
void ObstacleSensor::CalculateLocalPositions()
{
    for( unsigned int i = 0; i < mNumForceLines; ++i )
    {
        mGeometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, i * 2, 2 ) );
    }

    for( unsigned int i = 0; i < mNumDetectors; ++i )
    {
        double cosTheta = cos( i * mAngleIncrement * piDivOneEighty );
        double sinTheta = sin( i * mAngleIncrement * piDivOneEighty );

        (*mDetectorVertexArray)[ i * 2 ] = osg::Vec3( 0.0, 0.0, 0.0 );
        (*mDetectorVertexArray)[ i * 2 + 1 ] =
            osg::Vec3( cosTheta, sinTheta, 0.0 );

        mDetectorGeometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, i * 2, 2 ) );

        mIntersectorGroup->addIntersector( new osgUtil::LineSegmentIntersector(
            osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) ) );
    }
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

    osg::Vec3 endPoint( 0.0, 0.0, 0.0 );
    osg::Vec3 agentPosition = agentDCS->getPosition();
    osgUtil::IntersectorGroup::Intersectors& intersectors =
        mIntersectorGroup->getIntersectors();
    for( unsigned int i = 0; i < mNumDetectors; ++i )
    {
        endPoint = (*mDetectorVertexArray)[ i * 2 + 1 ];
        endPoint *= mRange;
        endPoint += agentPosition;

        mLineSegmentIntersector =
            static_cast< osgUtil::LineSegmentIntersector* >(
                intersectors.at( i ).get() );
        mLineSegmentIntersector->reset();
        mLineSegmentIntersector->setStart( agentPosition );
        mLineSegmentIntersector->setEnd( endPoint );
    }

    //Remove the agentDCS and the targetDCS for intersection test
    pluginDCS->RemoveChild( agentDCS.get() );
    pluginDCS->RemoveChild( targetDCS.get() );
    //This is an expensive call
    //Try to only call once by using group intersector
    osgUtil::IntersectionVisitor intersectionVisitor( mIntersectorGroup.get() );
    pluginDCS->accept( intersectionVisitor );
    //Add back the agentDCS and targetDCS
    pluginDCS->AddChild( agentDCS.get() );
    pluginDCS->AddChild( targetDCS.get() );

    for( unsigned int i = 0; i < mNumDetectors; ++i )
    {
        mLineSegmentIntersector =
            static_cast< osgUtil::LineSegmentIntersector* >(
                intersectors.at( i ).get() );
        if( mLineSegmentIntersector->containsIntersections() )
        {
            osgUtil::LineSegmentIntersector::Intersection intersection =
                mLineSegmentIntersector->getFirstIntersection();
            mIntersections.push_back( intersection );
        }
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

    //Get the repulsive force
    btVector3 repulsiveForce( 0, 0, 0 );
    for( size_t i = 0; i < mIntersections.size(); ++i )
    {
        osg::Vec3 intersect = mIntersections.at( i ).getWorldIntersectPoint();
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

        //Code to find angle between two vectors sharing origin
        //Dot product rule
        //A . B = |A||B|cos( theta )
        //theta = acos[ ( A . B ) / |A||B| ]
        btVector3 currentForce =
            mAgentEntity->GetPhysicsRigidBody()->getLinearVelocity();

        double theta( 0 );
        double targetForceLength = targetForce.length();
        double currentForceLength = currentForce.length();
        if( targetForceLength != 0.0 && currentForceLength != 0.0 )
        {
            theta = acos( targetForce.dot( currentForce ) /
                ( targetForceLength * currentForceLength ) );
            theta *= oneEightyDivPI;
        }

        //Wall following algorithm
        //Set threshold to 80 instead of 90 to help with rounding corners
        if( theta > 80.0 )
        {
            double x = repulsiveForce.x();
            double y = repulsiveForce.y();

            double cosTheta = cos( 145.0 * piDivOneEighty );
            double sinTheta = sin( 145.0 * piDivOneEighty );

            double xNew = ( x * cosTheta ) - ( y * sinTheta );
            double yNew = ( x * sinTheta ) + ( y * cosTheta );

            targetForce.setValue( xNew, yNew, 0.0 );
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

    //Calculate the total force
    btVector3 totalForce( 0, 0, 0 );
    totalForce += repulsiveForce;
    totalForce += targetForce;

    mResultantForce = totalForce;
    if( mResultantForce.length() != 0.0 )
    {
        mResultantForce = mResultantForce.normalize();
    }

    if( mGeode->getNodeMask() )
    {
        repulsiveForce.normalize();
        targetForce.normalize();
        mResultantForce.normalize();

        (*mVertexArray)[ 0 ] = (*mVertexArray)[ 1 ] =
        (*mVertexArray)[ 2 ] = (*mVertexArray)[ 3 ] =
        (*mVertexArray)[ 4 ] = (*mVertexArray)[ 5 ] = agentDCS->getPosition();

        (*mVertexArray)[ 1 ].x() += repulsiveForce.x() * 3.0;
        (*mVertexArray)[ 1 ].y() += repulsiveForce.y() * 3.0;
        (*mVertexArray)[ 1 ].z() += repulsiveForce.z();

        (*mVertexArray)[ 3 ].x() += targetForce.x() * 3.0;
        (*mVertexArray)[ 3 ].y() += targetForce.y() * 3.0;
        (*mVertexArray)[ 3 ].z() += targetForce.z();

        (*mVertexArray)[ 5 ].x() += mResultantForce.x() * 3.0;
        (*mVertexArray)[ 5 ].y() += mResultantForce.y() * 3.0;
        (*mVertexArray)[ 5 ].z() += mResultantForce.z();

        mGeometry->dirtyDisplayList();
        mGeometry->dirtyBound();
    }

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
