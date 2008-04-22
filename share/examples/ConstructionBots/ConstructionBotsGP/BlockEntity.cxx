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
#include "BlockEntity.h"
#include "Block.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

// --- OSG Includes --- //
#include <osgUtil/IntersectionVisitor>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>
#include <BulletDynamics/ConstraintSolver/btGeneric6DofConstraint.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace bots;

const double piDivOneEighty = 0.0174532925;

////////////////////////////////////////////////////////////////////////////////
BlockEntity::BlockEntity(
    bots::Block* block,
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
    :
    CADEntity( block, pluginDCS, physicsSimulator ),
    mPluginDCS( pluginDCS ),
    mBlockGeometry( block ),
    mConstraint( 0 ),
    mLocation( 0, 0 ),
    mLocalPositions( new osg::Vec3Array() ),
    mLineSegmentIntersector( new osgUtil::LineSegmentIntersector(
                                 osg::Vec3( 0, 0, 0 ), osg::Vec3( 0, 0, 0 ) ) )
{
    //Initialize side attachments
    mConnectedBlocks[ 0 ] = NULL;
    mConnectedBlocks[ 1 ] = NULL;
    mConnectedBlocks[ 2 ] = NULL;
    mConnectedBlocks[ 3 ] = NULL;

    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
BlockEntity::~BlockEntity()
{
    if( mConstraint )
    {
        if( mPhysicsSimulator )
        {
            mPhysicsSimulator->GetDynamicsWorld()->removeConstraint(
                mConstraint );
        }

        delete mConstraint;
    }
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::Initialize()
{
    CalculateLocalPositions();
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::CalculateLocalPositions()
{
    mLocalPositions->resize( 16 );

    double blockHalfWidth = 0.5;
    osg::Vec3d startPoint, endPoint;
    startPoint.set( 0, 0, 0 );
    endPoint.set( 2.0 * blockHalfWidth, 0, 0 );

    //Rotate vector about point( 0, 0, 0 ) by theta
    //x' = x * cos( theta ) - y * sin( theta );
    //y' = x * sin( theta ) + y * cos( theta );
    for( int i = 0; i < 4; ++i )
    {
        double x, y, xNew, yNew;
        double cosTheta = cos( i * 90 * piDivOneEighty );
        double sinTheta = sin( i * 90 * piDivOneEighty );

        x = startPoint.x();
        y = startPoint.y();
        xNew = ( x * cosTheta ) - ( y * sinTheta );
        yNew = ( x * sinTheta ) + ( y * cosTheta );
        (*mLocalPositions)[ i * 2 ] = osg::Vec3( xNew, yNew, 0 );

        x = endPoint.x();
        y = endPoint.y();
        xNew = ( x * cosTheta ) - ( y * sinTheta );
        yNew = ( x * sinTheta ) + ( y * cosTheta );
        (*mLocalPositions)[ i * 2 + 1 ] = osg::Vec3( xNew, yNew, 0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::AttachUpdate()
{
    //Update the connected blocks for this and neighbors
    ConnectionDetection();

    //Get coordinates and occupancy matrix from neighbors
    std::map< unsigned int, bots::BlockEntity* >::const_iterator itr;
    itr = mConnectedBlocks.begin();
    if( itr->second )
    {
        mOccupancyMatrix = itr->second->GetOccupancyMatrix();
        mLocation = itr->second->GetLocation();
        switch( itr->first )
        {
            case 0:
            {
                mLocation.first -= 1;
            }
            break;

            case 1:
            {
                mLocation.second -= 1;
            }
            break;

            case 2:
            {
                mLocation.first += 1;
            }
            break;

            case 3:
            {
                mLocation.second -= 1;
            }
            break;
        }
    }

    //Self align w/ blocks
    GetPhysicsRigidBody()->StaticConcaveShape();
    //double alignedPosition[ 3 ] = { mLocation.first, mLocation.second, 0.5 };
    //GetDCS()->SetTranslationArray( alignedPosition );

    //Update side states for this and neighbors
    for( itr; itr != mConnectedBlocks.end(); ++itr )
    {

    }

    mBlockGeometry->SetColor( 4, osg::Vec4( 0, 0, 0, 1 ) );

}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::UpdateSideStates()
{
    //First test the occupancy matrix for valid attachment sites
    std::map< unsigned int, bots::BlockEntity* >::const_iterator itr;
    for( itr = mConnectedBlocks.begin(); itr != mConnectedBlocks.end(); ++itr )
    {
        if( !itr->second )
        {
            itr->first;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bool BlockEntity::PermissionToAttach( osg::Drawable* drawable )
{
    return mSideStates[ drawable ];
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::ConnectionDetection()
{
    osg::Vec3 blockPosition = mDCS->getPosition();
    for( unsigned int i = 0; i < 4; ++i )
    {
        osg::Vec3d startPoint = (*mLocalPositions)[ i * 2 ];
        osg::Vec3d endPoint = (*mLocalPositions)[ i * 2 + 1 ];

        startPoint += blockPosition;
        endPoint += blockPosition;

        mLineSegmentIntersector->reset();
        mLineSegmentIntersector->setStart( startPoint );
        mLineSegmentIntersector->setEnd( endPoint );

        osgUtil::IntersectionVisitor intersectionVisitor(
            mLineSegmentIntersector.get() );
        mPluginDCS->accept( intersectionVisitor );

        const osgUtil::LineSegmentIntersector::Intersections& intersections =
            mLineSegmentIntersector->getIntersections();

        osg::ref_ptr< osg::Drawable > thisDrawable =
            intersections.begin()->drawable;
        osg::ref_ptr< osg::Drawable > connectedDrawable =
            intersections.rbegin()->drawable;

        if( thisDrawable.get() == connectedDrawable.get() )
        {
            mSideStates[ thisDrawable.get() ] = true;
        }
        else
        {
            ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
                connectedDrawable->getParent( 0 ) );
            osg::ref_ptr< ves::xplorer::scenegraph::DCS > dcs =
                static_cast< ves::xplorer::scenegraph::DCS* >(
                    parentVisitor.GetParentNode() );

            unsigned int oppositeSide = i;
            if( i > 1 )
            {
                oppositeSide -= 2;
            }
            else
            {
                oppositeSide += 2;
            }

            mBlockGeometry->SetColor( i, osg::Vec4( 0, 0, 0, 1 ) );
            mConnectedBlocks[ i ] = mBlockEntityMap[ dcs->GetName() ];
            mConnectedBlocks[ i ]->SetBlockConnection( oppositeSide, this );
            mConnectedBlocks[ i ]->GetBlockGeometry()->SetColor(
                oppositeSide, osg::Vec4( 0, 0, 0, 1 ) );
            mSideStates[ thisDrawable.get() ] = false;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bots::Block* BlockEntity::GetBlockGeometry()
{
    return mBlockGeometry.get();
}
////////////////////////////////////////////////////////////////////////////////
const std::pair< int, int >& BlockEntity::GetLocation()
{
    return mLocation;
}
////////////////////////////////////////////////////////////////////////////////
const std::map< std::pair< int, int >, bool >& BlockEntity::GetOccupancyMatrix()
{
    return mOccupancyMatrix;
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetBlockConnection(
    unsigned int side, bots::BlockEntity* blockEntity )
{
    mConnectedBlocks[ side ] = blockEntity;
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetBlockEntityMap(
    const std::map< std::string, bots::BlockEntity* >& blockEntityMap )
{
    mBlockEntityMap = blockEntityMap;
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetConstraints( int gridSize )
{
    btTransform trans;
    trans.setIdentity();
    trans.setOrigin( btVector3( 0.0, 0.0, 0.5 ) );

    //Must disable deactivation so constraint is always applied
    mPhysicsRigidBody->setActivationState( DISABLE_DEACTIVATION );
    btRigidBody* fixedBody = mPhysicsSimulator->CreateRigidBody( 0, trans, 0 );

    btTransform frameInA, frameInB;
    frameInA = btTransform::getIdentity();
    frameInB = btTransform::getIdentity();

#if( BULLET_MAJOR_VERSION >= 2 ) && ( BULLET_MINOR_VERSION > 61 )
    mConstraint = new btGeneric6DofConstraint(
        *mPhysicsRigidBody, *fixedBody, frameInA, frameInB, false );
#else
    mConstraint = new btGeneric6DofConstraint(
        *mPhysicsRigidBody, *fixedBody, frameInA, frameInB );
#endif

    //Fix the translation range for the agents
    //Give 0.5 units extra in xy plane for agent/wall collision
    //Give small z-range for agent/grid collision
    mConstraint->setLinearLowerLimit( btVector3( -100.0, -100.0, -100.0 ) );
    mConstraint->setLinearUpperLimit( btVector3(  100.0,  100.0,  100.0 ) );

    //Remove rotation from agents
    //Range should be small or singularities will 'explode' the constraint
    mConstraint->setAngularLowerLimit( btVector3( 0, 0, 0 ) );
    mConstraint->setAngularUpperLimit( btVector3( 0, 0, 0 ) );

    mPhysicsSimulator->GetDynamicsWorld()->addConstraint( mConstraint );
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetNameAndDescriptions( int number )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    mDCS->setDescriptions( descriptorsList );

    std::stringstream ss;
    ss << "Block" << number;
    std::cout << ss.str() << std::endl;
    mDCS->setName( ss.str() );
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetOccupancyMatrix(
    const std::map< std::pair< int, int >, bool >& occMatrix )
{
    mOccupancyMatrix = occMatrix;
}
////////////////////////////////////////////////////////////////////////////////
