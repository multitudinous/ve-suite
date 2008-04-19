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
#include <osg/Geometry>
#include <osg/LineWidth>

#include <osgUtil/IntersectionVisitor>
#include <osgUtil/LineSegmentIntersector>

#include <osgDB/ReadFile>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>
#include <BulletDynamics/ConstraintSolver/btGeneric6DofConstraint.h>

// --- C/C++ Libraries --- //
#include <sstream>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
BlockEntity::BlockEntity(
    bots::Block* block,
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
    :
    CADEntity( block, pluginDCS, physicsSimulator ),
    mPluginDCS( pluginDCS ),
    mGeometry( block ),
    mConstraint( 0 ),
    mLocation( 0, 0 )
{
    //Initialize side attachments
    mConnectedBlocks[ "Left" ] = NULL;
    mConnectedBlocks[ "Near" ] = NULL;
    mConnectedBlocks[ "Right" ] = NULL;
    mConnectedBlocks[ "Far" ] = NULL;
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
void BlockEntity::AttachUpdate()
{
    ConnectionDetection();

    mGeometry->SetColor( std::string( "Block" ), osg::Vec4( 0, 0, 0, 1 ) );

    std::map< std::string, bots::BlockEntity* >::const_iterator itr;
    itr = mConnectedBlocks.begin();
    if( itr->second )
    {
        mOccupancyMatrix = itr->second->GetOccupancyMatrix();
        mLocation = itr->second->GetLocation();

    }
    for( itr; itr != mConnectedBlocks.end(); ++itr )
    {
        if( itr->second )
        {
            mGeometry->SetColor( itr->first, osg::Vec4( 0, 0, 0, 1 ) );
            itr->second->GetGeometry()->SetColor( itr->first, osg::Vec4( 0, 0, 0, 1 ) );
        }
    }

}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::ConnectionDetection()
{
    ves::xplorer::scenegraph::DCS* temp;
    osg::Vec3 blockPosition = mDCS->getPosition();
    osg::ref_ptr< osgUtil::LineSegmentIntersector > lineSegmentIntersector =
        new osgUtil::LineSegmentIntersector( osg::Vec3( blockPosition.x() - 1,
                                                        blockPosition.y(),
                                                        blockPosition.z() ),
                                                        blockPosition );

    //Test the left connection
    {
        osgUtil::IntersectionVisitor intersectionVisitor(
            lineSegmentIntersector.get() );
        mPluginDCS->accept( intersectionVisitor );

        osgUtil::LineSegmentIntersector::Intersections& intersections =
            lineSegmentIntersector->getIntersections();
        if( intersections.size() == 2 )
        {
            osg::ref_ptr< osg::Drawable > drawable =
                intersections.begin()->drawable;

            ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
                drawable->getParent( 0 ) );

            temp = static_cast< ves::xplorer::scenegraph::DCS* >(
                parentVisitor.GetParentNode() );

            if( temp )
            {
                mConnectedBlocks[ "Left" ] = mBlockEntityMap[ temp->GetName() ];
                //mConnectedBlocks[ "Left" ]->SetBlockConnection( std::string( "Right" ), this );
            }
        }
    }
    //Test the near connection
    {
        lineSegmentIntersector->reset();
        lineSegmentIntersector->setStart( osg::Vec3( blockPosition.x(),
                                                     blockPosition.y() - 1,
                                                     blockPosition.z() ) );

        osgUtil::IntersectionVisitor intersectionVisitor(
            lineSegmentIntersector.get() );
        mPluginDCS->accept( intersectionVisitor );

        osgUtil::LineSegmentIntersector::Intersections& intersections =
            lineSegmentIntersector->getIntersections();
        if( intersections.size() == 2 )
        {
            osg::ref_ptr< osg::Drawable > drawable =
                intersections.begin()->drawable;

            ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
                drawable->getParent( 0 ) );

            temp = static_cast< ves::xplorer::scenegraph::DCS* >(
                parentVisitor.GetParentNode() );

            if( temp )
            {
                mConnectedBlocks[ "Near" ] = mBlockEntityMap[ temp->GetName() ];
                //mConnectedBlocks[ "Near" ]->SetBlockConnection( std::string( "Far" ), this );
            }
        }
    }
    //Test the right connection
    {
        lineSegmentIntersector->reset();
        lineSegmentIntersector->setStart( osg::Vec3( blockPosition.x() + 1,
                                                     blockPosition.y(),
                                                     blockPosition.z() ) );

        osgUtil::IntersectionVisitor intersectionVisitor(
            lineSegmentIntersector.get() );
        mPluginDCS->accept( intersectionVisitor );

        osgUtil::LineSegmentIntersector::Intersections& intersections =
            lineSegmentIntersector->getIntersections();
        if( intersections.size() == 2 )
        {
            osg::ref_ptr< osg::Drawable > drawable =
                intersections.begin()->drawable;

            ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
                drawable->getParent( 0 ) );

            temp = static_cast< ves::xplorer::scenegraph::DCS* >(
                parentVisitor.GetParentNode() );

            if( temp )
            {
                mConnectedBlocks[ "Right" ] = mBlockEntityMap[ temp->GetName() ];
                //mConnectedBlocks[ "Right" ]->SetBlockConnection( std::string( "Left" ), this );
            }
        }
    }
    //Test the far connection
    {
        lineSegmentIntersector->reset();
        lineSegmentIntersector->setStart( osg::Vec3( blockPosition.x(),
                                                     blockPosition.y() + 1,
                                                     blockPosition.z() ) );

        osgUtil::IntersectionVisitor intersectionVisitor(
            lineSegmentIntersector.get() );
        mPluginDCS->accept( intersectionVisitor );

        osgUtil::LineSegmentIntersector::Intersections& intersections =
            lineSegmentIntersector->getIntersections();
        if( intersections.size() == 2 )
        {
            osg::ref_ptr< osg::Drawable > drawable =
                intersections.begin()->drawable;

            ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
                drawable->getParent( 0 ) );

            temp = static_cast< ves::xplorer::scenegraph::DCS* >(
                parentVisitor.GetParentNode() );

            if( temp )
            {
                mConnectedBlocks[ "Far" ] = mBlockEntityMap[ temp->GetName() ];
                //mConnectedBlocks[ "Far" ]->SetBlockConnection( std::string( "Near" ), this );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bots::Block* BlockEntity::GetGeometry()
{
    return mGeometry.get();
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
    const std::string& side, bots::BlockEntity* blockEntity )
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

#if ( BULLET_MAJOR_VERSION >= 2 ) && ( BULLET_MINOR_VERSION > 61 )
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
    //Range should be small, otherwise singularities will 'explode' the constraint
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
