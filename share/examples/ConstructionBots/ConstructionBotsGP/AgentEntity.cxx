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
#include "AgentEntity.h"
#include "Agent.h"
#include "BlockEntity.h"
#include "BlockSensor.h"
#include "HoldBlockSensor.h"
#include "ObstacleSensor.h"
#include "PerimeterSensor.h"
#include "SiteSensor.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>

// --- Bullet Includes --- //
#include <BulletDynamics/ConstraintSolver/btGeneric6DofConstraint.h>
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

// --- C/C++ Libraries --- //
#include <sstream>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
AgentEntity::AgentEntity(
    bots::Agent* agent,
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
    :
    CADEntity( agent, pluginDCS, physicsSimulator ),
    mBuildMode( false ),
    mMaxSpeed( 3.0 ),
    mBuildSpeed( 1.0 ),
    mPluginDCS( pluginDCS ),
    mTargetDCS( 0 ),
    mConstraint( 0 ),
    mAgentGeometry( agent ),
    mHeldBlock( 0 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
AgentEntity::~AgentEntity()
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
void AgentEntity::Initialize()
{
    mBlockSensor = bots::BlockSensorPtr(
        new bots::BlockSensor( this ) );
    mHoldBlockSensor = bots::HoldBlockSensorPtr(
        new bots::HoldBlockSensor( this ) );
    mObstacleSensor = bots::ObstacleSensorPtr(
        new bots::ObstacleSensor( this ) );
    mPerimeterSensor = bots::PerimeterSensorPtr(
        new bots::PerimeterSensor( this ) );
    mSiteSensor = bots::SiteSensorPtr(
        new bots::SiteSensor( this ) );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::CommunicatingBlocksAlgorithm()
{
    mHoldBlockSensor->CollectInformation();
    if( !mHoldBlockSensor->HoldingBlock() )
    {
        mBlockSensor->CollectInformation();
        if( mBlockSensor->BlockInView() )
        {
            //GoToBlock();

            if( mBlockSensor->CloseToBlock() )
            {
                PickUpBlock();
            }
        }
    }
    else
    {
        if( mBuildMode )
        {
            mPerimeterSensor->CollectInformation();
            if( mPerimeterSensor->Aligned() )
            {
                QueryBlock();
            }

            if( mPerimeterSensor->PerimeterDetected() )
            {
                FollowPerimeter();
            }
        }
        else
        {
            mSiteSensor->CollectInformation();
            if( mSiteSensor->SiteInView() )
            {
                //GoToSite();

                if( mSiteSensor->CloseToSite() )
                {
                    InitiateBuildMode();
                }
            }
        }
    }

    mObstacleSensor->CollectInformation();
    if( mObstacleSensor->ObstacleDetected() )
    {
        AvoidObstacle();
    }
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::AvoidObstacle()
{
    //Get normalized resultant force vector and multiply by speed
    btVector3 linearVelocity =
        mObstacleSensor->GetNormalizedResultantForceVector() * mMaxSpeed;
    //Keep gravity in velocity
    linearVelocity.setZ( mPhysicsRigidBody->getLinearVelocity().getZ() );

    mPhysicsRigidBody->setLinearVelocity( linearVelocity );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::Build()
{
    btVector3 velocity = mPhysicsRigidBody->getLinearVelocity();
    velocity.normalize();

    //Get the block close to the attach site
    double* position = GetDCS()->GetVETranslationArray();
    mHeldBlock->GetDCS()->SetTranslationArray( position );
    position[ 2 ] += 1.0;
    GetDCS()->SetTranslationArray( position );

    mHeldBlock->AttachUpdate();
    position = mHeldBlock->GetDCS()->GetVETranslationArray();

    std::map< std::string, bots::BlockEntity* >::const_iterator itr;
    itr = ( *mBlockEntityMap ).begin();
    for( itr; itr != ( *mBlockEntityMap ).end(); ++itr )
    {
        if( itr->second->IsAttached() )
        {
            itr->second->UpdateSideStates();
        }
    }

    //Rotate normalized velocity vector -90 degrees
    velocity *= 0.6;
    position[ 0 ] +=  velocity.y();
    position[ 1 ] += -velocity.x();
    position[ 0 ] += -velocity.x();
    position[ 1 ] += -velocity.y();
    GetDCS()->SetTranslationArray( position );

    mBuildMode = false;
    mPerimeterSensor->Reset();
    mBlockSensor->DisplayGeometry( true );
    mObstacleSensor->DisplayGeometry( true );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::FollowPerimeter()
{
    //Get normalized resultant force vector and multiply by speed
    btVector3 linearVelocity =
        mPerimeterSensor->GetNormalizedResultantForceVector() * mBuildSpeed;
    //Keep gravity in velocity
    linearVelocity.setZ( mPhysicsRigidBody->getLinearVelocity().getZ() );

    mPhysicsRigidBody->setLinearVelocity( linearVelocity );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::GoToBlock()
{
    //Get normalized block vector and multiply by speed
    btVector3 linearVelocity =
        mBlockSensor->GetNormalizedBlockVector() * mMaxSpeed;
    //Keep gravity in velocity
    linearVelocity.setZ( mPhysicsRigidBody->getLinearVelocity().getZ() );

    mPhysicsRigidBody->setLinearVelocity( linearVelocity );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::GoToSite()
{
    //Get normalized site vector and multiply by speed
    btVector3 linearVelocity =
        mSiteSensor->GetNormalizedSiteVector() * mMaxSpeed;
    //Keep gravity in velocity
    linearVelocity.setZ( mPhysicsRigidBody->getLinearVelocity().getZ() );

    mPhysicsRigidBody->setLinearVelocity( linearVelocity );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::InitiateBuildMode()
{
    mTargetDCS = NULL;
    mBuildMode = true;

    //mObstacleSensor->SetForceAttractionConstant( 1.0 );
    mSiteSensor->DisplayGeometry( false );
    mObstacleSensor->DisplayGeometry( false );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::PickUpBlock()
{
    bots::BlockEntity* targetEntity =
        ( *mBlockEntityMap )[ mTargetDCS->GetName() ];
    bool collision = mPhysicsRigidBody->CollisionInquiry(
        targetEntity->GetPhysicsRigidBody() );
    if( collision )
    {
        mHeldBlock = targetEntity;

        double* position = mDCS->GetVETranslationArray();
        position[ 2 ] += 1.0;
        mTargetDCS->SetTranslationArray( position );

        mTargetDCS = NULL;

        mBlockSensor->DisplayGeometry( false );
        mSiteSensor->DisplayGeometry( true );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::QueryBlock()
{
    osg::Drawable* drawable = mPerimeterSensor->GetQueriedConnection();
    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
        drawable->getParent( 0 ) );
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > dcs =
        static_cast< ves::xplorer::scenegraph::DCS* >(
            parentVisitor.GetParentNode() );

    bots::BlockEntity* blockEntity = ( *mBlockEntityMap )[ dcs->GetName() ];
    if( blockEntity->PermissionToAttach( drawable ) )
    {
        Build();
    }
}
////////////////////////////////////////////////////////////////////////////////
bots::BlockSensorPtr const AgentEntity::GetBlockSensor() const
{
    return mBlockSensor;
}
////////////////////////////////////////////////////////////////////////////////
bots::HoldBlockSensorPtr const AgentEntity::GetHoldBlockSensor() const
{
    return mHoldBlockSensor;
}
////////////////////////////////////////////////////////////////////////////////
bots::ObstacleSensorPtr const AgentEntity::GetObstacleSensor() const
{
    return mObstacleSensor;
}
////////////////////////////////////////////////////////////////////////////////
bots::PerimeterSensorPtr const AgentEntity::GetPerimeterSensor() const
{
    return mPerimeterSensor;
}
////////////////////////////////////////////////////////////////////////////////
bots::SiteSensorPtr const AgentEntity::GetSiteSensor() const
{
    return mSiteSensor;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* const AgentEntity::GetPluginDCS() const
{
    return mPluginDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* const AgentEntity::GetTargetDCS() const
{
    return mTargetDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::SetBlockEntityMap(
    std::map< std::string, bots::BlockEntity* >* blockEntityMap )
{
    mBlockEntityMap = blockEntityMap;
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::SetConstraints( int gridSize )
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
    //Range should be small or singularities will 'explode' the constraint
    mConstraint->setAngularLowerLimit( btVector3( 0, 0, 0 ) );
    mConstraint->setAngularUpperLimit( btVector3( 0, 0, 0 ) );

    mPhysicsSimulator->GetDynamicsWorld()->addConstraint( mConstraint );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::SetNameAndDescriptions( int number )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    mDCS->setDescriptions( descriptorsList );

    std::stringstream ss;
    ss << "Agent" << number;
    std::cout << ss.str() << std::endl;
    mDCS->setName( ss.str() );
}
////////////////////////////////////////////////////////////////////////////////
void AgentEntity::SetTargetDCS( ves::xplorer::scenegraph::DCS* targetDCS )
{
    mTargetDCS = targetDCS;
}
////////////////////////////////////////////////////////////////////////////////
