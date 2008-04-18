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
#include "ConstructionWorld.h"

#include "Grid.h"
#include "GridEntity.h"
#include "Block.h"
#include "BlockEntity.h"
#include "Agent.h"
#include "AgentEntity.h"
#include "ObstacleSensor.h"
#include "BlockSensor.h"
#include "SiteSensor.h"
#include "HoldBlockSensor.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/Sound.h>

#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

// --- osgAL Includes --- //
#ifdef VE_SOUND
#include <osgAL/SoundState>
#endif

// --- Bullet Includes --- //
#include "btBulletDynamicsCommon.h"

// --- C/C++ Libraries --- //
#include <iostream>
#include <ctime>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
ConstructionWorld::ConstructionWorld(
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
#ifdef VE_SOUND
    ,
    osgAL::SoundManager* soundManager
#endif
    )
:
mGrid( 0 ),
mAgents( 0 ),
mStartBlock( 0 ),
mPluginDCS( pluginDCS ),
mPhysicsSimulator( physicsSimulator )
#ifdef VE_SOUND
,
mAmbientSound( new ves::xplorer::scenegraph::Sound(
                   "AmbientSound", pluginDCS, soundManager ) )
#endif
{
    //Initialize the construction bot framework
    InitializeFramework();
}
////////////////////////////////////////////////////////////////////////////////
ConstructionWorld::~ConstructionWorld()
{
#ifdef VE_SOUND
    if( mAmbientSound )
    {
        delete mAmbientSound;
    }
#endif

    if( mGrid )
    {
        delete mGrid;   
    }

    //mStartBlock gets deleted here as well
    for( std::map< std::string, bots::BlockEntity* >::iterator itr =
         mBlockEntities.begin(); itr != mBlockEntities.end(); ++itr )
    {
        if( itr->second )
        {
            delete itr->second;
        }
    }

    for( size_t i = 0; i < mAgents.size(); ++i )
    {
        if( mAgents.at( i ) )
        {
            delete mAgents.at( i );
        }
    }

    mBlockEntities.clear();
    mAgents.clear();
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::InitializeFramework()
{
    //Seed the random number generator
    srand( time( 0 ) );

#ifdef VE_SOUND
    try
    {
        //mAmbientSound->LoadFile( "Sounds/AmbientSound.wav" );
        //mAmbientSound->GetSoundState()->setLooping( true );
    }
    catch( ... )
    {
        std::cerr << "Could not load AmbientSound.wav!" << std::endl;
    }
#endif

    std::map< std::pair< int, int >, bool > occupancyMatrix;
    int numBlocks = 20;
    int numAgents = 3;
    //Ensure that the grid size is odd for centrality purposes
    int gridSize = 51;

    int halfPosition = static_cast< int >( gridSize * 0.5f );
    for( int j = 0; j < gridSize; ++j )
    {
        for( int i = 0; i < gridSize; ++i )
        {
            bool temp = false;
            if( j == 25 && i <= 30 && i >= 20 )
            {
                temp = true;
            }

            int x =  i - halfPosition;
            int y = -j + halfPosition;
            occupancyMatrix[ std::make_pair( x, y ) ] = temp;
        }
    }

    //Tell PhysicsSimulator to store collision information
    mPhysicsSimulator->SetCollisionInformation( true );

    //Initialize the grid
    osg::ref_ptr< bots::Grid > grid = new bots::Grid();
    grid->CreateGrid( gridSize, occupancyMatrix );

    mGrid = new bots::GridEntity( grid.get(),
                                  mPluginDCS.get(),
                                  mPhysicsSimulator );
    mGrid->SetNameAndDescriptions();
    mGrid->InitPhysics();
    mGrid->GetPhysicsRigidBody()->setFriction( 1.0 );
    mGrid->GetPhysicsRigidBody()->StaticConcaveShape();

    //Initialize the starting block
    osg::ref_ptr< bots::Block > startBlock = new bots::Block();
    mStartBlock = new bots::BlockEntity( startBlock.get(),
                                         mPluginDCS.get(),
                                         mPhysicsSimulator );
    double startBlockPosition[ 3 ] = { 0, 0, 0.5 };
    mStartBlock->GetDCS()->SetTranslationArray( startBlockPosition );
    mStartBlock->InitPhysics();
    mStartBlock->GetPhysicsRigidBody()->setFriction( 1.0 );
    mStartBlock->GetPhysicsRigidBody()->StaticConcaveShape();
    mStartBlock->SetBlockEntityMap( mBlockEntities );
    mStartBlock->SetNameAndDescriptions( 0 );
    mStartBlock->SetOccupancyMatrix( occupancyMatrix );
    mBlockEntities[ mStartBlock->GetDCS()->GetName() ] = mStartBlock;
    
    //Initialize the blocks
    for( int i = 0; i < numBlocks; ++i )
    {
        //Need to check this interaction for memory leaks
        osg::ref_ptr< bots::Block > block = new bots::Block();
        bots::BlockEntity* blockEntity = new bots::BlockEntity(
            block.get(), mPluginDCS.get(), mPhysicsSimulator );

        //Set physics properties for blocks
        blockEntity->InitPhysics();
        blockEntity->GetPhysicsRigidBody()->setFriction( 1.0 );

        blockEntity->SetBlockEntityMap( mBlockEntities );

        //Set D6 constraint for blocks
        blockEntity->SetConstraints( gridSize );

        //Set name and descriptions for blocks
        blockEntity->SetNameAndDescriptions( i + 1 );

        //Set up map to blocks
        mBlockEntities[ blockEntity->GetDCS()->GetName() ] = blockEntity;
    }

    //Initialize the agents
    for( int i = 0; i < numAgents; ++i )
    {
        //Need to check this interaction for memory leaks
        osg::ref_ptr< bots::Agent > agent = new bots::Agent();
        bots::AgentEntity* agentEntity = new AgentEntity(
            agent.get(), mPluginDCS.get(), mPhysicsSimulator );

        //Set physics properties for blocks
        agentEntity->InitPhysics();
        agentEntity->GetPhysicsRigidBody()->setFriction( 1.0 );
        agentEntity->GetPhysicsRigidBody()->UserDefinedShape(
            agent->CreateCompoundShape() );
        agentEntity->SetBlockEntityMap( mBlockEntities );

        //Set D6 constraint for agents
        agentEntity->SetConstraints( gridSize );

        //Store collisions for the agents
        agentEntity->GetPhysicsRigidBody()->SetStoreCollisions( true );

        //Set the sensor range for the agents
        agentEntity->GetBlockSensor()->SetRange( gridSize * 0.25 );
        agentEntity->GetSiteSensor()->SetRange( gridSize * sqrt( 2.0 ) );

        //Set name and descriptions for blocks
        agentEntity->SetNameAndDescriptions( i );

        mAgents.push_back( agentEntity );
    }

    //Create random positions for the objects in the framework
    CreateRandomPositions( gridSize );

    //mStartBlock acts as the first block of the structure so we must attach it
    mStartBlock->AttachUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::CreateRandomPositions( int gridSize )
{
    std::vector< ves::xplorer::scenegraph::CADEntity* > entities;
    std::map< std::string, bots::BlockEntity* >::iterator itr =
        mBlockEntities.begin(); itr++;
    for( itr; itr != mBlockEntities.end(); ++itr )
    {
        entities.push_back( itr->second );
    }

    for( size_t i = 0; i < mAgents.size(); ++i )
    {
        entities.push_back( mAgents.at( i ) );
    }

    bool needsNewPosition( false );
    int posNegOne, posNegTwo;
    double randOne, randTwo;

    std::vector< std::pair< double, double > > positions;
    positions.push_back(
        std::pair< double, double >(
            mStartBlock->GetDCS()->GetVETranslationArray()[ 0 ], 
            mStartBlock->GetDCS()->GetVETranslationArray()[ 1 ] ) );

    for( size_t i = 0; i < entities.size(); ++i )
    {
        do
        {
            needsNewPosition = false;

            posNegOne = rand() % 2;
            posNegTwo = rand() % 2;

            if( posNegOne == 0 )
            {
                posNegOne = 1;
            }
            else if( posNegOne == 1 )
            {
                posNegOne = -1;
            }

            if( posNegTwo == 0 )
            {
                posNegTwo = 1;
            }
            else if( posNegTwo == 1 )
            {
                posNegTwo = -1;
            }
                                                      //Subtract block width
                                                      //to keep blocks off walls
            randOne = posNegOne * ( 0.5 * ( 1 + rand() % ( gridSize ) ) - 1.0 );
            randTwo = posNegTwo * ( 0.5 * ( 1 + rand() % ( gridSize ) ) - 1.0 );

            for( size_t j = 0; j < positions.size(); ++j )
            {
                if( ( fabs( randOne ) < ( gridSize * 0.2 ) &&
                      fabs( randTwo ) < ( gridSize * 0.2 ) ) )
                {
                    needsNewPosition = true;
                }
                else if( ( randOne > ( positions.at( j ).first - 1.0 ) &&
                           randOne < ( positions.at( j ).first + 1.0 ) )
                           ||
                         ( randTwo > ( positions.at( j ).second - 1.0 ) &&
                           randTwo < ( positions.at( j ).second + 1.0 ) ) )
                {
                    needsNewPosition = true;
                }
            }
        }
        while( needsNewPosition );

        positions.push_back( std::pair< double, double >( randOne, randTwo ) );

        double objectPosition[ 3 ] = { randOne, randTwo, 0.5 };
        entities.at( i )->GetDCS()->SetTranslationArray( objectPosition );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::CommunicatingBlocksAlgorithm()
{
    for( size_t i = 0; i < mAgents.size(); ++i )
    {
        bots::AgentEntity* agent = mAgents.at( i );
        bots::ObstacleSensorPtr obstacleSensor = agent->GetObstacleSensor();
        bots::BlockSensorPtr blockSensor = agent->GetBlockSensor();
        bots::SiteSensorPtr siteSensor = agent->GetSiteSensor();
        bots::HoldBlockSensorPtr holdBlockSensor = agent->GetHoldBlockSensor();

        holdBlockSensor->CollectInformation();
        if( !holdBlockSensor->HoldingBlock() )
        {
            blockSensor->CollectInformation();
            if( blockSensor->BlockInView() )
            {
                agent->GoToBlock();

                if( blockSensor->CloseToBlock() )
                {
                    agent->PickUpBlock();
                }
            }
            
            blockSensor->DisplayLine( true );
            siteSensor->DisplayLine( false );
        }
        else if( !agent->IsBuilding() )
        {
            siteSensor->CollectInformation();
            if( siteSensor->SiteInView() )
            {
                agent->GoToSite();

                if( siteSensor->CloseToSite() )
                {
                    agent->InitiateBuildMode();
                }
            }

            blockSensor->DisplayLine( false );
            siteSensor->DisplayLine( true );
        }
        else
        {
            agent->Build();
        }


        //Need to look at this
        //agent->WanderAround();

        obstacleSensor->CollectInformation();
        if( obstacleSensor->ObstacleDetected() )
        {
            agent->AvoidObstacle();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::PreFrameUpdate()
{
    if( !mPhysicsSimulator->GetIdle() )
    {
        CommunicatingBlocksAlgorithm();
    }
}
////////////////////////////////////////////////////////////////////////////////
