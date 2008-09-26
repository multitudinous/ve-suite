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
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
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
#include <iomanip>
#include <ctime>

using namespace bots;

const bool RUN_CONTINUOUS_SIMULATIONS = true;

////////////////////////////////////////////////////////////////////////////////
ConstructionWorld::ConstructionWorld(
    ves::xplorer::scenegraph::DCS* pluginDCS,
#ifdef VE_SOUND
    osgAL::SoundManager* soundManager,
#endif
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
    :
    mBlocksLeft( 0 ),
    mFrameCount( 1 ),
    mGridSize( 0 ),
    mNumBlocks( 0 ),
    mNumAgents( 0 ),
    mGrid( 0 ),
    mAgentEntities( 0 ),
    mStartBlock( 0 ),
    mPluginDCS( pluginDCS ),
#ifdef VE_SOUND
    mSoundManager( soundManager ),
    mAmbientSound( new ves::xplorer::scenegraph::Sound(
                       "AmbientSound", pluginDCS, soundManager ) ),
#endif
    mPhysicsSimulator( physicsSimulator ),
    mSimulationData( "Data\\simulation.dat", std::ios::out )
{
    //Initialize the construction bot framework
    InitializeFramework();
}
////////////////////////////////////////////////////////////////////////////////
ConstructionWorld::~ConstructionWorld()
{
#ifdef VE_SOUND
    //Delete the sounds
    if( mAmbientSound )
    {
        delete mAmbientSound;
    }
#endif

    //Delete the grid entity
    if( mGrid )
    {
        delete mGrid;
    }

    //Delete the block entitites
    //mStartBlock gets deleted here as well
    for( std::map< std::string, bots::BlockEntity* >::iterator itr =
         mBlockEntities.begin(); itr != mBlockEntities.end(); ++itr )
    {
        if( itr->second )
        {
            delete itr->second;
        }
    }

    //Delete the AgentEntities
    for( size_t i = 0; i < mAgentEntities.size(); ++i )
    {
        if( mAgentEntities.at( i ) )
        {
            delete mAgentEntities.at( i );
        }
    }

    //Clear the containers
    mBlockEntities.clear();
    mAgentEntities.clear();

    //Close the simulation data file
    mSimulationData.close();
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::InitializeFramework()
{
    //Seed the random number generator
    srand( time( 0 ) );

#ifdef VE_SOUND
    try
    {
        mAmbientSound->LoadFile( "Sounds/AmbientSound.wav" );
    }
    catch( ... )
    {
        std::cerr << "Could not load AmbientSound.wav!" << std::endl;
    }

    //mAmbientSound->GetSoundState()->setPlay( true );
    mAmbientSound->GetSoundState()->setAmbient( false );
#endif //VE_SOUND

    //Tell PhysicsSimulator to store collision information
    mPhysicsSimulator->SetCollisionInformation( true );

    //Ensure that the grid size is odd for centrality purposes
    mGridSize = 51;

    //Initialize the occupancy matrix
    {
        int halfPosition = static_cast< int >( mGridSize * 0.5 );
        for( int j = 0; j < mGridSize; ++j )
        {
            for( int i = 0; i < mGridSize; ++i )
            {
                int x =  i - halfPosition;
                int y = -j + halfPosition;
                mOccupancyMatrix[ std::make_pair( x, y ) ] =
                    std::make_pair( false, false );
            }
        }

        //Program the desired structure into the occupancy matrix
        mOccupancyMatrix[ std::make_pair(  0,  0 ) ].first = true;

        mOccupancyMatrix[ std::make_pair(  1,  0 ) ].first = true;
        mOccupancyMatrix[ std::make_pair(  0,  1 ) ].first = true;
        mOccupancyMatrix[ std::make_pair( -1,  0 ) ].first = true;
        mOccupancyMatrix[ std::make_pair(  0, -1 ) ].first = true;

        mOccupancyMatrix[ std::make_pair(  1,  1 ) ].first = true;
        mOccupancyMatrix[ std::make_pair( -1,  1 ) ].first = true;
        mOccupancyMatrix[ std::make_pair( -1, -1 ) ].first = true;
        mOccupancyMatrix[ std::make_pair(  1, -1 ) ].first = true;

        mOccupancyMatrix[ std::make_pair(  2,  0 ) ].first = true;
        mOccupancyMatrix[ std::make_pair(  0,  2 ) ].first = true;
        mOccupancyMatrix[ std::make_pair( -2,  0 ) ].first = true;
        mOccupancyMatrix[ std::make_pair(  0, -2 ) ].first = true;

        mOccupancyMatrix[ std::make_pair(  1,  2 ) ].first = true;
        mOccupancyMatrix[ std::make_pair( -1,  2 ) ].first = true;
        mOccupancyMatrix[ std::make_pair( -1, -2 ) ].first = true;
        mOccupancyMatrix[ std::make_pair(  1, -2 ) ].first = true;

        mOccupancyMatrix[ std::make_pair(  2,  1 ) ].first = true;
        mOccupancyMatrix[ std::make_pair( -2,  1 ) ].first = true;
        mOccupancyMatrix[ std::make_pair( -2, -1 ) ].first = true;
        mOccupancyMatrix[ std::make_pair(  2, -1 ) ].first = true;

        mOccupancyMatrix[ std::make_pair(  3,  0 ) ].first = true;
        mOccupancyMatrix[ std::make_pair(  0,  3 ) ].first = true;
        mOccupancyMatrix[ std::make_pair( -3,  0 ) ].first = true;
        mOccupancyMatrix[ std::make_pair(  0, -3 ) ].first = true;
    }

    //Initialize the grid
    {
        osg::ref_ptr< bots::Grid > grid = new bots::Grid();
        grid->CreateGrid( mGridSize, mOccupancyMatrix );
        mGrid = new bots::GridEntity(
            grid.get(), mPluginDCS.get(), mPhysicsSimulator );
        mGrid->SetNameAndDescriptions();
        mGrid->InitPhysics();
        mGrid->GetPhysicsRigidBody()->setFriction( 0.0 );
        mGrid->GetPhysicsRigidBody()->StaticConcaveShape();
    }

    //Initialize the starting block
    {
        //Need to check this interaction for memory leaks
        osg::ref_ptr< bots::Block > startBlock = new bots::Block();
        mStartBlock = new bots::BlockEntity(
            startBlock.get(), mPluginDCS.get(), mPhysicsSimulator );

        //Set physics properties for start block
        mStartBlock->InitPhysics();
        mStartBlock->GetPhysicsRigidBody()->setFriction( 1.0 );
        mStartBlock->GetPhysicsRigidBody()->StaticConcaveShape();

        //Set D6 constraint for start block
        mStartBlock->SetConstraints( mGridSize );

        //Give the start block access to the BlockEntity map
        mStartBlock->SetBlockEntityMap( mBlockEntities );

        //Set name and descriptions for start block
        mStartBlock->SetNameAndDescriptions( 0 );

        //Add start block to the BlockEntity map
        mBlockEntities[ mStartBlock->GetDCS()->GetName() ] = mStartBlock;
    }

    //Initialize the blocks
    mNumBlocks = 24;
    mBlocksLeft = mNumBlocks;
    for( int i = 0; i < mNumBlocks; ++i )
    {
        //Need to check this interaction for memory leaks
        osg::ref_ptr< bots::Block > block = new bots::Block();
        bots::BlockEntity* blockEntity = new bots::BlockEntity(
            block.get(), mPluginDCS.get(), mPhysicsSimulator );

        //Set physics properties for blocks
        blockEntity->InitPhysics();
        blockEntity->GetPhysicsRigidBody()->setFriction( 1.0 );

        //Set D6 constraint for blocks
        blockEntity->SetConstraints( mGridSize );

        //Give block entities access to the BlockEntity map
        blockEntity->SetBlockEntityMap( mBlockEntities );

        //Set name and descriptions for blocks
        blockEntity->SetNameAndDescriptions( i + 1 );

        //Add block to the BlockEntity map
        mBlockEntities[ blockEntity->GetDCS()->GetName() ] = blockEntity;
    }

    //Initialize the agents
    mNumAgents = 4;
    for( int i = 0; i < mNumAgents; ++i )
    {
        //Need to check this interaction for memory leaks
        osg::ref_ptr< bots::Agent > agent = new bots::Agent();
#ifdef VE_SOUND
        bots::AgentEntity* agentEntity = new AgentEntity(
            agent.get(), mPluginDCS.get(), mSoundManager, mPhysicsSimulator );
#else
        bots::AgentEntity* agentEntity = new AgentEntity(
            agent.get(), mPluginDCS.get(), mPhysicsSimulator );
#endif
        //Set number of blocks left to be placed
        agentEntity->SetBlocksLeft( mBlocksLeft );

        //Set physics properties for blocks
        agentEntity->InitPhysics();
        agentEntity->GetPhysicsRigidBody()->setFriction( 1.0 );
        agentEntity->GetPhysicsRigidBody()->UserDefinedShape(
            agent->CreateCompoundShape() );
        agentEntity->SetBlockEntityMap( mBlockEntities );

        //Set D6 constraint for agents
        agentEntity->SetConstraints( mGridSize );

        //Store collisions for the agents
        agentEntity->GetPhysicsRigidBody()->SetStoreCollisions( true );

        //Set the sensor range for the agents
        agentEntity->GetBlockSensor()->SetRange( mGridSize * 0.3 );
        agentEntity->GetObstacleSensor()->SetRange( sqrt( 2.0 ) * mGridSize );
        agentEntity->GetSiteSensor()->SetRange( sqrt( 2.0 ) * 0.5 * mGridSize );

        //Set name and descriptions for blocks
        agentEntity->SetNameAndDescriptions( i );

        mAgentEntities.push_back( agentEntity );
    }

    //Create random positions for the objects in the framework
    CreateRandomPositions( mGridSize );

    //Kick off simulation by attaching the start block after positions are set
    {
        double startBlockPosition[ 3 ] = { 0, 0, 0.5 };
        mStartBlock->GetDCS()->SetTranslationArray( startBlockPosition );
        mStartBlock->SetOccupancyMatrix( mOccupancyMatrix );
        mStartBlock->AttachUpdate( true );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::CreateRandomPositions( int mGridSize )
{
    std::vector< ves::xplorer::scenegraph::CADEntity* > entities;
    std::map< std::string, bots::BlockEntity* >::iterator itr =
        mBlockEntities.begin(); ++itr;
    for( itr; itr != mBlockEntities.end(); ++itr )
    {
        entities.push_back( itr->second );
    }

    for( size_t i = 0; i < mAgentEntities.size(); ++i )
    {
        entities.push_back( mAgentEntities.at( i ) );
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
                                                      //Subtract some amount
                                                      //to keep blocks off walls
            randOne = posNegOne * ( 0.5 * ( 1 + rand() % ( mGridSize ) ) - 2.0 );
            randTwo = posNegTwo * ( 0.5 * ( 1 + rand() % ( mGridSize ) ) - 2.0 );

            for( size_t j = 0; j < positions.size(); ++j )
            {
                if( ( fabs( randOne ) < ( mGridSize * 0.2 ) &&
                      fabs( randTwo ) < ( mGridSize * 0.2 ) ) )
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
void ConstructionWorld::WriteSimulationDataToFile()
{
    mSimulationData << std::setiosflags( std::ios::left )
                    << mFrameCount << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::PreFrameUpdate()
{
    if( !mPhysicsSimulator->GetIdle() )
    {
        for( size_t i = 0; i < mAgentEntities.size(); ++i )
        {
            mAgentEntities.at( i )->CommunicatingBlocksAlgorithm();
        }
    }

    if( mBlocksLeft )
    {
        mFrameCount++;
    }
    else
    {
        //Write simulation data to file
        WriteSimulationDataToFile();

        //Reset the simulation
        ResetSimulation();
    }
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::ResetSimulation()
{
    //Reset the simulation time and block counter
    mFrameCount = 1;
    mBlocksLeft = mNumBlocks;
    if( !RUN_CONTINUOUS_SIMULATIONS )
    {
        return;
    }

    //Reset the occupancy matrix
    std::map< std::pair< int, int >, std::pair< bool, bool > >::iterator
        occMatItr = mOccupancyMatrix.begin();
    for( occMatItr; occMatItr != mOccupancyMatrix.end(); ++occMatItr )
    {
        occMatItr->second.second = false;
    }

    //Reset the grid
    //Nothing needed to reset grid at this time

    //Reset the blocks
    std::map< std::string, bots::BlockEntity* >::iterator blockIter =
        mBlockEntities.begin();
    for( blockIter; blockIter != mBlockEntities.end(); ++blockIter )
    {
        blockIter->second->Reset();
    }

    //Reset the agents
    for( size_t i = 0; i < mAgentEntities.size(); ++i )
    {
        mAgentEntities.at( i )->Reset();
    }

    //Create random positions for the objects in the framework
    CreateRandomPositions( mGridSize );

    //Kick off simulation by attaching the start block after positions are set
    {
        double startBlockPosition[ 3 ] = { 0, 0, 0.5 };
        mStartBlock->GetDCS()->SetTranslationArray( startBlockPosition );
        mStartBlock->GetPhysicsRigidBody()->StaticConcaveShape();
        mStartBlock->SetOccupancyMatrix( mOccupancyMatrix );
        mStartBlock->AttachUpdate( true );
    }
}
////////////////////////////////////////////////////////////////////////////////
