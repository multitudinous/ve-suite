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

const double piDivOneEighty = 0.0174532925;
const double oneEightyDivPI = 57.2957795;

////////////////////////////////////////////////////////////////////////////////
ConstructionWorld::ConstructionWorld(
    ves::xplorer::scenegraph::DCS* pluginDCS,
#ifdef VE_SOUND
    osgAL::SoundManager* soundManager,
#endif
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
    :
    mFrameCount( 1 ),
    mNumSimulations( 10 ),
    mNumSimulationsLeft( mNumSimulations ),
    mNumBlocks( 24 ),
    mNumBlocksLeft( mNumBlocks ),
    mNumAgents( 1 ),
    mMaxNumAgents( 24 ),
    mDeltaAgents( 1 ),
    mMinBlockSensorRange( 10.0 ),
    // --- This gets calculated later based off the grid size --- //
    mMaxBlockSensorRange( 0.0 ),
    mDeltaBlockSensorRange( 2.0 ),
    mBlockSensorRange( mMinBlockSensorRange ),
    // --- Ensure that the grid size is odd for centrality purposes --- //
    mGridSize( 51 ),
    mGrid( NULL ),
    mAgentEntities(),
    mStartBlock( NULL ),
    mPluginDCS( pluginDCS ),
#ifdef VE_SOUND
    mSoundManager( soundManager ),
    mAmbientSound( new ves::xplorer::scenegraph::Sound(
                       "AmbientSound", pluginDCS, soundManager ) ),
#endif
    mPhysicsSimulator( physicsSimulator ),
    mSimulationData( "Data/simulation.dat", std::ios::out | std::ios::app )
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
    //If mGridSize is not odd, exit the program
    if( !( mGridSize % 2 ) )
    {
        std::cout << "Make sure that the grid size is odd!!!" << std::endl;
        exit( 0 );
    }

    //Seed the random number generator
    srand( time( 0 ) );

    //Write out initial framework setup
    if( mBlockSensorRange == mMinBlockSensorRange )
    {
        mSimulationData
            << std::setiosflags( std::ios::left )
            << "GridSize = " << mGridSize
            << std::endl
            << std::setiosflags( std::ios::left )
            << "NumBlocks = " << mNumBlocks
            << std::endl
            << std::setiosflags( std::ios::left )
            << "NumAgents = " << mNumAgents
            << std::endl
            << std::setiosflags( std::ios::left )
            << "BlockSensorRange = " << mBlockSensorRange
            << std::endl;
    }

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

    //Initialize the occupancy matrix
    {
        int halfPosition = static_cast< int >( mGridSize * 0.5 );
        for( unsigned int j = 0; j < mGridSize; ++j )
        {
            for( unsigned int i = 0; i < mGridSize; ++i )
            {
                int x = -halfPosition + i;
                int y =  halfPosition - j;
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
        mGrid->GetPhysicsRigidBody()->setFriction( 0.5 );
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
        mStartBlock->GetPhysicsRigidBody()->setFriction( 0.5 );
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
    for( unsigned int i = 0; i < mNumBlocks; ++i )
    {
        //Need to check this interaction for memory leaks
        osg::ref_ptr< bots::Block > block = new bots::Block();
        bots::BlockEntity* blockEntity = new bots::BlockEntity(
            block.get(), mPluginDCS.get(), mPhysicsSimulator );

        //Set physics properties for blocks
        blockEntity->InitPhysics();
        blockEntity->GetPhysicsRigidBody()->setFriction( 0.5 );

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
    for( unsigned int i = 0; i < mNumAgents; ++i )
    {
        CreateAgentEntity();
    }

    //Create random positions for the objects in the framework
    CreateRandomPositions();

    //Calculate maximum useful length of the block sensor range for mGridSize
    CalculateMaxBlockSensorRange();

    //Kick off simulation by attaching the start block after positions are set
    {
        double startBlockPosition[ 3 ] = { 0.0, 0.0, 0.5 };
        mStartBlock->GetDCS()->SetTranslationArray( startBlockPosition );
        mStartBlock->SetOccupancyMatrix( mOccupancyMatrix );
        mStartBlock->AttachUpdate( true );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::CalculateMaxBlockSensorRange()
{
    double a1( 0.0 ), b1( 0.0 ), c1( 0.0 );
    a1 = sqrt( 2.0 * pow( 0.5, 2 ) );
    b1 = sqrt( 2.0 * pow( mGridSize * 0.5, 2 ) );
    c1 = sqrt( pow( a1, 2 ) + pow( b1, 2 ) );
    //std::cout << c1 << std::endl;

    double alpha( 0.0 ), beta( 0.0 ), theta( 0.0 );
    alpha = asin( a1 / c1 );
    alpha *= oneEightyDivPI;
    beta = 135.0 - alpha;
    theta = 180.0 - beta;
    //std::cout << theta << std::endl;

    double a2( 0.0 ), b2( 0.0 ), c2( 0.0 );
    a2 = ( mGridSize * 0.5 ) - 0.5;
    c2 = a2 / sin( theta * piDivOneEighty );
    //std::cout << c2 << std::endl;

    mMaxBlockSensorRange = c1 + c2 - ( 2.0 * a1 );
    mMaxBlockSensorRange = ceil( mMaxBlockSensorRange );
    //std::cout << mMaxBlockSensorRange << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::CreateAgentEntity()
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
    agentEntity->SetNumBlocksLeft( mNumBlocksLeft );

    //Set physics properties for blocks
    agentEntity->InitPhysics();
    agentEntity->GetPhysicsRigidBody()->setFriction( 0.5 );
    agentEntity->GetPhysicsRigidBody()->UserDefinedShape(
        agent->CreateCompoundShape() );
    agentEntity->SetBlockEntityMap( mBlockEntities );

    //Set D6 constraint for agents
    agentEntity->SetConstraints( mGridSize );

    //Store collisions for the agents
    agentEntity->GetPhysicsRigidBody()->SetStoreCollisions( true );

    //Set the sensor range for the agents
    agentEntity->GetBlockSensor()->SetRange( mBlockSensorRange );
    agentEntity->GetSiteSensor()->SetRange( sqrt( 2.0 ) * 0.5 * mGridSize );
    agentEntity->GetObstacleSensor()->SetRange( sqrt( 2.0 ) * mGridSize );
    agentEntity->Reset();

    mAgentEntities.push_back( agentEntity );

    //Set name and descriptions for blocks
    agentEntity->SetNameAndDescriptions( mAgentEntities.size() );
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::CreateRandomPositions()
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

            if( !posNegOne )
            {
                posNegOne = 1;
            }
            else
            {
                posNegOne = -1;
            }

            if( !posNegTwo )
            {
                posNegTwo = 1;
            }
            else
            {
                posNegTwo = -1;
            }
            
            //Subtract 4.0 to keep blocks off walls and allow agents an escape
            randOne = posNegOne * ( 0.5 * ( rand() % ( mGridSize + 1 ) ) - 5.0 );
            randTwo = posNegTwo * ( 0.5 * ( rand() % ( mGridSize + 1 ) ) - 5.0 );

            for( size_t j = 0; j < positions.size(); ++j )
            {
                if( ( fabs( randOne ) < ( mGridSize * 0.1 ) &&
                      fabs( randTwo ) < ( mGridSize * 0.1 ) ) )
                {
                    needsNewPosition = true;
                }
                else if( ( randOne > ( positions.at( j ).first - 1.1 ) &&
                           randOne < ( positions.at( j ).first + 1.1 ) )
                           &&
                         ( randTwo > ( positions.at( j ).second - 1.1 ) &&
                           randTwo < ( positions.at( j ).second + 1.1 ) ) )
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
void ConstructionWorld::PreFrameUpdate()
{
    if( !mPhysicsSimulator->GetIdle() )
    {
        for( size_t i = 0; i < mAgentEntities.size(); ++i )
        {
            mAgentEntities.at( i )->CommunicatingBlocksAlgorithm();
        }
    }

    if( mNumBlocksLeft )
    {
        mFrameCount++;
    }
    else
    {
        if( !RUN_CONTINUOUS_SIMULATIONS )
        {
            return;
        }
    
        //Decrement the simulation count
        --mNumSimulationsLeft;
        
        //Reset the simulation
        ResetSimulation();
    }
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionWorld::ResetSimulation()
{
    //Write simulation data to file
    mSimulationData << std::setiosflags( std::ios::left )
                    << mFrameCount << "\t";

    //Reset the simulation time and block counter
    mFrameCount = 1;
    mNumBlocksLeft = mNumBlocks;

    if( !mNumSimulationsLeft )
    {
        mNumSimulationsLeft = mNumSimulations;

        if( mBlockSensorRange < mMaxBlockSensorRange )
        {
            mBlockSensorRange += mDeltaBlockSensorRange;

            //Set the sensor range for the agents
            for( size_t i = 0; i < mAgentEntities.size(); ++i )
            {
                mAgentEntities.at( i )->GetBlockSensor()->SetRange(
                    mBlockSensorRange );
            }

            mSimulationData
                << std::setiosflags( std::ios::left )
                << std::endl
                << "BlockSensorRange = " << mBlockSensorRange
                << std::endl;
        }
        else
        {
            mBlockSensorRange = mMinBlockSensorRange;
            if( mNumAgents < mMaxNumAgents )
            {
                mNumAgents += mDeltaAgents;
                CreateAgentEntity();

                mSimulationData
                    << std::setiosflags( std::ios::left )
                    << std::endl << std::endl
                    << "GridSize = " << mGridSize
                    << std::endl
                    << std::setiosflags( std::ios::left )
                    << "NumBlocks = " << mNumBlocks
                    << std::endl
                    << std::setiosflags( std::ios::left )
                    << "NumAgents = " << mNumAgents
                    << std::endl
                    << std::setiosflags( std::ios::left )
                    << "BlockSensorRange = " << mBlockSensorRange
                    << std::endl;
            }
            else
            {
                mSimulationData
                    << std::setiosflags( std::ios::left )
                    << "Simulation Complete"
                    << std::endl;
                std::cout << "Done!!!!!!!!!!!!!!!!!!!!!!!!!" << std::endl;
                exit( 0 );
            }
        }
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
    CreateRandomPositions();

    //Kick off simulation by attaching the start block after positions are set
    {
        double startBlockPosition[ 3 ] = { 0.0, 0.0, 0.50 };
        mStartBlock->GetDCS()->SetTranslationArray( startBlockPosition );
        mStartBlock->GetPhysicsRigidBody()->StaticConcaveShape();
        mStartBlock->SetOccupancyMatrix( mOccupancyMatrix );
        mStartBlock->AttachUpdate( true );
    }
}
////////////////////////////////////////////////////////////////////////////////
