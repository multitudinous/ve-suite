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
#include "World.h"

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
World::World( ves::xplorer::scenegraph::DCS* pluginDCS,
              ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
#ifdef VE_SOUND
            , osgAL::SoundManager* soundManager
#endif
              )
:
m_structureNotComplete( true ),
m_grid( 0 ),
m_blocks( 0 ),
m_agents( 0 ),
m_startBlock( 0 ),
m_pluginDCS( pluginDCS ),
m_physicsSimulator( physicsSimulator )
#ifdef VE_SOUND
, m_ambientSound( new ves::xplorer::scenegraph::Sound( "AmbientSound", pluginDCS, soundManager ) )
#endif
{
    //Seed the random number generator
    srand( time( 0 ) );

    //Initialize the construction bot framework
    InitFramework();
}
////////////////////////////////////////////////////////////////////////////////
World::~World()
{
#ifdef VE_SOUND
    if( m_ambientSound )
    {
        delete m_ambientSound;
    }
#endif

    if( m_grid )
    {
        delete m_grid;   
    }

    //m_startBlock is added under m_blocks vector,
    //so it gets deleted in here
    for( size_t i = 0; i < m_blocks.size(); ++i )
    {
        if( m_blocks.at( i ) )
        {
            delete m_blocks.at( i );
        }
    }

    for( size_t i = 0; i < m_agents.size(); ++i )
    {
        if( m_agents.at( i ) )
        {
            delete m_agents.at( i );
        }
    }

    m_entities.clear();
    m_blocks.clear();
    m_agents.clear();
}
////////////////////////////////////////////////////////////////////////////////
void World::InitFramework()
{
#ifdef VE_SOUND
    try
    {
        //m_ambientSound->LoadFile( "Sounds/AmbientSound.wav" );
        //m_ambientSound->GetSoundState()->setLooping( true );
    }
    catch( ... )
    {
        std::cerr << "Could not load AmbientSound.wav!" << std::endl;
    }
#endif

    std::map< std::pair< int, int >, bool > occMatrix;
    int numBlocks = 36;
    int numAgents = 1;
    //Ensure that the grid size is odd for centrality purposes
    int gridSize = 51;

    int halfPosition = static_cast< int >( gridSize * 0.5f );
    for( int j = 0; j < gridSize; ++j )
    {
        for( int i = 0; i < gridSize; ++i )
        {
            occMatrix.insert( std::make_pair( std::make_pair( 
                i - halfPosition, -j + halfPosition ), false ) );
        }
    }

    //Tell PhysicsSimulator to store collision information
    m_physicsSimulator->SetCollisionInformation( true );

    //Initialize the grid
    m_grid = new bots::GridEntity(
        new bots::Grid( gridSize, occMatrix ),
        m_pluginDCS.get(),
        m_physicsSimulator );
    m_grid->SetNameAndDescriptions();
    m_grid->InitPhysics();
    m_grid->GetPhysicsRigidBody()->setFriction( 0.0 );
    m_grid->GetPhysicsRigidBody()->StaticConcaveShape();
    m_entities[ m_grid->GetDCS()->GetName() ] = m_grid;

    //Initialize the starting block
    m_startBlock = new bots::BlockEntity(
        new bots::Block(),
        m_pluginDCS.get(),
        m_physicsSimulator );
    m_startBlock->GetGeometry()->SetColor( 0.0, 0.0, 0.0, 1.0 );
    //Set name and descriptions for blocks
    m_startBlock->SetNameAndDescriptions( 0 );
    double startBlockPosition[ 3 ] = { 0, 0, 0.5 };
    m_startBlock->GetDCS()->SetTranslationArray( startBlockPosition );
    
    m_startBlock->InitPhysics();
    m_startBlock->GetPhysicsRigidBody()->setFriction( 0.0 );
    m_startBlock->GetPhysicsRigidBody()->StaticConcaveShape();
    //Set up map to entities
    m_entities[ m_startBlock->GetDCS()->GetName() ] = m_startBlock;
    
    //Initialize the blocks
    for( int i = 0; i < numBlocks; ++i )
    {
        m_blocks.push_back( new bots::BlockEntity(
            new bots::Block(),
            m_pluginDCS.get(),
            m_physicsSimulator ) );
    }

    //Initialize the agents
    for( int i = 0; i < numAgents; ++i )
    {
        m_agents.push_back( new AgentEntity(
            new bots::Agent(),
            m_pluginDCS.get(),
            m_physicsSimulator ) );
    }

    for( size_t i = 0; i < m_blocks.size(); ++i )
    {
        //Set name and descriptions for blocks
        m_blocks.at( i )->SetNameAndDescriptions( i + 1 );

        //Set physics properties for blocks
        m_blocks.at( i )->InitPhysics();
        m_blocks.at( i )->GetPhysicsRigidBody()->setFriction( 0.0 );

        //Set D6 constraint for blocks
        m_blocks.at( i )->SetConstraints( gridSize );

        //Set up map to entities
        m_entities[ m_blocks.at( i )->GetDCS()->GetName() ] = m_blocks.at( i );
    }

    for( size_t i = 0; i < m_agents.size(); ++i )
    {
        //Set name and descriptions for blocks
        m_agents.at( i )->SetNameAndDescriptions( i );

        //Set physics properties for blocks
        m_agents.at( i )->InitPhysics();
        m_agents.at( i )->GetPhysicsRigidBody()->setFriction( 0.0 );

        //Set D6 constraint for agents
        m_agents.at( i )->SetConstraints( gridSize );

        //Store collisions for the agents
        m_agents.at( i )->GetPhysicsRigidBody()->SetStoreCollisions( true );

        //Set the sensor range for the agents
        m_agents.at( i )->GetBlockSensor()->SetRange( gridSize * 0.25 );
        m_agents.at( i )->GetSiteSensor()->SetRange( gridSize * sqrt( 2.0 ) );

        //Set up map to entities
        m_entities[ m_agents.at( i )->GetDCS()->GetName() ] = m_agents.at( i );
    }

    //Create random positions for the objects in the framework
    CreateRandomPositions( gridSize );

    //Push back the starting block
    m_blocks.push_back( m_startBlock );
}
////////////////////////////////////////////////////////////////////////////////
void World::CreateRandomPositions( int gridSize )
{
    std::vector< ves::xplorer::scenegraph::CADEntity* > objects;
    for( size_t i = 0; i < m_blocks.size(); ++i )
    {
        objects.push_back( m_blocks.at( i ) );
    }

    for( size_t i = 0; i < m_agents.size(); ++i )
    {
        objects.push_back( m_agents.at( i ) );
    }

    bool needsNewPosition( false );
    int posNegOne, posNegTwo;
    double randOne, randTwo;

    std::vector< std::pair< double, double > > positions;
    positions.push_back( std::pair< double, double >
        ( m_startBlock->GetDCS()->GetVETranslationArray()[ 0 ], 
          m_startBlock->GetDCS()->GetVETranslationArray()[ 1 ] ) );

    for( size_t i = 0; i < objects.size(); ++i )
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
        objects.at( i )->GetDCS()->SetTranslationArray( objectPosition );
    }
}
////////////////////////////////////////////////////////////////////////////////
void World::CommunicatingBlocksAlgorithm()
{
    for( size_t i = 0; i < m_agents.size(); ++i )
    {
        bots::AgentEntity* agent = m_agents.at( i );
        bots::ObstacleSensor* obstacleSensor = agent->GetObstacleSensor();
        bots::BlockSensor* blockSensor = agent->GetBlockSensor();
        bots::SiteSensor* siteSensor = agent->GetSiteSensor();
        bots::HoldBlockSensor* holdBlockSensor = agent->GetHoldBlockSensor();

        //Remove reference lines
        blockSensor->RemoveLine();
        siteSensor->RemoveLine();

        /*
        holdBlockSensor->CollectInformation();
        if( holdBlockSensor->HoldingBlock() )
        {
            siteSensor->CollectInformation();
            if( siteSensor->SiteInView() )
            {
                if( siteSensor->CloseToSite() )
                {
                    bots::BlockEntity* targetEntity = static_cast< bots::BlockEntity* >
                        ( m_entities[ agent->GetTargetDCS()->GetName() ] );
                    bool collision = agent->GetPhysicsRigidBody()->
                        CollisionInquiry( targetEntity->GetPhysicsRigidBody() );
                    if( collision )
                    {
                        //agent->SetBuildMode( true );
                        //agent->Get
                    }
                }

                agent->GoToSite();
            }
        }
        else
        */
        {
            blockSensor->CollectInformation();
            if( blockSensor->BlockInView() )
            {
                //if( blockSensor->CloseToBlock() )
                {
                    bots::BlockEntity* targetEntity = static_cast< bots::BlockEntity* >
                        ( m_entities[ agent->GetTargetDCS()->GetName() ] );
                    bool collision = agent->GetPhysicsRigidBody()->
                        CollisionInquiry( targetEntity->GetPhysicsRigidBody() );
                    if( collision )
                    {
                        agent->PickUpBlock( targetEntity );
                    }
                }
                
                agent->GoToBlock();
            }
        }

        /*
        agent->WanderAround();

        obstacleSensor->CollectInformation();
        if( obstacleSensor->ObstacleDetected() )
        {
            agent->AvoidObstacle();
        }
        */

    }
}
////////////////////////////////////////////////////////////////////////////////
void World::PreFrameUpdate()
{
    if( m_structureNotComplete )
    {
        CommunicatingBlocksAlgorithm();
    }
}
////////////////////////////////////////////////////////////////////////////////
