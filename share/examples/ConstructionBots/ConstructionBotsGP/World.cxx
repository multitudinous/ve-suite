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
#include <fstream>
#include <ctime>

using namespace Construction;

////////////////////////////////////////////////////////////////////////////////
World::World( int worldScale,
              ves::xplorer::scenegraph::DCS* pluginDCS,
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
m_worldScale( worldScale ),
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

    if( m_startBlock )
    {
        delete m_startBlock;
    }

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
        m_ambientSound->LoadFile( "Sounds/AmbientSound.wav" );
        m_ambientSound->GetSoundState()->setLooping( true );
    }
    catch( ... )
    {
        std::cerr << "Could not load AmbientSound.wav!" << std::endl;
    }
#endif

    //Ifstream constructor opens the occupancy matrix file
    std::ifstream occMatFile( "Data/omat.txt", std::ios::in );			
	
    //Test for valid file
    if( !occMatFile )
    {														
	    std::cerr << "File could not be opened\n";

	    exit( 1 );
    }

    int itr = -1;
    int startItr = 0;
    int numBlocks = 0;
    int numAgents = 0;
    char tempChar = 0;

    std::vector< bool > occMatrix;

    while( occMatFile >> tempChar )
    {
        if( itr == -1 )
	    {
            numAgents = atoi( &tempChar );
        }
	    else if( tempChar == '0' )
	    {
		    occMatrix.push_back( false );
	    }
	    else if( tempChar == '1' )
	    {
		    occMatrix.push_back( true );
            ++numBlocks;     
	    }
	    else if( tempChar == 'S' )
	    {
            startItr = itr + 1;
		    occMatrix.push_back( true );
	    }

        ++itr;
    }

    //Ensure that the grid size is odd for centrality purposes
    if( occMatrix.size() % 2 )
    {
        std::cout << "Occupancy matrix successfully initialized!" << std::endl;
    }
    else
    {
        std::cout << "Error in initializing occupancy matrix!" << std::endl;
        std::cout << "Please make the grid size odd for centrality purposes!" << std::endl;

        exit( 1 );
    }

    double startBlockPosition[ 3 ] = { 0, 0, m_worldScale * 0.5 };
    int gridSize = sqrt( static_cast< double >( occMatrix.size() ) );
    int modulus = startItr % gridSize;
 
    startBlockPosition[ 0 ] = modulus - ( ( gridSize * 0.5 ) + ( m_worldScale * 0.5 ) );
    startBlockPosition[ 1 ] = ( ( gridSize * 0.5 ) - ( m_worldScale * 0.5 ) ) - ( ( startItr - modulus ) / gridSize );

    //Tell PhysicsSimulator to store collision information
    m_physicsSimulator->SetCollisionInformation( true );

    //Initialize the grid
    m_grid = new Construction::GridEntity(
        new Construction::Grid( gridSize, occMatrix ),
        m_pluginDCS.get(),
        m_physicsSimulator );
    m_grid->SetNameAndDescriptions();
    m_grid->InitPhysics();
    m_grid->GetPhysicsRigidBody()->setFriction( 1.0 );
    m_grid->GetPhysicsRigidBody()->StaticConcaveShape();
    m_entities[ m_grid->GetDCS()->GetName() ] = m_grid;

    //Initialize the starting block
    m_startBlock = new Construction::BlockEntity(
        new Construction::Block(),
        m_pluginDCS.get(),
        m_physicsSimulator );
    //m_entities.push_back( m_startBlock );
    m_startBlock->GetGeometry()->SetColor( 0.0, 0.0, 0.0, 1.0 );
    //Set name and descriptions for blocks
    m_startBlock->SetNameAndDescriptions( 0 );
    m_startBlock->GetDCS()->SetTranslationArray( startBlockPosition );
    m_startBlock->InitPhysics();
    m_startBlock->GetPhysicsRigidBody()->setFriction( 1.0 );
    m_startBlock->GetPhysicsRigidBody()->StaticConcaveShape();
    //Set up map to entities
    m_entities[ m_startBlock->GetDCS()->GetName() ] = m_startBlock;
    

    //Initialize the blocks
    for( int i = 0; i < numBlocks; ++i )
    {
        m_blocks.push_back( new Construction::BlockEntity(
            new Construction::Block(),
            m_pluginDCS.get(),
            m_physicsSimulator ) );
    }

    //Initialize the agents
    for( int i = 0; i < numAgents; ++i )
    {
        m_agents.push_back( new AgentEntity(
            new Construction::Agent(),
            m_pluginDCS.get(),
            m_physicsSimulator ) );
    }

    for( size_t i = 0; i < m_blocks.size(); ++i )
    {
        //Set name and descriptions for blocks
        m_blocks.at( i )->SetNameAndDescriptions( i + 1 );

        //Set physics properties for blocks
        m_blocks.at( i )->InitPhysics();
        m_blocks.at( i )->GetPhysicsRigidBody()->setFriction( 1.0 );

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
        m_agents.at( i )->GetPhysicsRigidBody()->setFriction( 1.0 );

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
                                                                      //Subtract m_worldScale
                                                                      //to keep blocks off walls
            randOne = posNegOne * ( 0.5 * ( 1 + rand() % ( gridSize ) ) - m_worldScale );
            randTwo = posNegTwo * ( 0.5 * ( 1 + rand() % ( gridSize ) ) - m_worldScale );

            for( size_t j = 0; j < positions.size(); ++j )
            {
                if( ( fabs( randOne ) < ( gridSize * 0.2 ) ||
                      fabs( randTwo ) < ( gridSize * 0.2 ) ) )
                {
                    needsNewPosition = true;
                }
                else if( ( randOne > ( positions.at( j ).first - m_worldScale ) &&
                           randOne < ( positions.at( j ).first + m_worldScale ) )
                           &&
                         ( randTwo > ( positions.at( j ).second - m_worldScale ) &&
                           randTwo < ( positions.at( j ).second + m_worldScale ) ) )
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
        Construction::AgentEntity* agent = m_agents.at( i );
        Construction::ObstacleSensor* obstacleSensor = agent->GetObstacleSensor();
        Construction::BlockSensor* blockSensor = agent->GetBlockSensor();
        Construction::SiteSensor* siteSensor = agent->GetSiteSensor();
        Construction::HoldBlockSensor* holdBlockSensor = agent->GetHoldBlockSensor();

        //Remove reference lines
        blockSensor->RemoveLine();
        siteSensor->RemoveLine();

        holdBlockSensor->CollectInformation();
        if( holdBlockSensor->HoldingBlock() )
        {
            siteSensor->CollectInformation();
            if( siteSensor->SiteInView() )
            {
                agent->GoToSite();
                if( siteSensor->CloseToSite() )
                {
                    bool collision = agent->GetPhysicsRigidBody()->CollisionInquiry(
                        m_entities[ agent->GetTargetDCS()->GetName() ]->GetPhysicsRigidBody() );
                    if( collision )
                    {
                        agent->Build();
                    }
                }
            }
            else
            {
                agent->WanderAround();
            }
        }
        else
        {
            blockSensor->CollectInformation();
            if( blockSensor->BlockInView() )
            {
                agent->GoToBlock();
                if( blockSensor->CloseToBlock() )
                {
                    Construction::BlockEntity* targetEntity = static_cast< Construction::BlockEntity* >
                        ( m_entities[ agent->GetTargetDCS()->GetName() ] );
                    bool collision = agent->GetPhysicsRigidBody()->
                        CollisionInquiry( targetEntity->GetPhysicsRigidBody() );
                    if( collision )
                    {
                        agent->PickUpBlock( targetEntity );
                    }
                }
            }
            else
            {
                agent->WanderAround();
            }
        }

        obstacleSensor->CollectInformation();
        if( obstacleSensor->ObstacleDetected() )
        {
            agent->AvoidObstacle();
        }

    }
}
////////////////////////////////////////////////////////////////////////////////
void World::PreFrameUpdate()
{
    if( m_structureNotComplete )
    {
        CommunicatingBlocksAlgorithm();
    }
    else
    {
        std::cout << "Structure is finished!" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* World::GetPluginDCS()
{
    return m_pluginDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
