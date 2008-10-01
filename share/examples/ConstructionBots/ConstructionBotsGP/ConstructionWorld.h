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

#ifndef CONSTRUCTION_WORLD_H
#define CONSTRUCTION_WORLD_H

// --- VE-Suite Includes --- //
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class DCS;
    class CADEntity;
    class PhysicsSimulator;
#ifdef VE_SOUND
    class Sound;
#endif
}
}
}

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- osgAL Includes --- //
#ifdef VE_SOUND
namespace osgAL
{
    class SoundManager;
}
#endif

// --- C/C++ Includes --- //
#include <map>
#include <vector>
#include <string>
#include <fstream>

namespace bots
{
// --- My Includes --- //
class GridEntity;
class BlockEntity;
class AgentEntity;

class ConstructionWorld
{
public:
    ///Constructor
    ConstructionWorld(
        ves::xplorer::scenegraph::DCS* pluginDCS,
#ifdef VE_SOUND
        osgAL::SoundManager* soundManager,
#endif
        ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    ///Destructor
    ~ConstructionWorld();

    ///Perform the preframe update
    void PreFrameUpdate();

    ///Reset the simulation
    void ResetSimulation();

private:
    ///Initialize the construction world framework
    void InitializeFramework();

    ///Randomize where the blocks and agents are placed in the environment
    void CreateRandomPositions();

    ///The number of frames processed to complete this simulation
    unsigned int mFrameCount;

    ///The number of simulations to run for each "state set"
    unsigned int mNumSimulations;

    ///The number of simulations left to run for the current "state set"
    unsigned int mNumSimulationsLeft;

    ///The number of construction blocks for the simulation
    unsigned int mNumBlocks;

    ///The number of blocks left to build the structure
    unsigned int mNumBlocksLeft;

    ///The number of construction agents for the simulation
    unsigned int mNumAgents;

    ///The number of agents to increment by after each block sensor range trial
    unsigned int mDeltaAgents;

    ///The size of the construction framework grid plane
    unsigned int mGridSize;

    ///The initial length of the block sensor range
    double mBlockSensorRange;

    ///The delta length added to the block sensor range after a simulation set
    double mDeltaBlockSensorRange;

    ///A pointer to the plugin DCS
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;

#ifdef VE_SOUND
    ///A pointer to the sound manager for this plugin
    osgAL::SoundManager* mSoundManager;
    ///The ambient sound played for this simulation
    ves::xplorer::scenegraph::Sound* mAmbientSound;
#endif

    ///The grid entity for this construction world
    bots::GridEntity* mGrid;

    ///The start block to initiate the structure building
    bots::BlockEntity* mStartBlock;

    ///The block entities used to build the structure
    std::map< std::string, bots::BlockEntity* > mBlockEntities;

    ///The agents which build the structure
    std::vector< bots::AgentEntity* > mAgentEntities;

    ///The occupancy matrix containing the shape desired to be built
    std::map< std::pair< int, int >, std::pair< bool, bool > > mOccupancyMatrix;

    ///A pointer to the physics simulator for this plugin
    ves::xplorer::scenegraph::PhysicsSimulator* mPhysicsSimulator;

    ///A file to write simulation data to
    std::ofstream mSimulationData;

};
} //end bots

#endif //CONSTRUCTION_WORLD_H
