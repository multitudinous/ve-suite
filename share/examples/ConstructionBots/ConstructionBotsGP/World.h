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

#ifndef WORLD_H
#define WORLD_H

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
#include <vector>
#include <map>

namespace bots
{
// --- My Includes --- //
class GridEntity;
class BlockEntity;
class AgentEntity;

class World
{
public:
    World( ves::xplorer::scenegraph::DCS* pluginDCS,
           ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
#ifdef VE_SOUND
         , osgAL::SoundManager* soundManager
#endif
           );

    ~World();

    void PreFrameUpdate();

private:
    void InitFramework();
    void CreateRandomPositions( int gridSize );

    void CommunicatingBlocksAlgorithm();

    bool mStructureNotComplete;

    ves::xplorer::scenegraph::PhysicsSimulator* mPhysicsSimulator;

    std::map< std::string, ves::xplorer::scenegraph::CADEntity* > mEntities;
    bots::GridEntity* mGrid;
    std::vector< bots::BlockEntity* > mBlocks;
    std::vector< bots::AgentEntity* > mAgents;
    bots::BlockEntity* mStartBlock;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* mAmbientSound;
#endif

};
} //end bots

#endif //WORLD_H
