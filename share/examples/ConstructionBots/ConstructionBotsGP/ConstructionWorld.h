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

namespace bots
{
// --- My Includes --- //
class GridEntity;
class BlockEntity;
class AgentEntity;

class ConstructionWorld
{
public:
    ConstructionWorld(
        ves::xplorer::scenegraph::DCS* pluginDCS,
#ifdef VE_SOUND
        osgAL::SoundManager* soundManager,
#endif
        ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    ~ConstructionWorld();

    void PreFrameUpdate();

private:
    void InitializeFramework();
    void CreateRandomPositions( int gridSize );

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;

    bots::GridEntity* mGrid;
    bots::BlockEntity* mStartBlock;
    std::map< std::string, bots::BlockEntity* > mBlockEntities;
    std::vector< bots::AgentEntity* > mAgents;

    ves::xplorer::scenegraph::PhysicsSimulator* mPhysicsSimulator;

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* mAmbientSound;
#endif

};
} //end bots

#endif //CONSTRUCTION_WORLD_H
