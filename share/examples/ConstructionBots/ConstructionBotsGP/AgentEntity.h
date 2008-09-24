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

#ifndef AGENT_ENTITY_H
#define AGENT_ENTITY_H

// --- My Includes --- //
#include "BlockSensorPtr.h"
#include "HoldBlockSensorPtr.h"
#include "ObstacleSensorPtr.h"
#include "PerimeterSensorPtr.h"
#include "SiteSensorPtr.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;

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

// --- Bullet Includes --- //
class btGeneric6DofConstraint;

// --- C/C++ Libraries --- //

namespace bots
{
// --- My Includes --- //
class Agent;
class BlockEntity;

class AgentEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    ///Constructor
    AgentEntity(
        bots::Agent* agent,
        ves::xplorer::scenegraph::DCS* pluginDCS,
#ifdef VE_SOUND
        osgAL::SoundManager* soundManager,
#endif
        ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    ///Destructor
    virtual ~AgentEntity();

    ///The communicating blocks algorithm
    void CommunicatingBlocksAlgorithm();

    ///Get the plugin DCS
    ves::xplorer::scenegraph::DCS* const GetPluginDCS() const;

    ///Get the target DCS
    ves::xplorer::scenegraph::DCS* const GetTargetDCS() const;

    ///Get the block sensor
    bots::BlockSensorPtr const GetBlockSensor() const;

    ///Get the hold block sensor
    bots::HoldBlockSensorPtr const GetHoldBlockSensor() const;

    ///Get the obstacle sensor
    bots::ObstacleSensorPtr const GetObstacleSensor() const;

    ///Get the perimeter sensor
    bots::PerimeterSensorPtr const GetPerimeterSensor() const;

    ///Get the site sensor
    bots::SiteSensorPtr const GetSiteSensor() const;

    ///Set the block entity map
    void SetBlockEntityMap(
        std::map< std::string, bots::BlockEntity* >& blockEntityMap );

    ///Set the total number of blocks needed to complete the structure
    void SetBlocksLeft( unsigned int& blocksLeft );

    ///Set the constraints for the physics mesh
    void SetConstraints( int gridSize );

    ///Set the name and descriptions for this CADEntity
    void SetNameAndDescriptions( int number );
    
    ///Set the target DCS
    void SetTargetDCS( ves::xplorer::scenegraph::DCS* targetDCS );

protected:

private:
    ///Make BlockSensor class a friend
    friend class BlockSensor;

    ///Make HoldBlockSensor class a friend
    friend class HoldBlockSensor;

    ///Make ObstacleSensor class a friend
    friend class ObstacleSensor;

    ///Make PerimeterSensor class a friend
    friend class PerimeterSensor;

    ///Make SiteSensor class a friend
    friend class SiteSensor;
    
    ///Initialize this agent entity
    void Initialize();

    ///Performs the avoid obstacle behavior
    void AvoidObstacle();

    ///Performs the build behavior
    void Build();

    ///Performs the follow perimeter behavior
    void FollowPerimeter();

    ///Performs the go to block behavior
    void GoToBlock();

    ///Performs the go to site behavior
    void GoToSite();

    ///Prepare this agent entity for building
    void InitiateBuildMode();

    ///Performs the pick up block behavior
    void PickUpBlock();

    ///Query a block for permission to attach
    void QueryBlock();

    ///Tells if this agent entity is currently building
    bool mBuildMode;

    ///The number of blocks left to complete the structure
    unsigned int* mBlocksLeft;

    ///The max speed at which this agent travels
    double mMaxSpeed;

    ///The speed used while building
    double mBuildSpeed;

    ///The color of the blocks
    osg::Vec4 mBlockColor;

    ///The color of the site
    osg::Vec4 mSiteColor;

    ///A pointer to the plugin DCS
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;

    ///A pointer to the target DCS
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mTargetDCS;

#ifdef VE_SOUND
    ///The sound played for this agent
    ves::xplorer::scenegraph::Sound* mAgentSound;

    ///The sound played when a block is successfully picked up
    ves::xplorer::scenegraph::Sound* mPickUpBlockSound;

    ///The sound played when a block is successfully attached to the structure
    ves::xplorer::scenegraph::Sound* mAttachBlockSound;
#endif

    ///The constraints on the physics mesh
    btGeneric6DofConstraint* mConstraint;

    ///The geometry of this agent entity
    osg::ref_ptr< bots::Agent > mAgentGeometry;

    ///The block sensor
    bots::BlockSensorPtr mBlockSensor;

    ///The hold block sensor
    bots::HoldBlockSensorPtr mHoldBlockSensor;

    ///The obstacle sensor
    bots::ObstacleSensorPtr mObstacleSensor;

    ///The perimeter sensor
    bots::PerimeterSensorPtr mPerimeterSensor;

    ///The site sensor
    bots::SiteSensorPtr mSiteSensor;

    ///A pointer to the block entity map
    ///This in only here to test for collisions and for site interaction
    const std::map< std::string, bots::BlockEntity* >* mBlockEntityMap;

    ///A pointer to the held block entity
    bots::BlockEntity* mHeldBlock;

};
}

#endif //AGENT_ENTITY_H
