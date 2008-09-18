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
    AgentEntity(
        bots::Agent* agent,
        ves::xplorer::scenegraph::DCS* pluginDCS,
#ifdef VE_SOUND
        osgAL::SoundManager* soundManager,
#endif
        ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~AgentEntity();

    void CommunicatingBlocksAlgorithm();

    ves::xplorer::scenegraph::DCS* const GetPluginDCS() const;
    ves::xplorer::scenegraph::DCS* const GetTargetDCS() const;

    bots::BlockSensorPtr const GetBlockSensor() const;
    bots::HoldBlockSensorPtr const GetHoldBlockSensor() const;
    bots::ObstacleSensorPtr const GetObstacleSensor() const;
    bots::PerimeterSensorPtr const GetPerimeterSensor() const;
    bots::SiteSensorPtr const GetSiteSensor() const;

    ///
    void SetBlockEntityMap(
        std::map< std::string, bots::BlockEntity* >* blockEntityMap );

    ///
    void SetBlocksLeft( unsigned int* blocksLeft );

    ///
    void SetConstraints( int gridSize );

    ///
    void SetNameAndDescriptions( int number );
    
    ///
    void SetTargetDCS( ves::xplorer::scenegraph::DCS* targetDCS );

protected:

private:
    //Give sensors easy access to AgentEntity
    friend class BlockSensor;
    friend class HoldBlockSensor;
    friend class ObstacleSensor;
    friend class PerimeterSensor;
    friend class SiteSensor;
    
    void Initialize();

    //The agent behaviors
    void AvoidObstacle();
    void Build();
    void FollowPerimeter();
    void GoToBlock();
    void GoToSite();
    void InitiateBuildMode();
    void PickUpBlock();
    void QueryBlock();

    bool mBuildMode;

    unsigned int* mBlocksLeft;

    double mMaxSpeed;
    double mBuildSpeed;

    //The color of the blocks
    osg::Vec4 mBlockColor;
    //The color of the site
    osg::Vec4 mSiteColor;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mTargetDCS;

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* mAgentSound;
    ves::xplorer::scenegraph::Sound* mPickUpBlockSound;
    ves::xplorer::scenegraph::Sound* mAttachBlockSound;
#endif

    btGeneric6DofConstraint* mConstraint;

    osg::ref_ptr< bots::Agent > mAgentGeometry;

    //The agent sensors
    bots::BlockSensorPtr mBlockSensor;
    bots::HoldBlockSensorPtr mHoldBlockSensor;
    bots::ObstacleSensorPtr mObstacleSensor;
    bots::PerimeterSensorPtr mPerimeterSensor;
    bots::SiteSensorPtr mSiteSensor;

    //This in only here to test for collisions and for site interaction
    std::map< std::string, bots::BlockEntity* >* mBlockEntityMap;

    bots::BlockEntity* mHeldBlock;

};
}

#endif //AGENT_ENTITY_H
