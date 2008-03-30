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
#include "SiteSensorPtr.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

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
    AgentEntity( osg::ref_ptr< bots::Agent > agent,
                 ves::xplorer::scenegraph::DCS* pluginDCS,
                 ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~AgentEntity();

    //The agent behaviors
    void AvoidObstacle();
    void WanderAround();
    void GoToBlock();
    void PickUpBlock( bots::BlockEntity* blockEntity );
    void GoToSite();
    void Build();

    void SetNameAndDescriptions( int number );
    void SetConstraints( int gridSize );
    void SetTargetDCS( ves::xplorer::scenegraph::DCS* targetDCS );
    void SetBuildMode( bool buildMode );

    bool IsBuilding();

	ves::xplorer::scenegraph::DCS* GetPluginDCS();
	ves::xplorer::scenegraph::DCS* GetTargetDCS();

    bots::BlockSensorPtr GetBlockSensor();
    bots::HoldBlockSensorPtr GetHoldBlockSensor();
    bots::ObstacleSensorPtr GetObstacleSensor();
    bots::SiteSensorPtr GetSiteSensor();

private:
    void Initialize();

    bool mBuildMode;

	osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mTargetDCS;

    btGeneric6DofConstraint* mConstraint;

    osg::ref_ptr< bots::Agent > mGeometry;

    //The agent sensors
    bots::BlockSensorPtr mBlockSensor;
    bots::HoldBlockSensorPtr mHoldBlockSensor;
    bots::ObstacleSensorPtr mObstacleSensor;
    bots::SiteSensorPtr mSiteSensor;
    
};
}

#endif //AGENT_ENTITY_H
