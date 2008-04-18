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
#include "HoldBlockSensor.h"
#include "AgentEntity.h"

// --- OSG Includes --- //
#include <osgUtil/LineSegmentIntersector>
#include <osgUtil/IntersectionVisitor>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
HoldBlockSensor::HoldBlockSensor( bots::AgentEntity* agentEntity )
:
Sensor( agentEntity ),
mHoldingBlock( false ),
mRange( 0.6 ),
mLineSegmentIntersector( new osgUtil::LineSegmentIntersector(
                             osg::Vec3( 0, 0, 0 ), osg::Vec3( 0, 0, 0 ) ) )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
HoldBlockSensor::~HoldBlockSensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HoldBlockSensor::CollectInformation()
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS =
        mAgentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS =
        mAgentEntity->GetDCS();

    osg::Vec3d startPoint = agentDCS->getPosition();
    osg::Vec3d endPoint = startPoint;
    endPoint.z() += mRange;

    //Reset results from last frame
    mHoldingBlock = false;

    mLineSegmentIntersector->reset();
    mLineSegmentIntersector->setStart( startPoint );
    mLineSegmentIntersector->setEnd( endPoint );

    osgUtil::IntersectionVisitor intersectionVisitor(
        mLineSegmentIntersector.get() );
    pluginDCS->RemoveChild( agentDCS.get() );
    pluginDCS->accept( intersectionVisitor );
    pluginDCS->AddChild( agentDCS.get() );

    if( mLineSegmentIntersector->containsIntersections() )
    {
        mHoldingBlock = true;
    }

    if( !mHoldingBlock && mAgentEntity->IsBuilding() )
    {
        mAgentEntity->SetBuildMode( false );
    }
}
////////////////////////////////////////////////////////////////////////////////
bool HoldBlockSensor::HoldingBlock()
{
    return mHoldingBlock;
}
////////////////////////////////////////////////////////////////////////////////
