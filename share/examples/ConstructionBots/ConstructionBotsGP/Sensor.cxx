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
#include "Sensor.h"
#include "AgentEntity.h"

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>

#include <osgUtil/LineSegmentIntersector>
//#include <osgUtil/IntersectionVisitor>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
Sensor::Sensor( bots::AgentEntity* agentEntity )
    :
    mAgentEntity( agentEntity ),
    mGeode( 0 ),
    mGeometry( 0 ),
    mVertexArray( 0 ),
    mLineSegmentIntersector( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Sensor::~Sensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Sensor::DisplayGeometry( bool onOff )
{
    mGeode->setNodeMask( onOff );
}
////////////////////////////////////////////////////////////////////////////////
