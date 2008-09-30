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

#ifndef SENSOR_H
#define SENSOR_H

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Array>

namespace osg
{
class Geode;
class Geometry;
}

namespace osgUtil
{
class LineSegmentIntersector;
}

namespace bots
{
// --- My Includes --- //
class AgentEntity;

class Sensor
{
public:
    ///Constructor
    Sensor( bots::AgentEntity* agentEntity );

    ///Destructor
    virtual ~Sensor();

    ///Collect information from the environment
    virtual void CollectInformation() = 0;

    ///Display the geometry for this sensor
    virtual void DisplayGeometry( bool onOff );

protected:
    ///Initialize this sensor
    virtual void Initialize() = 0;

    ///Reset this sensor
    virtual void Reset() = 0;

    ///A pointer to the agent entity that this sensor belongs to
    bots::AgentEntity* mAgentEntity;

    ///The geode which contains the geometry of this sensor
    osg::ref_ptr< osg::Geode > mGeode;

    ///The geometry of this sensor
    osg::ref_ptr< osg::Geometry > mGeometry;

    ///Contains the vertices of the geometry for this sensor
    osg::ref_ptr< osg::Vec3Array > mVertexArray;

    ///A line segment intersector to perform polygon intersection tests
    osg::ref_ptr< osgUtil::LineSegmentIntersector > mLineSegmentIntersector;

};
} //end bots

#endif //SENSOR_H
