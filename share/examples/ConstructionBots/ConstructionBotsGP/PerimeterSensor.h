/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef PERIMETER_SENSOR_H
#define PERIMETER_SENSOR_H

// --- My Includes --- //
#include "Sensor.h"

// --- OSG Includes --- //
#include <osg/ref_ptr>

#include <osgUtil/LineSegmentIntersector>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

// --- C/C++ Libraries --- //
#include <vector>

namespace bots
{
class PerimeterSensor : public Sensor
{
public:
    ///Constructor
    PerimeterSensor( bots::AgentEntity* agentEntity );

    ///Destructor
    virtual ~PerimeterSensor();

    ///Returns if the agent entity is aligned with a block in the structure
    const bool Aligned() const;

    ///Collect information from the environment
    virtual void CollectInformation();

    ///Get the normalized resultant force vector
    const btVector3& GetNormalizedResultantForceVector();

    ///Get the drawable of the block that was queried for connection
    osg::Drawable* const GetQueriedConnection() const;

    ///Get the range for this perimeter sensor
    const double& GetRange() const;

    ///Returns if the perimeter was detected
    const bool PerimeterDetected() const;

    ///Reset this perimeter sensor
    void Reset();

    ///Set the range of this perimeter sensor
    void SetRange( double range );

protected:
    ///Intialize this perimeter sensor
    virtual void Initialize();

private:
    ///Calculate the local intersection positions of this perimeter sensor
    /* There are eight perimeter sensors as shown below
          3    2
       4 _|____|_ 1
          |    |
       5 _|____|_ 0
          |    |
          6    7
    */
    void CalculateLocalPositions();

    const bool CollisionTest();

    ///Tells if the agent entity is aligned with a block in the structure
    bool mAligned;

    ///Tells if the perimeter has been detected
    bool mPerimeterDetected;

    ///Stores the last clockwise detection of this perimeter sensor
    int* mLastClockWiseDetection;

    ///Stores the previous drawable detected by this perimeter sensor
    osg::ref_ptr< osg::Drawable > mPreviousDrawable;

    ///The range of this perimeter sensor
    double mRange;

    ///The resultant force for the agent entity based off object detections
    btVector3 mResultantForce;

    ///The drawable of the block that has been queried for connection
    osg::ref_ptr< osg::Drawable > mQueriedConnection;

    ///Contains the line segment intersectors for this perimeter sensor
    osg::ref_ptr< osgUtil::IntersectorGroup > mIntersectorGroup;
    
    ///Contains the intersections detected by this perimeter sensor
    std::vector< osgUtil::LineSegmentIntersector::Intersection > mIntersections;

};
} //end bots

#endif //PERIMETER_SENSOR_H
