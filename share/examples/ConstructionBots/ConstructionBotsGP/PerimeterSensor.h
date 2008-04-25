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
    PerimeterSensor( bots::AgentEntity* agentEntity );
    virtual ~PerimeterSensor();

    virtual void CollectInformation();

    void Reset();

    const btVector3& GetNormalizedResultantForceVector();
    osg::Drawable* GetQueriedConnection();

    void SetRange( double range );

    bool Aligned();
    bool PerimeterDetected();

private:
    void Initialize();

    /* There are eight perimeter sensors as shown below
          2    1
       3 _|____|_ 0
          |    |
       4 _|____|_ 7
          |    |
          5    6
    */
    void CalculateLocalPositions();

    bool mAligned;
    bool mPerimeterDetected;

    int mLastClockWiseDetection;
    osg::ref_ptr< osg::Drawable > previousDrawable;

    double mRange;

    btVector3 mResultantForce;

    osg::Drawable* mQueriedConnection;
    
    std::vector< osgUtil::LineSegmentIntersector::Intersection > mIntersections;

    osg::ref_ptr< osg::Vec3Array > mLocalPositions;
    osg::ref_ptr< osgUtil::LineSegmentIntersector > mLineSegmentIntersector;

};
} //end bots

#endif //PERIMETER_SENSOR_H
