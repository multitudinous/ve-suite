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

#ifndef OBSTACLE_SENSOR_H
#define OBSTACLE_SENSOR_H

// --- My Includes --- //
#include "Sensor.h"

// --- OSG Includes --- //
#include <osg/ref_ptr>

#include <osgUtil/LineSegmentIntersector>

namespace osgUtil
{
class IntersectorGroup;
}

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

// --- C/C++ Libraries --- //
#include <vector>

//Simulates a 3D ring of ultrasound sensors for obstacle detection
namespace bots
{
class ObstacleSensor : public Sensor
{
public:
    ///Constructor
    ObstacleSensor( bots::AgentEntity* agentEntity );

    ///Destructor
    virtual ~ObstacleSensor();

    ///Collect information from the environment
    virtual void CollectInformation();

    ///Get the normalized resultant force vector
    const btVector3& GetNormalizedResultantForceVector();

    ///Returns is an obstacle has been detected by this obstacle sensor
    const bool ObstacleDetected() const;

    ///Reset this obstacle sensor
    virtual void Reset();

    ///Set the angle increment for this obstacle sensor
    void SetAngleIncrement( double angleIncrement );

    ///Set the force attraction constant for the VFF algorithm
    void SetForceAttractionConstant( double forceAttractionConstant );

    ///Set the force repelling constant for the VFF algorithm
    void SetForceRepellingConstant( double forceRepellingConstant );

    ///Set the range of this obstacle sensor
    void SetRange( double range );

protected:
    ///Initialize this obstacle sensor
    virtual void Initialize();

private:
    ///Calculate the local intersection positions of this obstacle sensor
    void CalculateLocalPositions();

    ///Tells if an obstacle has been detected by this obstacle sensor
    bool mObstacleDetected;

    ///Tells if this obstacle sensor is in wall follwing mode
    bool mWallFollowMode;

    ///The number of force lines for this obstacle sensor
    unsigned int mNumForceLines;

    ///The number of radial detectors for this obstacle sensor
    unsigned int mNumDetectors;

    ///The angle increment for this obstacle sensor
    double mAngleIncrement;

    ///The range of this obstacle sensor
    double mRange;

    ///The length of the lines used to visualize forces for the VFF algorithm
    double mForceLineLength;

    ///The force attraction constant for the VFF algorithm
    double mForceAttractionConstant;

    ///The force repelling constant for the VFF algorithm
    double mForceRepellingConstant;

    ///The resultant force vector calculated by the VFF algorithm
    btVector3 mResultantForce;

    ///The geometry for the radial detectors
    osg::ref_ptr< osg::Geometry > mDetectorGeometry;

    ///The vertices of the detector geometry
    osg::ref_ptr< osg::Vec3Array > mDetectorVertexArray;

    ///Contains the line segment intersectors for this obstacle sensor
    osg::ref_ptr< osgUtil::IntersectorGroup > mIntersectorGroup;

    ///Contains the intersections detected by this obstacle sensor
    std::vector< osgUtil::LineSegmentIntersector::Intersection > mIntersections;

};
} //end bots

#endif //OBSTACLE_SENSOR_H
