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

#ifndef SITE_SENSOR_H
#define SITE_SENSOR_H

// --- My Includes --- //
#include "Sensor.h"

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

//Simulates an optical type sensor
namespace bots
{
class SiteSensor : public Sensor
{
public:
    ///Constructor
    SiteSensor( bots::AgentEntity* agentEntity );

    ///Destructor
    virtual ~SiteSensor();

    ///Collect information from the environment
    virtual void CollectInformation();

    ///Rotate the site sensor
    void Rotate( bool leftover = false );

    ///Returns if the site is currently in view
    const bool SiteInView() const;

    ///Returns if the agent entity is close to the site
    const bool CloseToSite() const;

    ///Get the normalized vector to the site
    const btVector3& GetNormalizedSiteVector() const;

    ///Reset the angle of this site sensor
    void ResetAngle();

    ///Set the range for this sensor
    void SetRange( double range );

protected:
    ///Initialize this site sensor
    virtual void Initialize();

private:
    ///Tells if the site is in view of this sensor
    bool mSiteInView;

    ///Tells if the agent entity is close to the site
    bool mCloseToSite;

    ///The angle of this sensor
    double mAngle;

    ///The angle increment of this sensor
    double mAngleInc;

    ///The angle this site sensor rotates through each frame
    double mAnglePerFrame;

    ///The amount of angle leftover from equal subdivisions of mAnglePerFrame
    double mAngleLeftover;

    ///The number of rotations - 1 needed per frame to get mAnglePerFrame
    unsigned int mRotationsPerFrame;

    ///The range of this sensor
    double mRange;

    ///The normalized vector from this agent entity to the site
    btVector3 mNormalizedSiteVector;

};
} //end bots

#endif //SITE_SENSOR_H
