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

#ifndef BLOCK_SENSOR_H
#define BLOCK_SENSOR_H

// --- My Includes --- //
#include "Sensor.h"

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

//Simulates an optical type sensor
namespace bots
{
class BlockSensor : public Sensor
{
public:
    BlockSensor( bots::AgentEntity* agentEntity );

    virtual ~BlockSensor();

    virtual void CollectInformation();

    void Rotate();

    bool BlockInView();
    bool CloseToBlock();

    const btVector3& GetNormalizedBlockVector();

    void SetRange( double range );

protected:
    virtual void Initialize();

private:
    bool mBlockInView;
    bool mCloseToBlock;

    double mAngle;
    double mAngleInc;
    double mRange;

    btVector3 mNormalizedBlockVector;

};
} //end bots

#endif //BLOCK_SENSOR_H
