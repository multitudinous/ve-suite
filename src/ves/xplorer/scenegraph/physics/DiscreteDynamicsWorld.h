/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

#ifndef VES_XPLORER_SCENEGRAPH_PHYSICS_DISCRETEDYNAMICSWORLD_H
#define VES_XPLORER_SCENEGRAPH_PHYSICS_DISCRETEDYNAMICSWORLD_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDiscreteDynamicsWorld.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
/*!\file DiscreteDynamicsWorld.h
 *
 */

/*!\class ves::xplorer::scenegraph::DiscreteDynamicsWorld
 *
 */

/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS DiscreteDynamicsWorld :
    public btDiscreteDynamicsWorld
{
public:
    ///Constructor
    DiscreteDynamicsWorld(
        btDispatcher* dispatcher,
        btBroadphaseInterface* pairCache,
        btConstraintSolver* constraintSolver,
        btCollisionConfiguration* collisionConfiguration );

    ///Destructor
    virtual ~DiscreteDynamicsWorld();

    ///If maxSubSteps > 0, it will interpolate motion between fixedTimeStep's
    virtual int stepSimulation(
        btScalar timeStep,
        int maxSubSteps = 1,
        btScalar fixedTimeStep = btScalar( 1.0 ) / btScalar( 60.0 ) );

protected:
    ///
    virtual void internalSingleStepSimulation( btScalar timeStep );

private:


};
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_PHYSICS_DISCRETEDYNAMICSWORLD_H
