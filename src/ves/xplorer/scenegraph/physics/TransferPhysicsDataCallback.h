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

#ifndef TRANSFER_PHYSICS_DATA_CALLBACK_H
#define TRANSFER_PHYSICS_DATA_CALLBACK_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/NodeCallback>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class PhysicsRigidBody;

#ifdef _OSG
/*!\file TransferPhysicsDataCallback.h
 *
 */

/*!\class ves::xplorer::scenegraph::TransferPhysicsDataCallback
 * This is the callback class configured to handle transfering physics data back to the respective osg node
 */

/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS TransferPhysicsDataCallback : public osg::NodeCallback
{
public:
    ///Constructor
    TransferPhysicsDataCallback();

    ///Destructor
    virtual ~TransferPhysicsDataCallback()
    {
        ;
    }

    ///Copy constructor
    TransferPhysicsDataCallback( const TransferPhysicsDataCallback& );

    ///Set the physics rigid body for this callback
    ///\param physicsRigidBody physics rigid body
    void SetPhysicsRigidBody( PhysicsRigidBody* physicsRigidBody );

    ///Operator required by osg
    virtual void operator()( osg::Node* node, osg::NodeVisitor* nv );

protected:
    PhysicsRigidBody* m_physicsRigidBody;///<Pointer to the physics rigid body

};
#elif _OPENSG
#endif

} // end scenegraph
} // end xplorer
} // end ves

#endif //TRANSFER_PHYSICS_DATA_CALLBACK_H
