/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef TRANSFER_PHYSICS_DATA_CALLBACK_H
#define TRANSFER_PHYSICS_DATA_CALLBACK_H

/*!\file TransferPhysicsDataCallback.h
 */

/*!\class VE_SceneGraph::TransferPhysicsDataCallback
 * This is the callback class configured to handle transfering physics data back to the respective osg node
 */

/*!\namespace VE_SceneGraph
 *
 */

// --- VE-Suite Includes --- //
#include "VE_Installer/include/VEConfig.h"

// --- OSG Includes --- //
#include <osg/NodeCallback>

// --- Bullet Includes --- //

class btRigidBody;

namespace VE_SceneGraph
{
#ifdef _OSG
class VE_SCENEGRAPH_EXPORTS TransferPhysicsDataCallback : public osg::NodeCallback
{
public:
    ///Constructor
    TransferPhysicsDataCallback();

    ///Destructor
    virtual ~TransferPhysicsDataCallback(){;}

    ///Copy constructor
    TransferPhysicsDataCallback( const TransferPhysicsDataCallback& );

    ///Set the bullet rigid for this callback
    ///\param transform bullet rigid body
    void SetbtRigidBody( btRigidBody* transform );

    ///Operator required by osg
    virtual void operator()( osg::Node* node, osg::NodeVisitor* nv );

protected:
    btRigidBody* m_btBody;///<Pointer to the bullet body

};
#elif _OPENSG
#endif
}

#endif //TRANSFER_PHYSICS_DATA_CALLBACK_H
