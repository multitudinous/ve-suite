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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/TransferPhysicsDataCallback.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/vesMotionState.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

#include <ves/xplorer/scenegraph/DCS.h>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletCollision/NarrowPhaseCollision/btPersistentManifold.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

#ifdef _OSG
////////////////////////////////////////////////////////////////////////////////
TransferPhysicsDataCallback::TransferPhysicsDataCallback()
:
m_physicsRigidBody( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TransferPhysicsDataCallback::TransferPhysicsDataCallback( const TransferPhysicsDataCallback& input )
:
osg::Object( input ),
osg::NodeCallback( input ),
m_physicsRigidBody( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TransferPhysicsDataCallback::SetPhysicsRigidBody( PhysicsRigidBody* physicsRigidBody )
{
    m_physicsRigidBody = physicsRigidBody;
}
////////////////////////////////////////////////////////////////////////////////
void TransferPhysicsDataCallback::operator()( osg::Node* node, osg::NodeVisitor* nv )
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > dcs = static_cast< ves::xplorer::scenegraph::DCS* >( node );

    if( dcs.valid() && m_physicsRigidBody )
    {
        ves::xplorer::scenegraph::vesMotionState* motionState =
            static_cast< vesMotionState* >( m_physicsRigidBody->getMotionState() );
        btTransform transform;
        if( motionState )
        {
            motionState->getWorldTransform( transform );
        }

        btQuaternion quat = transform.getRotation();
        dcs->setAttitude( osg::Quat( quat[ 0 ], quat[ 1 ], quat[ 2 ], quat[ 3 ] ) );

        btVector3 position = transform.getOrigin();
        dcs->setPosition( osg::Vec3d( position[ 0 ], position[ 1 ], position[ 2 ] ) );

        if( m_physicsRigidBody->IsStoringObjectsInContact() )
        {
            btDynamicsWorld* dynamicsWorld = 
            ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld();

            int numManifolds = dynamicsWorld->getDispatcher()->getNumManifolds();
            for( int i = 0; i < numManifolds; ++i )
            {
                btPersistentManifold* contactManifold = dynamicsWorld->getDispatcher()->getManifoldByIndexInternal( i );
                btCollisionObject* thisCollisionObject = static_cast< btCollisionObject* >( m_physicsRigidBody );
                btCollisionObject* obA = static_cast< btCollisionObject* >( contactManifold->getBody0() );
                btCollisionObject* obB = static_cast< btCollisionObject* >( contactManifold->getBody1() );
                if( thisCollisionObject == obA )
                {
                    //std::cout << "This Collision Object = obA" << std::endl;
                    //std::cout << std::endl;
                }
                else if( thisCollisionObject == obB )
                {
                    //std::cout << "This Collision Object = obB" << std::endl;
                    //std::cout << std::endl;
                }
            }
        }
    }

    traverse( node, nv );
}
////////////////////////////////////////////////////////////////////////////////
#endif

} // end scenegraph
} // end xplorer
} // end ves
