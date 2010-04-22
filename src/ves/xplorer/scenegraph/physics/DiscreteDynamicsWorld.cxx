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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/DiscreteDynamicsWorld.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

#ifdef VE_SOUND
#include <ves/xplorer/scenegraph/physics/sound/SoundUtilities.h>
#include <ves/xplorer/scenegraph/physics/sound/SoundTable.h>
#include <ves/xplorer/scenegraph/physics/sound/Material.h>
#endif

// --- osgBullet Includes --- //
#include <osgbBullet/Utils.h>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
DiscreteDynamicsWorld::DiscreteDynamicsWorld(
    btDispatcher* dispatcher,
    btBroadphaseInterface* pairCache,
    btConstraintSolver* constraintSolver,
    btCollisionConfiguration* collisionConfiguration )
    :
    btDiscreteDynamicsWorld(
        dispatcher, pairCache, constraintSolver, collisionConfiguration )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
DiscreteDynamicsWorld::~DiscreteDynamicsWorld()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DiscreteDynamicsWorld::internalSingleStepSimulation( btScalar timeStep )
{
    //Update all kinematic objects here
    //saveKinematicState( timeStep );

    //Call base class step function
    btDiscreteDynamicsWorld::internalSingleStepSimulation( timeStep );
}
////////////////////////////////////////////////////////////////////////////////
int DiscreteDynamicsWorld::stepSimulation(
    btScalar timeStep, int maxSubSteps, btScalar fixedTimeStep )
{
    int const& numSimulationSubSteps = btDiscreteDynamicsWorld::stepSimulation(
        timeStep, maxSubSteps, fixedTimeStep );

#ifdef VE_SOUND
    //Collision flags, mainly so that the door doesn't collide with the doorframe.
    /*
    enum CollisionTypes
    {
        COL_DOOR = 0x1 << 0,
        COL_DOORFRAME = 0x1 << 1,
        COL_DEFAULT = 0x1 << 2,
    };
    unsigned int doorCollidesWith( COL_DEFAULT );
    unsigned int doorFrameCollidesWith( COL_DEFAULT );
    unsigned int defaultCollidesWith( COL_DOOR | COL_DOORFRAME | COL_DEFAULT );


    typedef std::set< btCollisionObject* > BulletObjList;
    BulletObjList g_movingList;
    */

    //Loop over all collision ovjects and find the ones that are moving.
    //Need this for door creak.
    /*
    const btCollisionObjectArray& colObjs( world->getCollisionObjectArray() );
    int idx( world->getNumCollisionObjects() );
    while( idx-- )
    {
        btCollisionObject* co( colObjs[ idx ] );
        btVector3 v( co->getInterpolationLinearVelocity() );
        v[0] = osg::absolute< float >( v[ 0 ] );
        v[1] = osg::absolute< float >( v[ 1 ] );
        v[2] = osg::absolute< float >( v[ 2 ] );
        
        BulletObjList::const_iterator it( g_movingList.find( co ) );
        if( ( v[ 0 ] > 0.9 ) || ( v[ 1 ] > 0.9 ) || ( v[ 2 ] > 0.9 ) )
        {
            // It's moving.
            if( it == g_movingList.end() )
            {
                g_movingList.insert( co );
                // We didn't already play a sound, so play one now.
                Material* mc = ( Material* )( co->getUserPointer() );
                if( mc != NULL )
                {
                    SoundUtilities::instance()->move(
                        mc->_mat,
                        osgbBullet::asOsgVec3(
                            co->getWorldTransform().getOrigin() ) );
                }
            }
        }
        else
        {
            //It's not moving
            if( it != g_movingList.end() )
            {
                g_movingList.erase( it );
            }
        }
    }
    */
    bool collide( false ), slide( false );
    osg::Vec3 location;

    //Loop over all collision points and find impacts
    const int numManifolds( m_dispatcher1->getNumManifolds() );
    //std::cout << "num manifolds " << numManifolds << std::endl;
    for( unsigned int i = 0; i < numManifolds; ++i )
    {
        const btPersistentManifold* contactManifold(
            m_dispatcher1->getManifoldByIndexInternal( i ) );

        const btCollisionObject* obA( static_cast< const btCollisionObject* >(
            contactManifold->getBody0() ) );
        const btCollisionObject* obB( static_cast< const btCollisionObject* >(
            contactManifold->getBody1() ) );

        //Character controller is not a PhysicsRigidBody,
        //so does not have function GetSoundMaterial()
        //Need a better solution for this scenario in the future
        if( obA->getCollisionFlags() & btCollisionObject::CF_CHARACTER_OBJECT ||
            obB->getCollisionFlags() & btCollisionObject::CF_CHARACTER_OBJECT )
        {
            continue;
        }

        const int numContacts( contactManifold->getNumContacts() );
        //std::cout << "num contacts " << numContacts << std::endl;
        for( unsigned int j = 0; j < numContacts; ++j )
        {
            const btManifoldPoint& pt( contactManifold->getContactPoint( j ) );
            location = osgbBullet::asOsgVec3( pt.getPositionWorldOnA() );
            //std::cout << " life " << pt.m_lifeTime << std::endl;
            //std::cout << " impulse " << pt.m_appliedImpulse << std::endl;
            if( pt.m_lifeTime < 3 )
            {
                /*
                btRigidBody* rbA = btRigidBody::upcast( obA );
                btRigidBody* rbB = btRigidBody::upcast( obB );
                if( rbA && rbA )
                {
                    const btVector3 &v1 = rbA->getLinearVelocity();
                    const btVector3 &a1 = rbA->getAngularVelocity();
                    float vel1 = v1.x() * v1.x() + v1.y() * v1.y() + v1.z() * v1.z();
                    float ang1 = a1.x() * a1.x() + a1.y() * a1.y() + a1.z() * a1.z();
                    if( ( vel1 + ang1 ) > 2.0 )
                    {
                        collide = true;
                    }
                }
                //Need to tie this impulse to gain
                else
                */
                //Kind of a hack
                if( pt.m_appliedImpulse > 2.0 )
                {
                    collide = true;
                }
            }
            else
            {
                osg::Vec3 vA( osgbBullet::asOsgVec3(
                    obA->getInterpolationLinearVelocity() ) );
                osg::Vec3 vB( osgbBullet::asOsgVec3(
                    obB->getInterpolationLinearVelocity() ) );
                if( ( vA - vB ).length2() > 0.1 )
                {
                    slide = true;
                }
            }

            if( collide || slide )
            {
                void* tempUserDataA = obA->getUserPointer();
                void* tempUserDataB = obB->getUserPointer();
                if( !tempUserDataB || !tempUserDataA )
                {
                    continue;
                }

                PhysicsRigidBody* objA =
                    static_cast< PhysicsRigidBody* >( tempUserDataA );
                PhysicsRigidBody* objB =
                    static_cast< PhysicsRigidBody* >( tempUserDataB );

                Material* mcA = objA->GetSoundMaterial();
                Material* mcB = objB->GetSoundMaterial();
                //std::cout << "get material sounds " << std::endl;
                if( ( mcA != NULL ) && ( mcB != NULL ) )
                {
                    if( collide )
                    {
                        SoundUtilities::instance()->collide(
                            mcA->_mat, mcB->_mat, location, pt.m_appliedImpulse );
                    }
                    else
                    {
                        SoundUtilities::instance()->slide(
                            mcA->_mat, mcB->_mat, location );
                    }
                }
            }
            collide = false;
            slide = false;
        }
    }
#endif //VE_SOUND

    return numSimulationSubSteps;
}
////////////////////////////////////////////////////////////////////////////////
