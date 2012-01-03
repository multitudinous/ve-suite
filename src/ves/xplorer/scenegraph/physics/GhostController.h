/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#ifndef VES_XPLORER_SCENEGRAPH_KINEMATICRIGIDBODYCONTROLLER_H
#define VES_XPLORER_SCENEGRAPH_KINEMATICRIGIDBODYCONTROLLER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btActionInterface.h>

class btDynamicsWorld;
class btCollisionShape;
class btCollisionWorld;
class btPairCachingGhostObject;
class btConvexShape;
class btMotionState;

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Vec3d>

namespace osg
{
class Geode;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class PhysicsSimulator;
class btClosestNotMeConvexResultCallback;

/*!\file GhostController.h
 *
 */

/*!\class ves::xplorer::scenegraph::GhostController
 *
 */

/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS GhostController : public btActionInterface
{
public:
    ///Constructor
    GhostController();

    ///Destructor
    ~GhostController();

    ///
    virtual void debugDraw( btIDebugDraw* debugDrawer );

    ///
    btPairCachingGhostObject& GetGhostObject() const;

    ///
    btMotionState* GetMotionState() const;

    ///
    void Reset();

    ///
    void Rotate(
        btQuaternion const& deltaRotation, bool const& execute = false );

    ///
    void SetCollisionShape( btCollisionShape* collisionShape );

    ///
    void SetMotionState( btMotionState* const motionState );

    ///
    void Translate(
        btVector3 const& deltaTranslation, bool const& execute = false );

    ///btActionInterface interface
    virtual void updateAction(
        btCollisionWorld* collisionWorld, btScalar deltaTime );

protected:

private:
    ///Do sweep test to the delta transform the ghost object is moving
    void Advance(
        btScalar dt,
        btVector3 const& transStep, btQuaternion const& rotStep );

    ///Returns the reflection direction of a ray going 'direction' hitting a surface with normal 'normal'
    btVector3 ComputeReflectionDirection(
        btVector3 const& direction, btVector3 const& normal );

    ///
    void Depenetrate();

    ///
    void Move( btScalar dt );

    ///Returns the portion of 'direction' that is parallel to 'normal'
    btVector3 ParallelComponent(
        btVector3 const& direction, btVector3 const& normal );

    ///Returns the portion of 'direction' that is perpindicular to 'normal'
    btVector3 PerpindicularComponent(
        btVector3 const& direction, btVector3 const& normal );

    ///
    bool RecoverFromPenetration();

    ///
    void RecursiveCCD(
        btScalar dt,
        btCollisionShape const& collisionShape,
        btClosestNotMeConvexResultCallback& sweepResults );

    ///
    void UpdateTargetPositionBasedOnCollision(
        btVector3 const& hit_normal,
        btScalar tangentMag = btScalar( 0.0 ),
        btScalar normalMag = btScalar( 1.0 ) );

    ///
    btPairCachingGhostObject* m_ghostObject;

    ///
    btCollisionShape* m_collisionShape;

    ///Allows synchronization to world transform for corresponding btRigidBody
    btMotionState* m_motionState;

    ///
    PhysicsSimulator& m_physicsSimulator;

    ///
    btDynamicsWorld& m_dynamicsWorld;

    ///
    bool m_hitObject;

    ///
    unsigned int m_maxNumPenLoops;

    ///
    btScalar m_maxPenetrationCorrection;

    ///
    btTransform& m_currentTransform;

    ///
    btTransform m_targetTransform;

    ///
    btVector3 m_deltaTranslation;

    ///
    btVector3 m_normalizedDirection;

    ///
    btVector3& m_currentPosition;

    ///
    btVector3 m_targetPosition;

    ///
    btQuaternion m_deltaRotation;

    ///
    btQuaternion m_normalizedRotation;

    ///
    btQuaternion m_currentAttitude;

    ///
    btQuaternion m_targetAttitude;

    ///
    btMatrix3x3& m_currentBasis;

};

} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_KINEMATICRIGIDBODYCONTROLLER_H
