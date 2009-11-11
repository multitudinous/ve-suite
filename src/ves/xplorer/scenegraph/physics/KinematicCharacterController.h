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

#ifndef VES_XPLORER_SCENEGRAPH_KINEMATIC_CHARACTER_CONTROLLER_H
#define VES_XPLORER_SCENEGRAPH_KINEMATIC_CHARACTER_CONTROLLER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btActionInterface.h>

#include <BulletCollision/BroadphaseCollision/btCollisionAlgorithm.h>

class btCollisionShape;
class btRigidBody;
class btCollisionWorld;
class btCollisionDispatcher;
class btPairCachingGhostObject;
class btConvexShape;

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

/*!\file KinematicCharacterController.h
 *
 */
/*!\class ves::xplorer::scenegraph::KinematicCharacterController
 *
 */
/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS KinematicCharacterController :
    public btActionInterface
{
public:
    ///Constructor
    KinematicCharacterController();

    ///Destructor
    ~KinematicCharacterController();

    ///
    btPairCachingGhostObject* const GetGhostObject() const;

    ///
    void SetConvexShape( btConvexShape* convexShape );

    ///btActionInterface interface
    virtual void updateAction(
        btCollisionWorld* collisionWorld, btScalar deltaTime )
    {
        preStep( collisionWorld );
        playerStep( collisionWorld, deltaTime );
    }

    ///btActionInterface interface
    void debugDraw( btIDebugDraw* debugDrawer );

    ///
    void setUpAxis( int axis )
    {
        if( axis < 0 )
        {
            axis = 0;
        }

        if( axis > 2 )
        {
            axis = 2;
        }

        m_upAxis = axis;
    }

    /// This should probably be called setPositionIncrementPerSimulatorStep.
    /// This is neither a direction nor a velocity, but the amount to
    ///   increment the position each simulation iteration, regardless
    ///   of dt.
    /// This call will reset any velocity set by setVelocityForTimeInterval().
    virtual void setWalkDirection( const btVector3& walkDirection );

    ///Caller provides a velocity with which the character should move for
    ///the given time period. After the time period, velocity is reset to zero
    ///This call will reset any walk direction set by setWalkDirection().
    ///Negative time intervals will result in no motion.
    virtual void setVelocityForTimeInterval(
        const btVector3& velocity, btScalar timeInterval );

    ///
    void Reset();

    ///
    void warp( const btVector3& origin );

    ///
    void preStep( btCollisionWorld* collisionWorld );

    ///
    void playerStep( btCollisionWorld* collisionWorld, btScalar dt );

    ///
    void setFallSpeed( btScalar fallSpeed );

    ///
    void setJumpSpeed( btScalar jumpSpeed );

    ///
    void setMaxJumpHeight( btScalar maxJumpHeight );

    ///
    bool canJump() const;

    ///
    void setUseGhostSweepTest( bool useGhostObjectSweepTest )
    {
        m_useGhostObjectSweepTest = useGhostObjectSweepTest;
    }

    ///
    bool onGround() const;

protected:
    ///Returns the reflection direction of a ray going 'direction' hitting a surface with normal 'normal'
    ///From: http://www-cs-students.stanford.edu/~adityagp/final/node3.html
    btVector3 computeReflectionDirection(
        const btVector3& direction, const btVector3& normal );

    ///Returns the portion of 'direction' that is parallel to 'normal'
    btVector3 parallelComponent(
        const btVector3& direction, const btVector3& normal );

    ///Returns the portion of 'direction' that is perpindicular to 'normal'
    btVector3 perpindicularComponent(
        const btVector3& direction, const btVector3& normal );

    ///
    bool recoverFromPenetration( btCollisionWorld* collisionWorld );

    ///
    void stepUp( btCollisionWorld* collisionWorld );

    ///
    void updateTargetPositionBasedOnCollision(
        const btVector3& hit_normal,
        btScalar tangentMag = btScalar( 0.0 ),
        btScalar normalMag = btScalar( 1.0 ) );

    ///
    void stepForwardAndStrafe(
        btCollisionWorld* collisionWorld, const btVector3& walkMove );

    ///
    void stepDown( btCollisionWorld* collisionWorld, btScalar dt );

    ///
    bool m_jump;

    ///
    bool m_touchingContact;

    ///
    bool m_useGhostObjectSweepTest;

    ///
    bool m_useWalkDirection;

    ///
    int m_upAxis;

    ///
    double m_velocityTimeInterval;

    ///
    double m_vo;

    ///
    double m_jumpTime;

    ///
    //btScalar m_halfHeight;

    ///
    btPairCachingGhostObject* m_ghostObject;

    ///Is also in m_ghostObject, but it needs to be convex,
    ///so we store it here to avoid upcast
    btConvexShape* m_convexShape;

    ///
    btScalar m_fallSpeed;

    ///
    btScalar m_jumpSpeed;

    ///
    btScalar m_maxJumpHeight;

    ///
    //btScalar m_turnAngle;

    ///
    btScalar m_stepHeight;

    ///
    btScalar  m_currentStepOffset;

    ///@todo: remove this and fix the code
    btScalar m_addedMargin;

    ///This is the desired walk direction, set by the user
    btVector3 m_walkDirection;

    ///
    btVector3 m_normalizedDirection;

    ///Some internal variables
    btVector3 m_currentPosition;

    ///
    btVector3 m_targetPosition;

    ///
    btVector3 m_touchingNormal;

    ///Keep track of the contact manifolds
    btManifoldArray m_manifoldArray;

    ///
    double GetHeight( double elapsedTime );

    ///
    void StartJump( double vo );

    ///
    void StopJump();

    ///
    double m_characterWidth;

    ///
    double m_characterHeight;

private:

};

} // end scenegraph
} // end xplorer
} // end ves

#endif //VES_XPLORER_SCENEGRAPH_KINEMATIC_CHARACTER_CONTROLLER_H
