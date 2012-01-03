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

#ifndef VES_XPLORER_SCENEGRAPH_KINEMATICCHARACTERCONTROLLER_H
#define VES_XPLORER_SCENEGRAPH_KINEMATICCHARACTERCONTROLLER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btActionInterface.h>

#include <BulletCollision/BroadphaseCollision/btCollisionAlgorithm.h>

class btDynamicsWorld;
class btCollisionShape;
class btRigidBody;
class btCollisionWorld;
class btCollisionDispatcher;
class btPairCachingGhostObject;
class btConvexShape;

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

/*!\file KinematicCharacterController.h
 * \class ves::xplorer::scenegraph::KinematicCharacterController
 * \namespace ves::xplorer::scenegraph
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
    bool canJump() const;

    ///btActionInterface interface
    virtual void debugDraw( btIDebugDraw* debugDrawer );

    ///
    void EnableFlying( bool const& canFly = true );

    ///
    btPairCachingGhostObject* GetGhostObject() const;

    ///
    btScalar getGravity() const;

    ///
    btScalar GetJumpHeight( btScalar elapsedTime );

    ///
    btScalar getMaxSlope() const;

    ///
    static btVector3* getUpAxisDirections();

    ///
    bool IsFlying() const;

    ///
    void jump();

    ///
    bool onGround() const;

    ///
    void preStep( btCollisionWorld* collisionWorld );

    ///Do sweep tests above the character, to the side the character is moving,
    ///and below the character, depending on the character's velocity
    void playerStep( btCollisionWorld* collisionWorld, btScalar dt );

    ///
    void Reset();

    /// This should probably be called setPositionIncrementPerSimulatorStep.
    /// This is neither a direction nor a velocity, but the amount to
    ///   increment the position each simulation iteration, regardless
    ///   of dt.
    /// This call will reset any velocity set by setVelocityForTimeInterval().
    virtual void setDisplacement( const btVector3& displacement );

    ///Caller provides a velocity with which the character should move for
    ///the given time period. After the time period, velocity is reset to zero
    ///This call will reset any walk direction set by setWalkDirection().
    ///Negative time intervals will result in no motion.
    virtual void setVelocityForTimeInterval(
        const btVector3& velocity, btScalar timeInterval );

    ///
    void SetConvexShape( btConvexShape* convexShape );

    ///
    void setFallSpeed( btScalar fallSpeed );

    ///
    void setGravity( btScalar gravity );

    ///
    void setJumpSpeed( btScalar jumpSpeed );

    ///
    void setMaxJumpHeight( btScalar maxJumpHeight );

    ///
    void setMaxSlope( btScalar slopeRadians );

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

    ///btActionInterface interface
    virtual void updateAction(
        btCollisionWorld* collisionWorld, btScalar deltaTime )
    {
        //Recover from penetrations
        preStep( collisionWorld );

        playerStep( collisionWorld, deltaTime );
    }

    ///This is bad for ghost objects
    void warp( const btVector3& origin );

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

    ///Do sweep test above character
    void stepUp( btCollisionWorld* collisionWorld );

    ///Do sweep test to side character is moving
    void stepForwardAndStrafe(
        btCollisionWorld* collisionWorld, const btVector3& walkMove );

    ///Do sweep test below character
    void stepDown( btCollisionWorld* collisionWorld, btScalar dt );

    ///
    void updateTargetPositionBasedOnCollision(
        const btVector3& hit_normal,
        btScalar tangentMag = btScalar( 0.0 ),
        btScalar normalMag = btScalar( 1.0 ) );

    ///
    PhysicsSimulator& m_physicsSimulator;

    ///
    btDynamicsWorld& m_dynamicsWorld;

    ///
    bool m_touchingContact;

    ///
    bool m_wasOnGround;

    ///
    bool m_useWalkDirection;

    ///Is the character flying?
    bool m_fly;

    ///Is the character jumping?
    bool m_jump;

    ///
    unsigned int m_upAxis;

    ///
    btScalar m_halfHeight;

    ///
    btScalar m_verticalVelocity;

    ///
    btScalar m_verticalOffset;

    ///
    btScalar m_fallSpeed;

    ///
    btScalar m_jumpSpeed;

    ///
    btScalar m_maxJumpHeight;

    ///Slope angle that is set (used for returning the exact value)
    btScalar m_maxSlopeRadians;

    ///Cosine equivalent of m_maxSlopeRadians (calculated once when set, for optimization)
    btScalar m_maxSlopeCosine;

    ///
    btScalar m_gravity;

    ///
    btScalar m_turnAngle;

    ///
    btScalar m_stepHeight;

    ///@todo: remove this and fix the code
    btScalar m_addedMargin;

    ///The amount we test above the character for collision
    btScalar  m_currentStepOffset;

    ///
    btScalar m_velocityTimeInterval;

    ///
    btScalar m_forwardBackwardSpeedModifier;

    ///
    btScalar m_leftRightSpeedModifier;

    ///
    btScalar m_upDownSpeedModifier;

    ///
    btScalar m_flySpeedModifier;

    ///
    btScalar m_vo;

    ///
    btScalar m_jumpHeight;

    ///
    btScalar m_jumpTime;

    ///
    btScalar m_characterWidth;

    ///
    btScalar m_characterHeight;

    ///This is the desired walk direction, set by the user
    btVector3 m_displacement;

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
    btPairCachingGhostObject* m_ghostObject;

    ///Is also in m_ghostObject, but it needs to be convex,
    ///so we store it here to avoid upcast
    btConvexShape* m_convexShape;

private:
    ///
    void DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint );

    ///
    osg::ref_ptr< osg::Geode > m_lineGeode;

};

} // end scenegraph
} // end xplorer
} // end ves

#endif //VES_XPLORER_SCENEGRAPH_KINEMATICCHARACTERCONTROLLER_H
