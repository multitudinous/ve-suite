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
#include <ves/xplorer/scenegraph/physics/character/KinematicCharacterController.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

// --- Bullet Includes --- //
#include <LinearMath/btIDebugDraw.h>
#include <LinearMath/btMotionState.h>
#include <LinearMath/btDefaultMotionState.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>
#include <BulletCollision/CollisionDispatch/btGhostObject.h>
#include <BulletCollision/CollisionShapes/btMultiSphereShape.h>
#include <BulletCollision/BroadphaseCollision/btOverlappingPairCache.h>
#include <BulletCollision/BroadphaseCollision/btCollisionAlgorithm.h>
#include <BulletCollision/CollisionDispatch/btCollisionWorld.h>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
static btVector3 upAxisDirection[ 3 ] =
{
    btVector3( 1.0, 0.0, 0.0 ),
    btVector3( 0.0, 1.0, 0.0 ),
    btVector3( 0.0, 0.0, 1.0 )
};
////////////////////////////////////////////////////////////////////////////////
static btVector3 getNormalizedVector( const btVector3& v )
{
    btVector3 n = v.normalized();
    if( n.length() < SIMD_EPSILON )
    {
        n.setValue( 0.0, 0.0, 0.0 );
    }

    return n;
}
////////////////////////////////////////////////////////////////////////////////
///@todo Interact with dynamic objects,
///Ride kinematicly animated platforms properly
///More realistic (or maybe just a config option) falling
/// -> Should integrate falling velocity manually and use that in stepDown()
///Support jumping
///Support ducking
class btKinematicClosestNotMeRayResultCallback :
    public btCollisionWorld::ClosestRayResultCallback
{
public:
    btKinematicClosestNotMeRayResultCallback( btCollisionObject* me ) :
        btCollisionWorld::ClosestRayResultCallback(
                btVector3( 0.0, 0.0, 0.0 ), btVector3( 0.0, 0.0, 0.0 ) )
    {
        m_me = me;
    }

    virtual btScalar addSingleResult(
        btCollisionWorld::LocalRayResult& rayResult,
        bool normalInWorldSpace )
    {
        if( rayResult.m_collisionObject == m_me )
        {
            return 1.0;
        }

        return ClosestRayResultCallback::addSingleResult(
            rayResult, normalInWorldSpace );
    }

protected:
    btCollisionObject* m_me;

};
////////////////////////////////////////////////////////////////////////////////
class btKinematicClosestNotMeConvexResultCallback :
    public btCollisionWorld::ClosestConvexResultCallback
{
public:
    btKinematicClosestNotMeConvexResultCallback( btCollisionObject* me ) :
        btCollisionWorld::ClosestConvexResultCallback(
            btVector3( 0.0, 0.0, 0.0 ), btVector3( 0.0, 0.0, 0.0 ) )
    {
        m_me = me;
    }

    virtual btScalar addSingleResult(
        btCollisionWorld::LocalConvexResult& convexResult,
        bool normalInWorldSpace )
    {
        if( convexResult.m_hitCollisionObject == m_me )
        {
            return 1.0;
        }

        return ClosestConvexResultCallback::addSingleResult(
            convexResult, normalInWorldSpace );
    }

protected:
    btCollisionObject* m_me;

};
////////////////////////////////////////////////////////////////////////////////
KinematicCharacterController::KinematicCharacterController()
    :
    btActionInterface(),
    m_fly( false ),
    m_jump( false ),
    m_touchingContact( false ),
    m_useGhostObjectSweepTest( true ),
    m_useWalkDirection( true ),//Use walk direction by default, legacy behavior
    m_upAxis( 2 ),
    //This is the speed of the character in ft/s
    //Slow walk speed is 5 km/h ~ 1.0 ft/s
    //Usain Bolt's top 10m split 10m/0.82s ~ 40 ft/s
    m_forwardBackwardSpeedModifier( 15.0 ),
    m_leftRightSpeedModifier( 15.0 ),
    m_upDownSpeedModifier( 15.0 ),
    m_flySpeedModifier( 4.0 ),
    m_velocityTimeInterval( 0.0 ),
    m_vo( 0.0 ),
    m_jumpTime( 0.0 ),
    //m_halfHeight(),
    m_ghostObject( new btPairCachingGhostObject() ),
    m_convexShape( NULL ),
    m_fallSpeed( 0.0 ),
    m_jumpSpeed( 15.0 ),
    m_maxJumpHeight( 0.0 ),
    //m_turnAngle( 0.0 ),
    m_stepHeight( 1.0 ),
    m_currentStepOffset( 0.0 ),
    m_addedMargin( 0.02 ),
    m_direction( 0.0, 0.0, 0.0 ),
    m_normalizedDirection( 0.0, 0.0, 0.0 ),
    m_currentPosition( 0.0, 0.0, 0.0 ),
    m_targetPosition( 0.0, 0.0, 0.0 ),
    m_touchingNormal( 0.0, 0.0, 0.0 ),
    m_characterWidth( 1.83 ),
    //The average height of a male in the U.S. is 5.83 ft
    m_characterHeight( 6.0/*5.83*/ ),
    m_physicsSimulator( *PhysicsSimulator::instance() )
{
    btDynamicsWorld* dynamicsWorld = m_physicsSimulator.GetDynamicsWorld();

    btBroadphaseInterface* broadphase = dynamicsWorld->getBroadphase();
    broadphase->getOverlappingPairCache()->setInternalGhostPairCallback(
        new btGhostPairCallback() );

    m_ghostObject->setWorldTransform( btTransform::getIdentity() );

    btCapsuleShapeZ* convexShape =
        new btCapsuleShapeZ( m_characterWidth, m_characterHeight );
    SetConvexShape( convexShape );
}
////////////////////////////////////////////////////////////////////////////////
KinematicCharacterController::~KinematicCharacterController ()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::SetConvexShape( btConvexShape* convexShape )
{
    m_convexShape = convexShape;
    m_ghostObject->setCollisionShape( m_convexShape );
    m_ghostObject->setCollisionFlags( btCollisionObject::CF_CHARACTER_OBJECT );
}
////////////////////////////////////////////////////////////////////////////////
btVector3 KinematicCharacterController::computeReflectionDirection(
    const btVector3& direction, const btVector3& normal )
{
    return direction - ( btScalar( 2.0 ) * direction.dot( normal ) ) * normal;
}
////////////////////////////////////////////////////////////////////////////////
btVector3 KinematicCharacterController::parallelComponent(
    const btVector3& direction, const btVector3& normal )
{
    btScalar magnitude = direction.dot( normal );

    return normal * magnitude;
}
////////////////////////////////////////////////////////////////////////////////
btVector3 KinematicCharacterController::perpindicularComponent(
    const btVector3& direction, const btVector3& normal )
{
    return direction - parallelComponent( direction, normal );
}
////////////////////////////////////////////////////////////////////////////////
btPairCachingGhostObject* const KinematicCharacterController::GetGhostObject() const
{
    return m_ghostObject;
}
////////////////////////////////////////////////////////////////////////////////
bool KinematicCharacterController::recoverFromPenetration(
    btCollisionWorld* collisionWorld )
{
    bool penetration = false;
    btHashedOverlappingPairCache* btHOPC =
        m_ghostObject->getOverlappingPairCache();

    collisionWorld->getDispatcher()->dispatchAllCollisionPairs(
        btHOPC,
        collisionWorld->getDispatchInfo(),
        collisionWorld->getDispatcher() );

    m_currentPosition = m_ghostObject->getWorldTransform().getOrigin();

    btScalar maxPen = btScalar( 0.0 );
    for( int i = 0; i < btHOPC->getNumOverlappingPairs(); ++i )
    {
        m_manifoldArray.resize( 0 );

        btBroadphasePair* collisionPair =
            &btHOPC->getOverlappingPairArray()[ i ];

        if( collisionPair->m_algorithm )
        {
            collisionPair->m_algorithm->getAllContactManifolds( m_manifoldArray );
        }

        for( int j = 0; j < m_manifoldArray.size(); ++j )
        {
            btPersistentManifold* manifold = m_manifoldArray[ j ];
            btScalar directionSign =
                manifold->getBody0() == m_ghostObject ? btScalar( -1.0 ) : btScalar( 1.0 );
            for( int p = 0; p < manifold->getNumContacts(); ++p )
            {
                const btManifoldPoint&pt = manifold->getContactPoint( p );

                if( pt.getDistance() < 0.0 )
                {
                    if( pt.getDistance() < maxPen )
                    {
                        maxPen = pt.getDistance();
                        m_touchingNormal = pt.m_normalWorldOnB * directionSign;//??
                    }

                    m_currentPosition += pt.m_normalWorldOnB * directionSign * pt.getDistance() * btScalar( 0.2 );
                    penetration = true;
                }
                else
                {
                    ;
                }
            }

            //manifold->clearManifold();
        }
    }

    btTransform newTrans = m_ghostObject->getWorldTransform();
    newTrans.setOrigin( m_currentPosition );
    m_ghostObject->setWorldTransform( newTrans );

    return penetration;
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::stepUp( btCollisionWorld* world )
{
    //Phase 1: up
    btTransform start, end;
    start.setIdentity();
    end.setIdentity();

    m_targetPosition =
        m_currentPosition + upAxisDirection[ m_upAxis ] * m_stepHeight;

    /* FIXME: Handle penetration properly */
    start.setOrigin(
        m_currentPosition + upAxisDirection[ m_upAxis ] * btScalar( 0.1 ) );
    end.setOrigin( m_targetPosition );

    btKinematicClosestNotMeConvexResultCallback callback( m_ghostObject );
    callback.m_collisionFilterGroup =
        m_ghostObject->getBroadphaseHandle()->m_collisionFilterGroup;
    callback.m_collisionFilterMask =
        m_ghostObject->getBroadphaseHandle()->m_collisionFilterMask;

    if( m_useGhostObjectSweepTest )
    {
        m_ghostObject->convexSweepTest(
            m_convexShape, start, end,
            callback, world->getDispatchInfo().m_allowedCcdPenetration );
    }
    else
    {
        world->convexSweepTest( m_convexShape, start, end, callback );
    }

    if( callback.hasHit() )
    {
        //We moved up only a fraction of the step height
        m_currentStepOffset = m_stepHeight * callback.m_closestHitFraction;
        m_currentPosition.setInterpolate3(
            m_currentPosition, m_targetPosition, callback.m_closestHitFraction );
    }
    else
    {
        m_currentStepOffset = m_stepHeight;
        m_currentPosition = m_targetPosition;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::updateTargetPositionBasedOnCollision(
    const btVector3& hitNormal, btScalar tangentMag, btScalar normalMag )
{
    btVector3 movementDirection = m_targetPosition - m_currentPosition;
    btScalar movementLength = movementDirection.length();
    if( movementLength > SIMD_EPSILON )
    {
        movementDirection.normalize();

        btVector3 reflectDir =
            computeReflectionDirection( movementDirection, hitNormal );
        reflectDir.normalize();

        btVector3 parallelDir, perpindicularDir;

        parallelDir = parallelComponent( reflectDir, hitNormal );
        perpindicularDir = perpindicularComponent( reflectDir, hitNormal );

        m_targetPosition = m_currentPosition;
        if( 0 )//tangentMag != 0.0 )
        {
            btVector3 parComponent =
                parallelDir * btScalar( tangentMag * movementLength );
            m_targetPosition +=  parComponent;
        }

        if( normalMag != 0.0 )
        {
            btVector3 perpComponent =
                perpindicularDir * btScalar( normalMag * movementLength );
            m_targetPosition += perpComponent;
        }
    }
    else
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::stepForwardAndStrafe(
    btCollisionWorld* collisionWorld, const btVector3& walkMove )
{
    //Phase 2: forward and strafe
    btTransform start, end;
    m_targetPosition = m_currentPosition + walkMove;
    start.setIdentity ();
    end.setIdentity ();

    btScalar fraction = 1.0;
    btScalar distance2 = ( m_currentPosition-m_targetPosition ).length2();

    if( m_touchingContact )
    {
        if( m_normalizedDirection.dot( m_touchingNormal ) > btScalar( 0.0 ) )
        {
            updateTargetPositionBasedOnCollision( m_touchingNormal );
        }
    }

    int maxIter = 10;

    while( fraction > btScalar( 0.01 ) && --maxIter > 0 )
    {
        start.setOrigin( m_currentPosition );
        end.setOrigin( m_targetPosition );

        btKinematicClosestNotMeConvexResultCallback callback( m_ghostObject );
        callback.m_collisionFilterGroup =
            m_ghostObject->getBroadphaseHandle()->m_collisionFilterGroup;
        callback.m_collisionFilterMask =
            m_ghostObject->getBroadphaseHandle()->m_collisionFilterMask;

        btScalar margin = m_convexShape->getMargin();
        m_convexShape->setMargin( margin + m_addedMargin );

        if( m_useGhostObjectSweepTest )
        {
            m_ghostObject->convexSweepTest(
                m_convexShape, start, end, callback,
                collisionWorld->getDispatchInfo().m_allowedCcdPenetration );
        }
        else
        {
            collisionWorld->convexSweepTest(
                m_convexShape, start, end, callback,
                collisionWorld->getDispatchInfo().m_allowedCcdPenetration );
        }

        m_convexShape->setMargin( margin );

        fraction -= callback.m_closestHitFraction;

        if( callback.hasHit() )
        {
            //We moved only a fraction
            btScalar hitDistance =
                ( callback.m_hitPointWorld - m_currentPosition ).length();
            if( hitDistance < 0.0 )
            {
                ;
            }

            //If the distance is farther than the collision margin, move
            if( hitDistance > m_addedMargin )
            {
                m_currentPosition.setInterpolate3(
                    m_currentPosition, m_targetPosition, callback.m_closestHitFraction );
            }

            updateTargetPositionBasedOnCollision( callback.m_hitNormalWorld );
            btVector3 currentDir = m_targetPosition - m_currentPosition;
            distance2 = currentDir.length2();
            if( distance2 > SIMD_EPSILON )
            {
                currentDir.normalize();
                //See Quake2: If velocity is against original velocity,
                //stop ead to avoid tiny oscilations in sloping corners
                if( currentDir.dot( m_normalizedDirection ) <= btScalar( 0.0 ) )
                {
                    break;
                }
            }
            else
            {
                break;
            }
        }
        else
        {
            //We moved whole way
            m_currentPosition = m_targetPosition;
        }

        //if( callback.m_closestHitFraction == 0.0 )
        //break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::stepDown(
    btCollisionWorld* collisionWorld, btScalar dt )
{
    btTransform start, end;

    //Phase 3: down
    btVector3 step_drop = upAxisDirection[ m_upAxis ] * m_currentStepOffset;
    btVector3 gravity_drop = upAxisDirection[ m_upAxis ] * m_stepHeight;
    m_targetPosition -= step_drop + gravity_drop;

    start.setIdentity();
    end.setIdentity();

    start.setOrigin( m_currentPosition );
    end.setOrigin( m_targetPosition );

    btKinematicClosestNotMeConvexResultCallback callback( m_ghostObject );
    callback.m_collisionFilterGroup =
        m_ghostObject->getBroadphaseHandle()->m_collisionFilterGroup;
    callback.m_collisionFilterMask =
        m_ghostObject->getBroadphaseHandle()->m_collisionFilterMask;

    if( m_useGhostObjectSweepTest )
    {
        m_ghostObject->convexSweepTest(
            m_convexShape, start, end, callback,
            collisionWorld->getDispatchInfo().m_allowedCcdPenetration );
    }
    else
    {
        collisionWorld->convexSweepTest(
            m_convexShape, start, end, callback,
            collisionWorld->getDispatchInfo().m_allowedCcdPenetration );
    }

    if( callback.hasHit() )
    {
        //We dropped a fraction of the height -> hit floor
        m_currentPosition.setInterpolate3(
            m_currentPosition, m_targetPosition, callback.m_closestHitFraction );
    }
    else
    {
        //We dropped the full height
        m_currentPosition = m_targetPosition;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::setWalkDirection(
    const btVector3& walkDirection )
{
    m_useWalkDirection = true;
    m_direction = walkDirection;
    m_normalizedDirection = getNormalizedVector( m_direction );
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::setVelocityForTimeInterval(
    const btVector3& velocity, btScalar timeInterval )
{
    m_useWalkDirection = false;
    m_direction = velocity;
    m_normalizedDirection = getNormalizedVector( m_direction );
    m_velocityTimeInterval = timeInterval;
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::Reset()
{
    btDynamicsWorld* dynamicsWorld = m_physicsSimulator.GetDynamicsWorld();
    btDispatcher* dispatcher = dynamicsWorld->getDispatcher();
    btBroadphaseInterface* broadphase = dynamicsWorld->getBroadphase();
    broadphase->getOverlappingPairCache()->cleanProxyFromPairs(
        m_ghostObject->getBroadphaseHandle(), dispatcher );

    //Move the character so that its entire body is above the zero ground plane
    warp( btVector3( 0.0, 0.0, m_characterHeight ) );
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::EnableFlying( const bool& canFly )
{
    m_fly = canFly;
    if( m_fly )
    {
        m_forwardBackwardSpeedModifier *= m_flySpeedModifier;
        m_leftRightSpeedModifier *= m_flySpeedModifier;
        m_upDownSpeedModifier *= m_flySpeedModifier;
    }
    else
    {
        double speedModifier = 1.0 / m_flySpeedModifier;
        m_forwardBackwardSpeedModifier *= speedModifier;
        m_leftRightSpeedModifier *= speedModifier;
        m_upDownSpeedModifier *= speedModifier;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::warp( const btVector3& origin )
{
    btTransform xform;
    xform.setIdentity();
    xform.setOrigin( origin );
    m_ghostObject->setWorldTransform( xform );
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::preStep( btCollisionWorld* collisionWorld )
{
    unsigned int numPenetrationLoops( 0 );
    m_touchingContact = false;
    while( recoverFromPenetration( collisionWorld ) )
    {
        ++numPenetrationLoops;
        m_touchingContact = true;
        if( numPenetrationLoops > 4 )
        {
            break;
        }
    }

    m_currentPosition = m_ghostObject->getWorldTransform().getOrigin();
    m_targetPosition = m_currentPosition;
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::playerStep(
    btCollisionWorld* collisionWorld, btScalar dt )
{
    //Quick check...
    if( !m_useWalkDirection && m_velocityTimeInterval <= 0.0 )
    {
        //No motion
        return;
    }

    btTransform xform;
    xform = m_ghostObject->getWorldTransform();

    if( !m_fly )
    {
        stepUp( collisionWorld );
    }

    if( m_useWalkDirection )
    {
        stepForwardAndStrafe( collisionWorld, m_direction );
    }
    else
    {
        //Still have some time left for moving!
        btScalar dtMoving =
           ( dt < m_velocityTimeInterval ) ? dt : m_velocityTimeInterval;
        m_velocityTimeInterval -= dt;

        //How far will we move while we are moving?
        btVector3 move = m_direction * dtMoving;

        //Okay, step
        stepForwardAndStrafe( collisionWorld, move );
    }

    if( !m_fly )
    {
        stepDown( collisionWorld, dt );
    }

    xform.setOrigin( m_currentPosition );
    m_ghostObject->setWorldTransform( xform );
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::setFallSpeed( btScalar fallSpeed )
{
    m_fallSpeed = fallSpeed;
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::setJumpSpeed( btScalar jumpSpeed )
{
    m_jumpSpeed = jumpSpeed;
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::setMaxJumpHeight( btScalar maxJumpHeight )
{
    m_maxJumpHeight = maxJumpHeight;
}
////////////////////////////////////////////////////////////////////////////////
const bool KinematicCharacterController::CanFly() const
{
    return m_fly;
}
////////////////////////////////////////////////////////////////////////////////
bool KinematicCharacterController::canJump() const
{
    return onGround();
}
////////////////////////////////////////////////////////////////////////////////
bool KinematicCharacterController::onGround() const
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::debugDraw( btIDebugDraw* debugDrawer )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double KinematicCharacterController::GetHeight( double elapsedTime )
{
    if( !m_jump )
    {
        return 0.0;
    }

    const double G = -9.8;

    //vt = vo + G*t
    double vt = m_vo + G * m_jumpTime;
    m_jumpTime += elapsedTime;

    //s = vt*t + 1/2G*t^2
    return vt * elapsedTime + 0.5 * G * elapsedTime * elapsedTime;
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::StartJump( double vo )
{
    if( m_jump )
    {
        return;
    }

    m_jumpTime = 0.0;
    m_vo = vo;
    m_jump = true;
}
////////////////////////////////////////////////////////////////////////////////
void KinematicCharacterController::StopJump()
{
    m_jump = false;
}
////////////////////////////////////////////////////////////////////////////////
