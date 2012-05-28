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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/GhostController.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

// --- Bullet Includes --- //
#include <LinearMath/btIDebugDraw.h>
#include <LinearMath/btMotionState.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletCollision/CollisionDispatch/btCollisionWorld.h>
#include <BulletCollision/CollisionDispatch/btGhostObject.h>

#include <BulletCollision/BroadphaseCollision/btOverlappingPairCache.h>
#include <BulletCollision/BroadphaseCollision/btCollisionAlgorithm.h>

// --- OSG Includes --- //
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

////////////////////////////////////////////////////////////////////////////////
class btClosestNotMeConvexResultCallback :
    public btCollisionWorld::ClosestConvexResultCallback
{
public:
    btClosestNotMeConvexResultCallback(
        btCollisionObject* me,
        const btVector3& fromA, const btVector3& toA,
        btOverlappingPairCache* pairCache, btDispatcher* dispatcher )
        :
        btCollisionWorld::ClosestConvexResultCallback( fromA, toA ),
        m_me( me ),
        m_allowedPenetration( 0.0 ),
        m_pairCache( pairCache ),
        m_dispatcher( dispatcher )
    {
        ;
    }

    virtual btScalar addSingleResult(
        btCollisionWorld::LocalConvexResult& convexResult,
        bool normalInWorldSpace )
    {
        if( convexResult.m_hitCollisionObject == m_me )
        {
            return 1.0;
        }

        //Ignore result if there is no contact response
        if( !convexResult.m_hitCollisionObject->hasContactResponse() )
        {
            return 1.0;
        }

        btVector3 linVelA, linVelB;
        linVelA = m_convexToWorld - m_convexFromWorld;
        linVelB = btVector3( 0.0, 0.0, 0.0 );//toB.getOrigin() - fromB.getOrigin();

        btVector3 relativeVelocity = ( linVelA - linVelB );
        //Don't report time of impact for motion away from the contact normal
        //(or causes minor penetration)
        if( convexResult.m_hitNormalLocal.dot( relativeVelocity ) >=
                -m_allowedPenetration )
        {
            return 1.0;
        }

        return ClosestConvexResultCallback::addSingleResult(
                   convexResult, normalInWorldSpace );
    }

    virtual bool needsCollision( btBroadphaseProxy* proxy0 ) const
    {
        //Don't collide with itself
        if( proxy0->m_clientObject == m_me )
        {
            return false;
        }

        //Don't do CCD when the collision filters are not matching
        if( !ClosestConvexResultCallback::needsCollision( proxy0 ) )
        {
            return false;
        }

        btCollisionObject* otherObj =
            ( btCollisionObject* )proxy0->m_clientObject;

        //Call needsResponse, see http://code.google.com/p/bullet/issues/detail?id=179
        if( m_dispatcher->needsResponse( m_me, otherObj ) )
        {
            //Don't do CCD when there are already contact points (touching contact/penetration)
            btAlignedObjectArray< btPersistentManifold* > manifoldArray;
            btBroadphasePair* collisionPair =
                m_pairCache->findPair( m_me->getBroadphaseHandle(), proxy0 );
            if( collisionPair )
            {
                if( collisionPair->m_algorithm )
                {
                    manifoldArray.resize( 0 );
                    collisionPair->m_algorithm->getAllContactManifolds(
                        manifoldArray );
                    for( int j = 0; j < manifoldArray.size(); ++j )
                    {
                        btPersistentManifold* manifold = manifoldArray[ j ];
                        if( manifold->getNumContacts() > 0 )
                        {
                            return false;
                        }
                    }
                }
            }
        }
        return true;
    }

    ///
    btCollisionObject* m_me;

    ///
    btScalar m_allowedPenetration;

    ///
    btOverlappingPairCache* m_pairCache;

    ///
    btDispatcher* m_dispatcher;

};
////////////////////////////////////////////////////////////////////////////////
static btVector3 GetNormalizedVector( btVector3 const& v )
{
    btVector3 n = v.normalized();
    if( n.length() < SIMD_EPSILON )
    {
        n.setValue( 0.0, 0.0, 0.0 );
    }

    return n;
}
////////////////////////////////////////////////////////////////////////////////
GhostController::GhostController()
    :
    btActionInterface(),
    m_ghostObject( new btPairCachingGhostObject() ),
    m_collisionShape( NULL ),
    m_motionState( NULL ),
    m_physicsSimulator( *( PhysicsSimulator::instance() ) ),
    m_dynamicsWorld( *( m_physicsSimulator.GetDynamicsWorld() ) ),
    m_hitObject( false ),
    m_maxNumPenLoops( 4 ),
    m_maxPenetrationCorrection( 0.02 ),
    m_currentTransform( m_ghostObject->getWorldTransform() ),
    m_targetTransform(),
    m_deltaTranslation( 0.0, 0.0, 0.0 ),
    m_normalizedDirection( 0.0, 0.0, 0.0 ),
    m_currentPosition( m_currentTransform.getOrigin() ),
    m_targetPosition( 0.0, 0.0, 0.0 ),
    m_deltaRotation( 0.0, 0.0, 0.0, 1.0 ),
    m_normalizedRotation( 0.0, 0.0, 0.0, 1.0 ),
    m_currentAttitude( 0.0, 0.0, 0.0, 1.0 ),
    m_targetAttitude( 0.0, 0.0, 0.0, 1.0 ),
    m_currentBasis( m_currentTransform.getBasis() )
{
    m_ghostObject->setContactProcessingThreshold( 0.0 );
    m_currentTransform.setIdentity();
    m_targetTransform.setIdentity();
}
////////////////////////////////////////////////////////////////////////////////
GhostController::~GhostController()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::Move( btScalar dt )
{
    if( !m_collisionShape->isConvex() && !m_collisionShape->isCompound() )
    {
        m_currentPosition += m_deltaTranslation;
        m_currentAttitude = m_deltaRotation * m_currentAttitude;

        return;
    }

    //Movement increment
    Advance( dt, m_deltaTranslation, m_deltaRotation );

    //Recover from collisions
    Depenetrate();
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::Advance(
    btScalar dt,
    btVector3 const& transStep, btQuaternion const& rotStep )
{
    m_targetPosition = m_currentPosition + transStep;
    m_targetAttitude = rotStep * m_currentAttitude;
    m_targetTransform.setOrigin( m_targetPosition );
    m_targetTransform.setRotation( m_targetAttitude );

    //Use sweep to test for objects whose AABB is outside of ghost objects AABB
    btClosestNotMeConvexResultCallback sweepResults(
        m_ghostObject,
        //This needs fixed to work with compound objects
        //m_currentPosition, m_targetPosition,
        btVector3( 0.0, 0.0, 0.0 ), btVector3( 0.0, 0.0, 0.0 ),
        m_ghostObject->getOverlappingPairCache(),
        m_dynamicsWorld.getDispatcher() );
    sweepResults.m_collisionFilterGroup =
        m_ghostObject->getBroadphaseHandle()->m_collisionFilterGroup;
    sweepResults.m_collisionFilterMask =
        m_ghostObject->getBroadphaseHandle()->m_collisionFilterMask;

    //Collect all hits for collision shape and its children
    RecursiveCCD( dt, *m_collisionShape, sweepResults );

    //Move to the closest hit fraction / TOI
    if( sweepResults.hasHit() && ( sweepResults.m_closestHitFraction < 1.0 ) )
    {
        btVector3 linvel, angvel;
        btTransformUtil::calculateVelocity(
            m_currentTransform, m_targetTransform, dt, linvel, angvel );

        btScalar timeStep = sweepResults.m_closestHitFraction * dt;
        btTransformUtil::integrateTransform(
            m_currentTransform, linvel, angvel, timeStep, m_targetTransform );
    }

    m_currentPosition = m_targetTransform.getOrigin();
    m_currentAttitude = m_targetTransform.getRotation();
}
////////////////////////////////////////////////////////////////////////////////
btVector3 GhostController::ComputeReflectionDirection(
    btVector3 const& direction, btVector3 const& normal )
{
    return direction - ( btScalar( 2.0 ) * direction.dot( normal ) ) * normal;
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::debugDraw( btIDebugDraw* )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::Depenetrate()
{
    unsigned int numPenetrationLoops( 0 );
    while( RecoverFromPenetration() )
    {
        ++numPenetrationLoops;
        if( numPenetrationLoops > m_maxNumPenLoops )
        {
            std::cout << "GhostController::Recover(): "
                      << "Could not recover from penetration!" << std::endl;
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
btPairCachingGhostObject& GhostController::GetGhostObject() const
{
    return *m_ghostObject;
}
////////////////////////////////////////////////////////////////////////////////
btMotionState* GhostController::GetMotionState() const
{
    return m_motionState;
}
////////////////////////////////////////////////////////////////////////////////
btVector3 GhostController::ParallelComponent(
    btVector3 const& direction, btVector3 const& normal )
{
    btScalar magnitude = direction.dot( normal );

    return normal * magnitude;
}
////////////////////////////////////////////////////////////////////////////////
btVector3 GhostController::PerpindicularComponent(
    btVector3 const& direction, btVector3 const& normal )
{
    return direction - ParallelComponent( direction, normal );
}
////////////////////////////////////////////////////////////////////////////////
bool GhostController::RecoverFromPenetration()
{
    //Declare temp variables
    btScalar maxPenetration( 0.0 );
    btVector3 contactNormal( 0.0, 0.0, 0.0 );

    //Update overlapping pairs for ghost object because we are moving
    m_dynamicsWorld.updateSingleAabb( m_ghostObject );

    //Process overlapping pairs
    btHashedOverlappingPairCache* btHOPC =
        m_ghostObject->getOverlappingPairCache();
    m_dynamicsWorld.getDispatcher()->dispatchAllCollisionPairs(
        btHOPC, m_dynamicsWorld.getDispatchInfo(), m_dynamicsWorld.getDispatcher() );

    //Process Broadphase Collision Detection
    //This loops over all AABBs that overlap w/ the ghost object AABB as pairs
    for( int i = 0; i < btHOPC->getNumOverlappingPairs(); ++i )
    {
        btBroadphasePair const& collisionPair =
            btHOPC->getOverlappingPairArray()[ i ];
        btCollisionAlgorithm* algo = collisionPair.m_algorithm;
        if( !algo )
        {
            continue;
        }

        //Process Narrowphase Collision Detection
        btManifoldArray manifoldArray;
        algo->getAllContactManifolds( manifoldArray );
        for( int j = 0; j < manifoldArray.size(); ++j )
        {
            btPersistentManifold* manifold = manifoldArray[ j ];
            for( int k = 0; k < manifold->getNumContacts(); ++k )
            {
                btManifoldPoint const& pt = manifold->getContactPoint( k );
                btScalar distance = pt.getDistance();
                if( ( distance >= -m_maxPenetrationCorrection ) ||
                        ( distance >= maxPenetration ) )
                {
                    continue;
                }

                maxPenetration = distance;
                contactNormal = pt.m_normalWorldOnB;
                if( manifold->getBody0() == m_ghostObject )
                {
                    contactNormal *= btScalar( -1.0 );
                }
            }
        }
    }

    if( maxPenetration < 0.0 )
    {
        //std::cout << "maxPenetration: " << maxPenetration << std::endl;
        m_currentPosition += contactNormal * maxPenetration;
        return true;
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::RecursiveCCD(
    btScalar dt,
    btCollisionShape const& collisionShape,
    btClosestNotMeConvexResultCallback& sweepResults )
{
    if( collisionShape.isCompound() )
    {
        btCompoundShape const& compoundShape =
            static_cast< btCompoundShape const& >( collisionShape );
        for( int i = 0; i < compoundShape.getNumChildShapes(); ++i )
        {
            btCollisionShape const& childShape =
                *compoundShape.getChildShape( i );
            RecursiveCCD( dt, childShape, sweepResults );
        }
    }
    //Do convex sweep test for convex shapes
    else if( collisionShape.isConvex() )
    {
        btConvexShape const& convexShape =
            static_cast< btConvexShape const& >( collisionShape );
        m_ghostObject->convexSweepTest(
            &convexShape,
            m_currentTransform, m_targetTransform, sweepResults,
            m_dynamicsWorld.getDispatchInfo().m_allowedCcdPenetration );
        //m_dynamicsWorld.contactTest( m_ghostObject,  );
    }
    //Dont handle concave objects yet
    /*
    else if( collisionShape.isConcave() )
    {
        btConcaveShape const& concaveShape =
            static_cast< btConcaveShape const& >( collisionShape );
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::Reset()
{
    btDispatcher* dispatcher = m_dynamicsWorld.getDispatcher();
    btBroadphaseInterface* broadphase = m_dynamicsWorld.getBroadphase();
    broadphase->getOverlappingPairCache()->cleanProxyFromPairs(
        m_ghostObject->getBroadphaseHandle(), dispatcher );
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::Rotate(
    btQuaternion const& deltaRotation, bool const& execute )
{
    if( execute )
    {
        m_currentTransform.setRotation(
            deltaRotation * m_currentTransform.getRotation() );
        m_motionState->setWorldTransform( m_currentTransform );
    }
    else
    {
        m_deltaRotation = deltaRotation * m_deltaRotation;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::SetCollisionShape( btCollisionShape* collisionShape )
{
    m_collisionShape = collisionShape;

    m_ghostObject->setCollisionShape( collisionShape );
    m_ghostObject->setCollisionFlags( btCollisionObject::CF_CHARACTER_OBJECT );
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::SetMotionState( btMotionState* const motionState )
{
    m_motionState = motionState;
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::Translate(
    btVector3 const& deltaTranslation, bool const& execute )
{
    if( execute )
    {
        m_currentPosition += deltaTranslation;
        m_motionState->setWorldTransform( m_currentTransform );
    }
    else
    {
        m_deltaTranslation += deltaTranslation;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::updateAction(
    btCollisionWorld*, btScalar deltaTime )
{
    //Test if we have delta translation or rotation
    bool translated( !m_deltaTranslation.isZero() );
    bool rotated( m_deltaRotation != btQuaternion::getIdentity() );
    if( ( !translated && !rotated ) || m_hitObject )
    {
        return;
    }

    //Update current and target positions
    m_targetTransform = m_currentTransform;
    m_targetPosition = m_currentPosition;
    m_normalizedDirection = GetNormalizedVector( m_deltaTranslation );

    m_currentAttitude = m_targetAttitude = m_currentTransform.getRotation();
    m_normalizedRotation = m_targetAttitude.normalized();

    //Move
    Move( deltaTime );

    //Set the motion state transform with the current ghost transform
    m_currentBasis.setRotation( m_currentAttitude );
    m_motionState->setWorldTransform( m_currentTransform );

    //Reset
    m_deltaTranslation.setZero();
    m_deltaRotation = btQuaternion::getIdentity();
}
////////////////////////////////////////////////////////////////////////////////
void GhostController::UpdateTargetPositionBasedOnCollision(
    btVector3 const& hitNormal, btScalar tangentMag, btScalar normalMag )
{
    btVector3 movementDirection = m_targetPosition - m_currentPosition;
    btScalar movementLength = movementDirection.length();
    if( movementLength > SIMD_EPSILON )
    {
        movementDirection.normalize();

        btVector3 reflectDir =
            ComputeReflectionDirection( movementDirection, hitNormal );
        reflectDir.normalize();

        btVector3 parallelDir, perpindicularDir;

        parallelDir = ParallelComponent( reflectDir, hitNormal );
        perpindicularDir = PerpindicularComponent( reflectDir, hitNormal );

        //m_targetPosition = m_currentPosition;
        if( tangentMag != 0.0 )
        {
            btVector3 parComponent =
                parallelDir * btScalar( tangentMag * movementLength );
            //m_targetPosition +=  parComponent;
            m_currentPosition +=  parComponent;
        }

        if( normalMag != 0.0 )
        {
            btVector3 perpComponent =
                perpindicularDir * btScalar( normalMag * movementLength );
            //m_targetPosition += perpComponent;
            m_currentPosition += perpComponent;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////

} //end scenegraph
} //end xplorer
} //end ves
