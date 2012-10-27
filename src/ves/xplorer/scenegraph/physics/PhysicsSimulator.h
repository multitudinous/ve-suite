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

#ifndef VES_XPLORER_SCENEGRAPH_PHYSICSSIMULATOR_H
#define VES_XPLORER_SCENEGRAPH_PHYSICSSIMULATOR_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- VR Juggler Includes --- //
#include <vpr/Util/Singleton.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Vec3>

// --- STL Includes --- //
#include <vector>
#include <set>

#include <osgbDynamics/TripleBuffer.h>

namespace osg
{
class Transform;
class Node;
class Group;
}

class btDynamicsWorld;
class btDefaultCollisionConfiguration;
class btCollisionDispatcher;
class btBroadphaseInterface;
class btSequentialImpulseConstraintSolver;
class btRigidBody;
class btCollisionShape;
class btTransform;

// --- osgBullet Includes --- //
namespace osgbDynamics
{
class PhysicsThread;
class MotionState;
typedef std::set< osgbDynamics::MotionState* > MotionStateList;
}

namespace osgbCollision
{
class GLDebugDrawer;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class CADEntity;

/*!\file PhysicsSimulator.h
 * \class ves::xplorer::scenegraph::PhysicsSimulator
 * \namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS PhysicsSimulator
{
public:
    ///Acts as the destructor
    void ExitPhysics();

    ///Update the physics simulation by dt
    ///\param dt The time passed since last calculations
    void UpdatePhysics( float dt );

    ///Update the physics simulation by one time step ( 1/60 for now )
    void StepSimulation();

    ///Reset the physics simulation
    void ResetScene();

    ///Set the debug mode to use with the bullet debugger
    void SetDebugMode( int mode );

    ///Set debugging data to be rendered
    ///\param toggle Either true or false
    void SetDebuggingOn( bool toggle );

    ///Set whether physics is idle or not
    ///\param state State on or idle
    void SetIdle( bool state );

    ///Set the shoot speed
    ///\param speed
    void SetShootSpeed( float speed );

    ///Get whether physics is idle or not
    bool GetIdle();

    ///Get the bullet debug mode
    int GetDebugMode();

    ///Adds a rigid body to the physics simulator
    ///\param mass The mass of the rigid body
    ///\param startTransform The initial transform of the rigid body
    ///\param shape The collision shape of the rigid body
    btRigidBody* CreateRigidBody(
        float mass,
        const btTransform& startTransform,
        btCollisionShape* shape );

    ///Returns the dynamics world
    btDynamicsWorld* GetDynamicsWorld() const;

    ///Create flat ground plane for the world
    void CreateGroundPlane();

    ///Register a motionstate for the triple buffer to enable threaded physics
    void RegisterMotionState( osgbDynamics::MotionState* motionState );

    ///Register a motionstate for the triple buffer to enable threaded physics
    void UnregisterMotionState( osgbDynamics::MotionState* motionState );

private:
    ///Base constructor
    PhysicsSimulator();

    ///Destructor - never gets called, don't implement
    ~PhysicsSimulator();

    ///VPR singleton header
    vprSingletonHeader( PhysicsSimulator );

    ///Sets up the dynamics, collision algorithms, and solvers
    void InitializePhysicsSimulation();

    ///Create OSG rep for the ground
    ///\param size Size of the ground x, y, z
    ///\return The transform node for this ground
    osg::Transform* CreateOSGBox( osg::Vec3 size );

    ///Create the ground plane osg rep
    ///\param w Width of the plane
    ///\param h Hieght of the plane
    ///\param center x,y,z center of the plane
    ///\return OSG node for the plane
    osg::Node* CreateGround( float w, float h, const osg::Vec3& center );

    ///Determines whether the physics simulation is idle or not
    bool mIdle;

    ///Is the ground plane created
    bool mCreatedGroundPlane;

    ///Debug bullet
    bool mDebugBulletFlag;

    ///The debug level for bullet physics
    int mDebugMode;

    ///Speed of shooting boxes
    float shoot_speed;

    ///Implements dynamics - basic, discrete, parallel, and continuous
    btDynamicsWorld* mDynamicsWorld;

    ///
    btDefaultCollisionConfiguration* mCollisionConfiguration;

    ///Creates/Registers default collision algorithms
    ///for convex, compound and concave shape support
    btCollisionDispatcher* mDispatcher;

    ///Maintains objects with overlapping AABB
    btBroadphaseInterface* mBroadphase;

    ///A physics solver which sequentially applies impulses
    btSequentialImpulseConstraintSolver* mSolver;

    ///THe debug drawer for the bullet physics simulator
    osgbCollision::GLDebugDrawer* m_debugDrawer;

    ///A group node to better control when debug info is displayed for
    ///the osgBullet::GLDebugDrawer
    osg::ref_ptr< osg::Group > m_debugDrawerGroup;

    ///Tools for running the physics simulation on a seperate thread
    osgbDynamics::TripleBuffer m_tripleDataBuffer;

    ///List of every motion state created
    osgbDynamics::MotionStateList m_motionStateList;

    ///Physics thread for running solver
    osgbDynamics::PhysicsThread* m_physicsThread;

};
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_PHYSICSSIMULATOR_H
