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
#ifndef VE_PHYSICS_SIMULATOR_H
#define VE_PHYSICS_SIMULATOR_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class CADEntity;
}
}
}

// --- VR Juggler Includes --- //
#include <vpr/Util/Singleton.h>

#include <gadget/Type/PositionInterface.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Vec3>

namespace osg
{
class Transform;
class Node;
}
// --- Bullet Includes --- //
#include <LinearMath/btTransform.h>

class btDynamicsWorld;
class btDefaultCollisionConfiguration;
class btCollisionDispatcher;
class btBroadphaseInterface;
class btSequentialImpulseConstraintSolver;
class btRigidBody;
class btCollisionShape;

// --- C/C++ Libraries --- //
#include <vector>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
/*!\file PhysicsSimulator.h
 *
 */

/*!\class ves::xplorer::scenegraph::PhysicsSimulator
 *
 */

/*!\namespace ves::xplorer::scenegraph
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

    ///Shoot a box from the head position
    ///\param destination
    void ShootBox( const btVector3& destination );

    void SetDebugMode( int mode );

    ///Set whether physics is idle or not
    ///\param state State on or idle
    void SetIdle( bool state );

    void SetCollisionInformation( bool collisionInformation );

    ///Set the shoot speed
    ///\param speed
    void SetShootSpeed( float speed );

    ///Get whether physics is idle or not
    bool GetIdle();

    int GetDebugMode();

    ///Adds a rigid body to the physics simulator
    ///\param mass The mass of the rigid body
    ///\param startTransform The initial transform of the rigid body
    ///\param shape The collision shape of the rigid body
    btRigidBody* CreateRigidBody( float mass, const btTransform& startTransform, btCollisionShape* shape );

    ///Returns the dynamics world
    btDynamicsWorld* GetDynamicsWorld();
    
    ///Create flat ground plane for the world
    void CreateGroundPlane();

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
    
    int mDebugMode;///<The debug level for bullet physics

    ///Determines whether the physics simulation is idle or not
    bool mIdle;
    bool mCollisionInformation;
    ///Speed of shooting boxes
    float shoot_speed;
    ///Is the ground plane created
    bool mCreatedGroundPlane;
    
    gadget::PositionInterface head;///<The head in vr juggler

    std::vector< ves::xplorer::scenegraph::CADEntity* > mBoxVector;///<

    btDynamicsWorld* mDynamicsWorld;///<Implements dynamics - basic, discrete, parallel, and continuous

    btDefaultCollisionConfiguration* mCollisionConfiguration;
    btCollisionDispatcher* mDispatcher;///<Creates/Registers default collision algorithms, for convex, compound and concave shape support
    btBroadphaseInterface* mBroadphase;///<Maintains objects with overlapping AABB
    btSequentialImpulseConstraintSolver* mSolver;///<A physics solver which sequentially applies impulses
};
}
}
}

#endif //PHYSICS_SIMULATOR_H

