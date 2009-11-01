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
#ifndef VES_PHYSICS_RIGID_BODY_H
#define VES_PHYSICS_RIGID_BODY_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

//#include <ves/xplorer/scenegraph/physics/osgToBullet.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Node>

namespace osg
{
//class Node;
class Geode;
}

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btRigidBody.h>
#include <BulletCollision/BroadphaseCollision/btBroadphaseProxy.h>
#include <LinearMath/btVector3.h>

class btCompoundShape;
class btCollisionShape;

namespace osgbBullet
{
class MotionState;
}
// --- C/C++ Libraries --- //
#include <map>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class PhysicsSimulator;
//class vesMotionState;
class osgToBullet;

/*!\file PhysicsRigidBody.h
 *
 */
/*!\class ves::xplorer::scenegraph::PhysicsRigidBody
 *
 */
/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS PhysicsRigidBody // : public btRigidBody
{
public:
    ///Constructor
    ///\param node The node to create a physics mesh from
    ///\param physicsSimulator Sets a pointer to the PhysicsSimulator singleton
    PhysicsRigidBody( osg::Node* node,
                      PhysicsSimulator* physicsSimulator );

    ///Destructor
    ~PhysicsRigidBody();

    ///Creates a box shape from the osg::BoundingBox of the mesh shape
    void BoundingBoxShape();

    bool CollisionInquiry( PhysicsRigidBody* physicsRigidBody );

    ///Creates a convex hull shape from a triangle mesh - mesh can be concave or convex
    void ConvexShape();

    const std::multimap< PhysicsRigidBody*, btVector3 >& GetCollisions();

    bool HasCollisions();
    bool IsStoringCollisions();

    ///Set the mass for the rigid body
    ///\param mass The mass value
    void SetMass( float mass );
    ///Set the mass for the rigid body
    ///\param mass The mass value
    void SetFriction( float friction );
    ///Set the mass for the rigid body
    ///\param mass The mass value
    void SetRestitution( float restitution );
    ///Create a rigid body with new enums
    ///\param lod Can be Overall or Compound
    ///\param motion Can be Dynamic or Static
    ///\param mesh Cane be Box, Sphere, Cylinder, Mesh
    void CreateRigidBody( const std::string& lod, const std::string& motion, const std::string& mesh, const std::string& decimation = std::string( "Exact" ) );
    
    void SetStoreCollisions( bool storeCollisions );

    ///Creates a sphere shape from the osg::BoundingSphere of the mesh shape
    void SphereShape( double radius = 0 );

    ///Creates a concave static-triangle mesh shape with Bounding Volume Hierarchy optimization
    void StaticConcaveShape();

    void UserDefinedShape( btCollisionShape* collisionShape );

    btRigidBody* GetbtRigidBody();
    
private:
    friend class PhysicsSimulator;
    ///Clean up the memory associated with collisions vector
    void ClearCollisions();
    ///Clean up the memory associated with a btRigidBody
    void CleanRigidBody();

    void PushBackCollision( PhysicsRigidBody* physicsRigidBody, const btVector3& location );
    ///Modify the mass, friction, restitution, and inertia for the rigidbody
    void SetMassProps( bool dynamic = true );
    ///Create a btRigidBody with different shape types
    void CustomShape( const BroadphaseNativeTypes shapeType, const bool overall, const std::string& decimation = std::string( "Exact" ) );
    ///Register the rigid body with the ves engine and perform other uniform
    ///operations on the rigidbody
    ///\param rigidBody The btRigidBody to register with ves
    void RegisterRigidBody( btRigidBody* rigidBody );
    ///Store bodies currently in collision with this body, yes or no
    bool mStoreCollisions;
    ///The mass of the rigid body
    float mMass;
    ///The mass of the rigid body
    float mFriction;
    ///The mass of the rigid body
    float mRestitution;
    ///A pointer to the PhysicsSimulator singleton
    PhysicsSimulator* mPhysicsSimulator;
    ///Tell wether we need to register osgBullets debug capability
    bool mDebugBoundaries;
    ///The holder of all physics data for bullet
    btRigidBody* mRB;

    osg::ref_ptr< osg::Node > mOSGToBullet;

    std::multimap< PhysicsRigidBody*, btVector3 > mCollisions;

};

} // end scenegraph
} // end xplorer
} // end ves

#endif //VES_PHYSICS_RIGID_BODY_H
