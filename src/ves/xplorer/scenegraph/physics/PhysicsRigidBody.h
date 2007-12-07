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
#ifndef PHYSICS_RIGID_BODY_H
#define PHYSICS_RIGID_BODY_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/physics/osgToBullet.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
    class Node;
    class Geode;
}

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btRigidBody.h>

class btCompoundShape;
class btCollisionShape;

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class PhysicsSimulator;
class vesMotionState;
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
class VE_SCENEGRAPH_EXPORTS PhysicsRigidBody : public btRigidBody
{
public:
    ///Constructor
    ///\param node The node to create a physics mesh from
    ///\param physicsSimulator Sets a pointer to the PhysicsSimulator singleton
    PhysicsRigidBody( osg::Node* node,
                      PhysicsSimulator* physicsSimulator );

    ///Destructor
    virtual ~PhysicsRigidBody();

    ///Set the mass for the rigid body
    ///\param mass The mass value
    void SetMass( float mass );

    ///Creates a box shape from the osg::BoundingBox of the mesh shape
    void BoundingBoxShape();

    ///Creates a sphere shape from the osg::BoundingSphere of the mesh shape
    void SphereShape( double radius = 0 );

    ///Creates a concave static-triangle mesh shape with Bounding Volume Hierarchy optimization
    void StaticConcaveShape();

    ///Creates a convex hull shape from a triangle mesh - mesh can be concave or convex
    void ConvexShape();

private:
    void SetMassProps();

    float m_mass;///<The mass of the rigid body
    float m_friction;///<The friction of the rigid body
    float m_restitution;///<The restitution of the rigid body

    PhysicsSimulator* m_physicsSimulator;///<A pointer to the PhysicsSimulator singleton

    vesMotionState* m_vesMotionState;

    osg::ref_ptr< osgToBullet > m_osgToBullet;

};
}
}
}

#endif //PHYSICS_RIGID_BODY_H
