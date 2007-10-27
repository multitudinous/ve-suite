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

/*!\file PhysicsRigidBody.h
*/

/*!\class ves::xplorer::scenegraph::PhysicsRigidBody
* 
*/

/*!\namespace ves::xplorer::scenegraph
*
*/

// --- VE-Suite Stuff --- //
#include <ves/VEConfig.h>

// --- OSG Stuff --- //
#include <osg/ref_ptr>
#include <osg/NodeVisitor>
#include <osg/BoundingBox>

// --- Bullet Stuff --- //
#include <BulletDynamics/Dynamics/btRigidBody.h>

class btTriangleMesh;
class btCollisionShape;

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class VE_SCENEGRAPH_EXPORTS PhysicsRigidBody : public btRigidBody, public osg::NodeVisitor
{
public:
    ///Constructor
    ///\param node The node to create a physics mesh from
    ///\param startTransform The beginning transform for the rigid body
    PhysicsRigidBody( osg::Node* node, const btTransform& startTransform = btTransform::getIdentity() );

    ///Destructor
    virtual ~PhysicsRigidBody();

    ///Override NodeVisitor apply function for geode
    ///\param geode A child geode w/in the node being traversed
    virtual void apply( osg::Geode& geode );

    ///Set the mass for the rigid body
    ///\param mass The mass value
    void SetMass( float mass );

    ///Creates a box shape from the osg::BoundingBox of the mesh shape
    void BoundingBoxShape();

    ///Creates a concave static-triangle mesh shape with Bounding Volume Hierarchy optimization
    void StaticConcaveShape();

    ///Creates a convex hull shape from a triangle mesh - mesh can be concave or convex
    void ConvexShape();

private:
    void SetMassProps();

    float m_mass;///<The mass of the rigid body

    osg::BoundingBox bb;///<Bounding box of the osg node

    btTriangleMesh* tri_mesh;///<The triangle mesh for the osg node

};
}
}
}

#endif //PHYSICS_RIGID_BODY_H
