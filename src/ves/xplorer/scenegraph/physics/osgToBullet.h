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
 * Date modified: $Date: 2007-10-27 18:39:25 -0500 (Sat, 27 Oct 2007) $
 * Version:       $Rev: 9742 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: osgToBullet.h 9742 2007-10-27 23:39:25Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef OSG_TO_BULLET_H
#define OSG_TO_BULLET_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/NodeVisitor>
#include <osg/BoundingBox>
#include <osg/BoundingSphere>

// --- Bullet Includes --- //
class btTriangleMesh;

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
/*!\file osgToBullet.h
 *
 */

/*!\class osgToBullet
 * 
 */
class VE_SCENEGRAPH_EXPORTS osgToBullet : public osg::NodeVisitor
{
public:
    osgToBullet( osg::Node* node );
    virtual ~osgToBullet();

    ///Override NodeVisitor apply function for geode
    ///\param geode A child geode w/in the node being traversed
    virtual void apply( osg::Geode& geode );

    btTriangleMesh* GetTriangleMesh();
    osg::BoundingBox GetBoundingBox();
    osg::BoundingSphere GetBoundingSphere();

private:
    btTriangleMesh* m_triangleMesh;///<The triangle mesh for physics

    osg::BoundingBox m_boundingBox;///<Bounding box of the osg node
    osg::BoundingSphere m_boundingSphere;///<Bounding sphere of the osg node

};

} // end scenegraph
} // end xplorer
} // end ves

#endif //OSG_TO_BULLET_H
