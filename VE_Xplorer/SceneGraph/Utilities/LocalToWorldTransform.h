/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2007-03-23 12:23:48 -0500 (Fri, 23 Mar 2007) $
 * Version:       $Rev: 7205 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: LocalToWorldTransform.h 7205 2007-03-23 17:23:48Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef LOCAL_TO_WORLD_TRANSFORM_H
#define LOCAL_TO_WORLD_TRANSFORM_H

/*!\file LocalToWorldTransform.h
*/

/*!\class LocalToWorldTransform
* 
*/

// --- VE-Suite Includes --- //
#include "VE_Installer/include/VEConfig.h"

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/NodeVisitor>

#include <gmtl/Matrix.h>

namespace VE_SceneGraph
{
namespace Utilities
{
class VE_SCENEGRAPH_UTILS_EXPORTS LocalToWorldTransform : public osg::NodeVisitor
{
public:
    LocalToWorldTransform( osg::PositionAttitudeTransform* worldNode, osg::PositionAttitudeTransform* localNode );
    virtual ~LocalToWorldTransform();

    virtual void apply( osg::PositionAttitudeTransform& pat );

    gmtl::Matrix44d& GetLocalToWorldTransform();

private:
    osg::ref_ptr< osg::PositionAttitudeTransform > m_worldNode;
    osg::ref_ptr< osg::PositionAttitudeTransform > m_localNode;
    gmtl::Matrix44d m_localToWorldTransform;

};
}
}

#endif //LOCAL_TO_WORLD_TRANSFORM_H
