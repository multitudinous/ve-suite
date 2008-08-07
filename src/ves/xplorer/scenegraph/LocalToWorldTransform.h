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

#ifndef LOCAL_TO_WORLD_TRANSFORM_H
#define LOCAL_TO_WORLD_TRANSFORM_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/NodeVisitor>

#include <gmtl/Matrix.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
/*!\file LocalToWorldTransform.h
 */

/*!\class LocalToWorldTransform
 *
 */
class VE_SCENEGRAPH_EXPORTS LocalToWorldTransform : public osg::NodeVisitor
{
public:
    ///Constructor
    ///\param stopNode The node with coordinate system to transform to
    ///\param startNode The local node to transform
    LocalToWorldTransform( osg::Node* stopNode, osg::Node* startNode );

    ///
    virtual void apply( osg::Node& node );

    ///Return by reference
    ///\param includeLocalTransform Specifies if the local transform
    /// should be included in the transformation matrix
    ///\return Get the desired transformation matrix
    const gmtl::Matrix44d& GetLocalToWorldTransform(
        bool includeLocalTransform = true ) const;

protected:
    ///Destructor
    virtual ~LocalToWorldTransform();

private:
    ///
    osg::ref_ptr< osg::Node > mStopNode;

    ///Matrix containing the cummulative transforms from local to world space
    ///This matrix includes the local transform
    gmtl::Matrix44d mLocalToWorldMatrix;
    ///Matrix containing the cummulative transforms from local to world space
    ///This matrix does not include the local transform
    gmtl::Matrix44d mLocalParentToWorldMatrix;
    

};
} //end scenegraph
} //end xplorer
} //end ves

#endif //LOCAL_TO_WORLD_TRANSFORM_H
