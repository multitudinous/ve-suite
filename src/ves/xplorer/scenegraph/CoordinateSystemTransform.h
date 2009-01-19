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

#ifndef COORDINATE_SYSTEM_TRANSFORM_H
#define COORDINATE_SYSTEM_TRANSFORM_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/NodeVisitor>

// --- VR Juggler Includes --- //
#include <gmtl/Matrix.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

/*!\file CoordinateSystemTransform.h
 *
 */

/*!\class CoordinateSystemTransform
 *
 */

class VE_SCENEGRAPH_EXPORTS CoordinateSystemTransform : public osg::NodeVisitor
{
public:

    ///Constructor
    ///\param stopNode The node with coordinate system to transform to
    ///\param startNode The local node to transform
    ///\param includeCameraTransform Specifies transformation into eye space
    CoordinateSystemTransform(
        osg::Node* stopNode,
        osg::Node* startNode,
        bool includeCameraTransform = false );

    ///Apply this NodeVisitor to a node
    ///\param node The node to run the traversal on
    virtual void apply( osg::Node& node );

    ///Get the desired transformation matrix
    ///\param includeLocalTransform Specifies inclusion of local transform
    ///\return The coordinate transformation matrix
    const gmtl::Matrix44d& GetTransformationMatrix(
        bool includeLocalTransform = true ) const;

protected:

    ///Destructor
    virtual ~CoordinateSystemTransform();

private:

    ///Do we want to transform into eye space
    bool mIncludeCameraTransform;

    ///The node to stop traversal at
    osg::ref_ptr< osg::Node > mStopNode;

    ///Matrix containing the cummulative transforms
    ///This matrix includes the local transform
    gmtl::Matrix44d mStartToStopMatrix;

    ///Matrix containing the cummulative transforms
    ///This matrix does not include the local transform
    gmtl::Matrix44d mStartToStopMatrixWithoutLocal;

};
} //end scenegraph
} //end xplorer
} //end ves

#endif //COORDINATE_SYSTEM_TRANSFORM_H
