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

// --- VES Includes --- //
#include <ves/xplorer/scenegraph/Select.h>

// --- OSG Includes --- //
#include <osg/Node>

#include <osgUtil/IntersectionVisitor>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

////////////////////////////////////////////////////////////////////////////////
osgUtil::LineSegmentIntersector::Intersections& TestForIntersections(
    osgUtil::LineSegmentIntersector& intersector,
    osg::Node& root,
    unsigned int traversalMask )
{
    osgUtil::IntersectionVisitor intersectionVisitor( &intersector );
    intersectionVisitor.setTraversalMask( traversalMask );
    root.accept( intersectionVisitor );

    return intersector.getIntersections();
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* FindVESObject( osg::NodePath& nodePath )
{
    osg::NodePath::reverse_iterator itr = nodePath.rbegin();
    for( itr; itr != nodePath.rend(); ++itr )
    {
        osg::Node::DescriptionList descList = (*itr)->getDescriptions();
        for( size_t i = 0; i < descList.size(); ++i )
        {
            if( descList.at( i ) == "VE_XML_ID" )
            {
                return *itr;
            }
        }
        nodePath.pop_back();
    }

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////

} //end scenegraph
} //end xplorer
} //end ves
