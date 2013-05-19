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
#ifndef REMOVE_NODE_NAME_VISITOR_H
#define REMOVE_NODE_NAME_VISITOR_H

/*!\file RemoveNodeNameVisitor.h
 * \class ves::xplorer::scenegraph::util::RemoveNodeNameVisitor
 *
 */

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/NodeVisitor>
#include <osg/Group>
#include <osg/Vec4>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
class RemoveNodeNameVisitor : public osg::NodeVisitor
{
public:
    ///Constructor
    ///\param node The node to be traversed
    ///\param nodeName The name of the node to highlight
    ///\param opaqueParent The hack to get around osg transparency issues
    RemoveNodeNameVisitor( osg::Node* node, std::string prefix, std::string postfix );

    ///Default Constructor
    RemoveNodeNameVisitor();

    ///Destructor
    virtual ~RemoveNodeNameVisitor()
    {
        ;
    }

    ///Apply function that gets called during the traversal
    ///\param node A parent node of the node being traversed
    virtual void apply( osg::Node& node );

    ///Get the nodes that were found
    ///\return A vector of all the nodes found with this name
    std::vector< osg::ref_ptr< osg::Node > > GetFoundNodes();

private:
    ///The name of the node to highlight
    std::string mNodeName;
    ///Fond node
    std::vector< osg::ref_ptr< osg::Node > > m_foundNodes;
    ///Add the glow to parts, if false then remove all glow
    bool mAddGlow;
    ///The color to set the glow
    osg::Vec3 m_glowColor;
    ///Ignore the case of the node name
    bool m_ignoreCase;
    ///prefix to remove
    std::string m_prefix;
    ///postfix to remove
    std::string m_postfix;
    ///node names to remove
    std::string m_bodyName;
};
}
}
}
}
#endif //HIGHLIGHT_NODE_BY_NAME_VISITOR_H
