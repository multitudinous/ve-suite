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
#ifndef HIGHLIGHT_NODE_BY_NAME_VISITOR_H
#define HIGHLIGHT_NODE_BY_NAME_VISITOR_H

/*!\file HighlightNodeByNameVisitor.h
*/

/*!\class ves::xplorer::scenegraph::HighlightNodeByNameVisitor
*
*/

/*!\namespace ves::xplorer::scenegraph
*
*/

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/NodeVisitor>
#include <osg/Group>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class VE_SCENEGRAPH_EXPORTS HighlightNodeByNameVisitor : public osg::NodeVisitor
{
public:
    ///Constructor
    ///\param node The node to be traversed
    ///\param nodeName The name of the node to highlight
    ///\param opaqueParent The hack to get around osg transparency issues
    HighlightNodeByNameVisitor( osg::Node* node, std::string nodeName, bool addGlow = true );
    
    ///Default Constructor
    HighlightNodeByNameVisitor();

    ///Destructor
    virtual ~HighlightNodeByNameVisitor(){ ; }

    ///Apply function that gets called during the traversal
    ///\param node A parent node of the node being traversed
    virtual void apply( osg::Node& node );

private:
    ///The name of the node to highlight
    std::string mNodeName;
    ///Fond node
    osg::ref_ptr< osg::Group > mOpaqueParent;
    ///Add the glow to parts, if false then remove all glow
    bool mAddGlow;
};
}
}
}

#endif //HIGHLIGHT_NODE_BY_NAME_VISITOR_H
