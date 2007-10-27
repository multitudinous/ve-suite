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
// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
FindParentsVisitor::FindParentsVisitor( osg::Node* node )
:
NodeVisitor( TRAVERSE_PARENTS ),
parentNode( 0 )
{
    node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
FindParentsVisitor::~FindParentsVisitor()
{
    parentNode = 0;
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* FindParentsVisitor::GetParentNode()
{
    return parentNode.get();
}
////////////////////////////////////////////////////////////////////////////////
std::string FindParentsVisitor::GetNodeGUID()
{
    return modelGUID;
}
////////////////////////////////////////////////////////////////////////////////
void FindParentsVisitor::apply( osg::Node& node )
{ 
    osg::Node::DescriptionList descriptorsList;
    descriptorsList = node.getDescriptions();
    
    //Find the parent node and the id of this particular node
    for( size_t i = 0; i < descriptorsList.size(); i++ )
    {
        if( descriptorsList.at( i ) == "VE_XML_ID" )
        {
            modelGUID = descriptorsList.at( i + 1 );
            parentNode = &node;

            break;
        }
    }
    
    //If we did not find an id and therefore a parent then keep going up
    if( !parentNode )
    {
        osg::NodeVisitor::traverse( node );
    }
}
////////////////////////////////////////////////////////////////////////////////
