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
// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/util/FindChildWithNameVisitor.h>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
FindChildWithNameVisitor::FindChildWithNameVisitor( osg::Node* node, 
    const std::string& nodeName, bool exactNameMatch )
    :
    NodeVisitor( TRAVERSE_ALL_CHILDREN ),
    parentNode( 0 ),
    mParentName( nodeName ),
    m_foundMatch( false ),
    m_exactNameMatch( exactNameMatch )
{
    node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
FindChildWithNameVisitor::~FindChildWithNameVisitor()
{
    parentNode = 0;
}
////////////////////////////////////////////////////////////////////////////////
bool FindChildWithNameVisitor::FoundChild()
{
    return m_foundMatch;
}
////////////////////////////////////////////////////////////////////////////////
void FindChildWithNameVisitor::apply( osg::Node& node )
{
    if( m_exactNameMatch )
    {
        //Find the parent node with the specified name
        if( node.getName() == mParentName )
        {
            m_foundMatch = true;
            parentNode = &node;
            return;
        }
    }
    else
    {
        std::string name = node.getName();
        size_t found = name.find( mParentName );
        if( found != std::string::npos )
        //if( !name.compare( 0, mNodeName.size(), mNodeName ) )
        {
            m_foundMatch = true;
            parentNode = &node;
            return;
        }
    }

    //If we did not find an id and therefore a parent then keep going up
    if( !m_foundMatch )
    {
        osg::NodeVisitor::traverse( node );
    }
}
