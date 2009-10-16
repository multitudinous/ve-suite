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
#include <ves/xplorer/scenegraph/util/ToggleNodesVisitor.h>

// --- OSG Stuff --- //
#include <osg/Geode>
#include <osg/Group>

// --- C/C++ Libraries --- //
#include <iostream>

#include <boost/algorithm/string/case_conv.hpp>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
ToggleNodesVisitor::ToggleNodesVisitor( osg::Node* osg_node, bool toggle, 
    std::vector< std::string > nodeID )
    :
    NodeVisitor( TRAVERSE_ALL_CHILDREN ),
    m_nodeIDs( nodeID ),
    mToggle( toggle )
{
    //This enables the visitor to traverse "off" nodes
    setNodeMaskOverride( 1 );
    osg_node->accept( *this );

    for( NodePathIter iter = m_nodePaths.begin(); iter != m_nodePaths.end(); ++iter )
    {
        osg::NodePath tempPath = iter->second;
        for( size_t i = 0; i < tempPath.size(); ++i )
        {
            tempPath.at( i )->setNodeMask( 1 );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
ToggleNodesVisitor::~ToggleNodesVisitor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ToggleNodesVisitor::apply( osg::Node& node )
{   
    if( mToggle )
    {
        node.setNodeMask( 1 );
        osg::NodeVisitor::traverse( node );
        return;
    }

    node.setNodeMask( 0 );

    std::string name;
    std::string nodeName = node.getName();
    boost::algorithm::to_lower( nodeName );

    bool foundNode = false;
    for( size_t i = 0; i < m_nodeIDs.size(); ++i )
    {
        name = m_nodeIDs.at( i );
        size_t found = nodeName.find( name );
        if( found != std::string::npos )            
        //if( name == nodeName )
        {
            node.setNodeMask( 1 );
            m_nodePaths.insert( std::pair< std::string, osg::NodePath >( name, _nodePath ) );
            foundNode = true;
            break;
        }
    }

    if( !foundNode )
    {
        osg::NodeVisitor::traverse( node );
    }
}
////////////////////////////////////////////////////////////////////////////////
