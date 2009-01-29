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
#include <ves/xplorer/scenegraph/util/TogglePluginVisitor.h>

// --- OSG Stuff --- //
#include <osg/Geode>
#include <osg/Group>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
TogglePluginVisitor::TogglePluginVisitor( osg::Node* osg_node, bool toggle, 
    const std::string& nodeID )
    :
    NodeVisitor( TRAVERSE_ALL_CHILDREN ),
    mNodeID( nodeID ),
    mToggle( toggle )
{
    //This enables the visitor to traverse "off" nodes
    mToggle = false;
    setNodeMaskOverride( 1 );
    osg_node->accept( *this );

    //Because we have hierarchy blocks we now need to enable all the parents
    //of the found node
    if( mNodeID != "ALL" )
    {
        for( size_t i = 0; i < mNodePath.size(); ++i )
        {
            mNodePath.at( i )->setNodeMask( 1 );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
TogglePluginVisitor::~TogglePluginVisitor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TogglePluginVisitor::apply( osg::Group& node )
{    
    //Lets see if the node is a plugin base
    osg::Node::DescriptionList descriptorsList;
    descriptorsList = node.getDescriptions();
    bool isBasePlugin = false;
    bool isActiveNode = false;
    for( size_t i = 0; i < descriptorsList.size(); i++ )
    {
        //Node may not be a plugin base class
        if( descriptorsList.at( i ) == "VE_XPLORER_PLUGIN_ID" )
        {
            isBasePlugin = true;
            //If the id is the plugin we are after
            if( mNodeID ==  descriptorsList.at( i+1 ) )
            {
                isActiveNode = true;
            }
            break;
        }
    }
    //Only process if it is a plugin
    if( isBasePlugin )
    {
        if( isActiveNode )
        {
            node.setNodeMask( 1 );
            mNodePath = _nodePath;
            mToggle = true;
        }
        else
        {
            node.setNodeMask( 0 );
        }
        //If we are turning all nodes back on
        if( mNodeID == "ALL" )
        {
            node.setNodeMask( 1 );
        }
        //This enables the visitor to traverse "off" nodes
        setNodeMaskOverride( 1 );
    }

    osg::NodeVisitor::traverse( node );

    if( !isBasePlugin )
    {
        return;
    }

    //To enable the objects below a hierarchy block we must know that its
    //parent is toggled on and toggle this one on as well
    if( mToggle )
    {
        node.setNodeMask( 1 );
    }

    //Once we reach to the active node on the way back up 
    //turn the toggle flag false
    if( isActiveNode )
    {
        mToggle = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
