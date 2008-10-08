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
// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/HighlightNodeByNameVisitor.h>
#include <ves/xplorer/scenegraph/util/OpacityVisitor.h>

#include <iostream>

using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
HighlightNodeByNameVisitor::HighlightNodeByNameVisitor( osg::Node* node, std::string nodeName, bool addGlow )
        :
        NodeVisitor( TRAVERSE_ALL_CHILDREN ),
        mNodeName( nodeName ),
        mAddGlow( addGlow )
{
    node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
HighlightNodeByNameVisitor::HighlightNodeByNameVisitor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HighlightNodeByNameVisitor::apply( osg::Node& node )
{
    std::string name = node.getName();
    bool foundNode = false;

    if( mAddGlow )
    {
        if( !name.compare( 0, mNodeName.size(), mNodeName ) )
        {
            std::cout << " changing parts " << name << " " 
                << mNodeName.size() << " " << mNodeName << std::endl;
            foundNode = true;
            osg::ref_ptr< osg::StateSet > geode_stateset = node.getOrCreateStateSet();

            //Now highlight the node
            ves::xplorer::scenegraph::util::OpacityVisitor 
                opVisitor( &node, false, false, 1.0f );

            //Add shader code to have code highlighted
            osg::Vec4 glowColor( 1.0, 0.0, 0.0, 1.0 );
            geode_stateset->addUniform( new osg::Uniform( "glowColor", glowColor ) );
            osg::StateSet::UniformList uniList = geode_stateset->getUniformList();
        }
    }
    else
    {
        osg::ref_ptr< osg::StateSet > geode_stateset = node.getOrCreateStateSet();
        osg::StateSet::UniformList uniList = geode_stateset->getUniformList();
        if( uniList.size() )
        {
            geode_stateset->removeUniform( "glowColor" );
            std::cout << uniList.size() << std::endl;
        }
    }

    //If we did not find an id and therefore a parent then keep going up
    if( !foundNode )
    {
        osg::NodeVisitor::traverse( node );
    }
}
////////////////////////////////////////////////////////////////////////////////
