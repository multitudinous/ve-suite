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
#include <ves/xplorer/scenegraph/HighlightNodeByNameVisitor.h>
#include <ves/xplorer/scenegraph/util/OpacityVisitor.h>

#include <iostream>
//#include <algorithm>
//#include <cctype>
#include <boost/algorithm/string/case_conv.hpp>

using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
HighlightNodeByNameVisitor::HighlightNodeByNameVisitor( osg::Node* node, std::string nodeName, bool addGlow, bool ignoreCase, osg::Vec4 glowColor )
        :
        NodeVisitor( TRAVERSE_ALL_CHILDREN ),
        mNodeName( nodeName ),
        mAddGlow( addGlow ),
        m_glowColor( glowColor ),
        m_ignoreCase( ignoreCase )
{
    if( m_ignoreCase )
    {
        boost::algorithm::to_lower( mNodeName );
        //std::transform( mNodeName.begin(), mNodeName.end(), 
        //    mNodeName.begin(), std::tolower);
    }
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

    if( m_ignoreCase )
    {
        boost::algorithm::to_lower( name );
        //std::transform( name.begin(), name.end(), 
        //    name.begin(), std::tolower);
    }

    if( mAddGlow )
    {
        size_t found = name.find( mNodeName );
        if( found != std::string::npos )
        {
            //std::cout << " changing parts " << name << " " 
            //    << mNodeName.size() << " " << mNodeName << std::endl;
            foundNode = true;
            osg::ref_ptr< osg::StateSet > geode_stateset = node.getOrCreateStateSet();

            //Now highlight the node
            ves::xplorer::scenegraph::util::OpacityVisitor 
                opVisitor( &node, false, false, 1.0f );

            //Add shader code to have code highlighted
            //osg::Vec4 enableGlow( 1.0, 0.0, 0.0, 1.0 );
            //m_glowColor = glowColor;
            geode_stateset->addUniform( new osg::Uniform( "glowColor", m_glowColor ) );
            //geode_stateset->addUniform( new osg::Uniform( "gloColor", m_glowColor ) );
            //osg::StateSet::UniformList uniList = geode_stateset->getUniformList();
            m_foundNodes.push_back( &node );
        }
    }
    else
    {
        osg::ref_ptr< osg::StateSet > geode_stateset = node.getOrCreateStateSet();
        osg::StateSet::UniformList uniList = geode_stateset->getUniformList();
        if( uniList.size() )
        {
            geode_stateset->removeUniform( "glowColor" );
        }
    }

    //If we did not find an id and therefore a parent then keep going up
    if( !foundNode )
    {
        osg::NodeVisitor::traverse( node );
    }
}
////////////////////////////////////////////////////////////////////////////////
std::vector< osg::ref_ptr< osg::Node > > HighlightNodeByNameVisitor::GetFoundNodes()
{
    return m_foundNodes;
}
////////////////////////////////////////////////////////////////////////////////
