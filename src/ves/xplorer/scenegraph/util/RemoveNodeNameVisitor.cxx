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
// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/util/RemoveNodeNameVisitor.h>

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/regex.hpp>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
RemoveNodeNameVisitor::RemoveNodeNameVisitor( osg::Node* node, std::string prefix, std::string postfix )
    :
    NodeVisitor( TRAVERSE_ALL_CHILDREN )
{
    m_prefix = "a_";
    m_postfix = " #1";
    
    node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
RemoveNodeNameVisitor::RemoveNodeNameVisitor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void RemoveNodeNameVisitor::apply( osg::Node& node )
{
    std::string name = node.getName();

    if( name == "body" )
    {
        name.erase();
    }

    boost::algorithm::erase_regex( name, boost::regex( "^body\\ \\#[0-9]+" ) );

    //if( boost::algorithm::starts_with( name, "body #" ) )
    //{
    //    name.erase();
    //}

    if( !name.empty() )
    {
        boost::algorithm::erase_regex( name, boost::regex( "^a_" ) );

        //Remove all of the #1, #2 suffixes
        boost::algorithm::erase_regex( name, boost::regex( "\\ \\#[0-9]+$" ) );
    }

    node.setName( name );

    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////////////
std::vector< osg::ref_ptr< osg::Node > > RemoveNodeNameVisitor::GetFoundNodes()
{
    return m_foundNodes;
}
////////////////////////////////////////////////////////////////////////////////
