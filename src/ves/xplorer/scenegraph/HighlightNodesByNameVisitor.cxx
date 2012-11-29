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
#include <ves/xplorer/scenegraph/HighlightNodesByNameVisitor.h>
#include <ves/xplorer/scenegraph/util/TransparencySupport.h>

#include <iostream>
//#include <algorithm>
//#include <cctype>
#include <boost/algorithm/string/case_conv.hpp>

using namespace ves::xplorer::scenegraph;
//using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
HighlightNodesByNameVisitor::HighlightNodesByNameVisitor( osg::Node* node, std::set< std::string > nodeNames, bool addGlow, bool ignoreCase, osg::Vec3 glowColor )
    :
    NodeVisitor( TRAVERSE_ALL_CHILDREN ),
    m_nodeNames( nodeNames ),
    mAddGlow( addGlow ),
    m_glowColor( glowColor ),
    m_ignoreCase( ignoreCase )
{
    if( m_ignoreCase )
    {
        m_nodeNames.clear();
        for( std::set< std::string >::const_iterator iter = nodeNames.begin(); iter != nodeNames.end(); ++iter )
        {
            //std::string nodeName = *iter;
            //boost::algorithm::to_lower_copy( *iter );
            m_nodeNames.insert( boost::algorithm::to_lower_copy( *iter ) );
        }
        //std::transform( mNodeName.begin(), mNodeName.end(),
        //    mNodeName.begin(), std::tolower);
    }
    node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
HighlightNodesByNameVisitor::HighlightNodesByNameVisitor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HighlightNodesByNameVisitor::apply( osg::Node& node )
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
        std::set< std::string >::const_iterator iter = m_nodeNames.find( name );
        
        if( iter != m_nodeNames.end() )
        {
            //size_t found = name.find( mNodeName );
            //if( found != std::string::npos )
            {
                foundNode = true;
                osg::ref_ptr< osg::StateSet > geode_stateset = node.getOrCreateStateSet();
                //I think we need to check and see if the stateset has any parents
                if( geode_stateset->getNumParents() > 1 )
                {
                    //std::cout << drawable_stateset->getNumParents() << std::endl;
                    //std::cout << "StateSet is shared." << std::endl;
                    osg::ref_ptr< osg::StateSet > temp_stateset =
                    new osg::StateSet( *( geode_stateset.get() ), osg::CopyOp::DEEP_COPY_ALL );
                    node.setStateSet( temp_stateset.get() );
                }
                
                //Now highlight the node
                transparentDisable( &node );
                
                //Add shader code to have code highlighted
                //osg::Vec3 enableGlow( 1.0, 0.0, 0.0 );
                geode_stateset->addUniform( new osg::Uniform( "glowColor", m_glowColor ) );
                m_foundNodes.push_back( &node );
            }
        }
    }
    else
    {
        osg::ref_ptr< osg::StateSet > geode_stateset = node.getStateSet();
        if( geode_stateset.valid() )
        {
            osg::StateSet::UniformList uniList =
                geode_stateset->getUniformList();
            if( uniList.size() )
            {
                geode_stateset->removeUniform( "glowColor" );
            }
        }
    }

    //If we did not find the name and therefore a part then keep going down
    if( !foundNode )
    {
        osg::NodeVisitor::traverse( node );
    }
}
////////////////////////////////////////////////////////////////////////////////
std::vector< osg::ref_ptr< osg::Node > > HighlightNodesByNameVisitor::GetFoundNodes()
{
    return m_foundNodes;
}
////////////////////////////////////////////////////////////////////////////////
