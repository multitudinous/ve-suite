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
#include <ves/xplorer/scenegraph/util/NormalizeVisitor.h>

// --- OSG Stuff --- //
#include <osg/Geode>
#include <osg/Group>
#include <osg/Geometry>
#include <osg/Material>
#include <osg/Texture>
#include <osg/TexEnv>
#include <osg/Array>
#include <osg/BlendFunc>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
NormalizeVisitor::NormalizeVisitor( osg::Node* osg_node, bool normalize )
        :NodeVisitor( TRAVERSE_ALL_CHILDREN ),
         mNormalize( normalize )
{
    osg_node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
NormalizeVisitor::~NormalizeVisitor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void NormalizeVisitor::apply( osg::Group& node )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList = node.getDescriptions();
    bool isPart = false;
    //Find if the node is an assembly
    for( size_t i = 0; i < descriptorsList.size(); i++ )
    {
        if( descriptorsList.at( i ) == "Part" )
        {
            isPart = true;
            break;
        }
    }

    ///Only process if it is a part
    if( isPart )
    {
        osg::ref_ptr< osg::StateSet > stateset = node.getOrCreateStateSet();
        SetupNormalizeForStateSet( stateset.get() );
    }

    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////
void NormalizeVisitor::SetupNormalizeForStateSet( osg::StateSet* stateset )
{
    ///Do this so that the normals will not be affected by the 
    ///scaling applied by the user - See osg post on April 19, 2007
    if( mNormalize )
    {
        stateset->setMode( GL_NORMALIZE, osg::StateAttribute::ON );
    }
    else
    {
        stateset->setMode( GL_NORMALIZE, osg::StateAttribute::OFF );
    }
}

////////////////////////////////////////////////////////////////////////////////
