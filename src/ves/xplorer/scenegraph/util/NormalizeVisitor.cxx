/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <osg/PositionAttitudeTransform>
#include <osg/Geometry>
#include <osg/Material>
#include <osg/Texture>
#include <osg/TexEnv>
#include <osg/Array>
#include <osg/BlendFunc>
#include <osg/MatrixTransform>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::util;
//Here are the use cases for this visitor:
//parent node 
//supported in both cases
//scale == 1 but children are != 1
//supported in both cases
//scale != 1 and children are != 1
//supported in ves load but not real-time
//scale != 1 and children are == 1
//These can then be set in real-time or from the gui

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
void NormalizeVisitor::apply( osg::PositionAttitudeTransform& node )
{
    //NOTE: Remember that children nodes are processed first when
    //ves files are read and parent nodes are processed second
    //NOTE: When setting the scale on the gui the parent node or child
    //may only be touched. It is a single operation.
    osg::Node::DescriptionList descriptorsList;
    descriptorsList = node.getDescriptions();
    bool isPart = false;
    //Find if the node is an assembly
    for( size_t i = 0; i < descriptorsList.size(); i++ )
    {
        std::string nodeType = descriptorsList.at( i );
        if( nodeType  == "Part" )
        {
            isPart = true;
            break;
        }
        /*else if( nodeType == "Assembly" )
        {
            //If a parent assembly node has a scale of 1 and is trying to
            //normalize all the sub children BUT the children nodes have a
            //scale != 1 then we do not want to traverse any farther 
            //as normalization needs to be on still. If the children
            //nodes have a scale == 1 then no big deal, go ahead and 
            //turn off normalization
            //NOTE::This is neccessary due to how statesets are managed in OSG
            //if( !mNormalize && node.getScale()[ 0 ] != 1.0f )
            //{
            //    return;
            //}
        }*/
    }

    ///Only process if it is a part
    if( isPart )
    {
        osg::ref_ptr< osg::StateSet > stateset = node.getOrCreateStateSet();
        SetupNormalizeForStateSet( stateset.get(), &node );
    }

    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////
void NormalizeVisitor::SetupNormalizeForStateSet( osg::StateSet* stateset, 
    osg::Node* node )
{
    ///Do this so that the normals will not be affected by the 
    ///scaling applied by the user - See osg post on April 19, 2007
    if( mNormalize )
    {
        //This is a new setting that can be used when the scaling is 
        //uniform. In general we are going to use this because non-uniform
        //scaling does not occur frequently.
        //http://www.opengl.org/resources/features/KilgardTechniques/oglpitfall/
        //See osg post on July 3, 2008 by Paul Martz
        stateset->setMode( GL_RESCALE_NORMAL, osg::StateAttribute::ON );
    }
    else
    {
        //If the node path above this part node contains a scale anywhere then
        //the resulting matrix will have a scale value other than 1. In this 
        //case do not turn off normalization
        osg::NodePath nodePath = getNodePath();
        osg::Matrix m;
        if( nodePath.size() > 0 )
        { 
            m = osg::computeLocalToWorld( nodePath );
        }
        if( (m.getScale()[ 0 ] == 1.0f) && (m.getScale()[ 1 ] == 1.0f) && (m.getScale()[ 2 ] == 1.0f) )
        {
            stateset->setMode( GL_RESCALE_NORMAL, osg::StateAttribute::OFF );
        }
        else
        {
            //If the user has a group node with a scale and then the user
            //adds a file at run time the rescale of normals will not take
            //effect.
            stateset->setMode( GL_RESCALE_NORMAL, osg::StateAttribute::ON );
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
