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
#include <ves/xplorer/scenegraph/util/RescaleTextureVisitor.h>

// --- OSG Stuff --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Image>
#include <osg/Texture>
#include <osg/TexEnv>
#include <osg/Array>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
RescaleTextureVisitor::RescaleTextureVisitor( osg::Node* osg_node )
        :NodeVisitor( TRAVERSE_ALL_CHILDREN )
{
    osg_node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
RescaleTextureVisitor::~RescaleTextureVisitor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void RescaleTextureVisitor::apply(osg::Node& node)
{
    
    osg::StateSet* ss = node.getStateSet();
    if( ss )
    {
        apply(*ss);
    }
    
    traverse(node);
}
////////////////////////////////////////////////////////////////////////////////
void RescaleTextureVisitor::apply(osg::Geode& geode)
{    
    osg::StateSet* ss = geode.getStateSet();
    
    if( ss )
    {
        apply(*ss);
    }
    
    for( unsigned int i=0;i<geode.getNumDrawables();++i)
    {
        osg::Drawable* drawable = geode.getDrawable(i);
        if( drawable )
        {
            ss = drawable->getStateSet();
            if( ss )
            {
                apply(*ss);
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void RescaleTextureVisitor::apply(osg::StateSet& stateset)
{
    for( size_t i=0; i < stateset.getTextureAttributeList().size(); ++i )
    {
        osg::StateAttribute* sa = stateset.getTextureAttribute(i,osg::StateAttribute::TEXTURE);
        osg::Texture* texture = dynamic_cast<osg::Texture*>(sa);
        if( texture )
        {
            apply(*texture);
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void RescaleTextureVisitor::apply(osg::Texture& texture)
{
        for( unsigned int i=0; i<texture.getNumImages(); ++i)
        {
            texture.getImage(i)->ensureValidSizeForTexturing( 134217728 );
        }
}
////////////////////////////////////////////////////////////////////////////////
