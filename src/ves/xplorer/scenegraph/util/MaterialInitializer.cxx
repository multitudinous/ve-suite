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
#include <ves/xplorer/scenegraph/util/MaterialInitializer.h>

// --- OSG Stuff --- //
#include <osg/Geode>
#include <osg/Group>
#include <osg/Material>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
MaterialInitializer::MaterialInitializer( osg::Node* osg_node )
        :
        NodeVisitor( TRAVERSE_ALL_CHILDREN )
{
    osg_node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
MaterialInitializer::~MaterialInitializer()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MaterialInitializer::apply( osg::Group& node )
{
    osg::ref_ptr< osg::StateSet > stateset = node.getOrCreateStateSet();
    osg::ref_ptr< osg::Material > material = static_cast< osg::Material* >( stateset->getAttribute( osg::StateAttribute::MATERIAL ) );

    if( material.valid() )
    {
        return;
    }

    else
    {
        material = new osg::Material();
        material->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.56862f, 0.56842f, 0.56842f, 1.0f ) );

        stateset->setAttribute( material.get(), osg::StateAttribute::ON );
    }
}
////////////////////////////////////////////////////////////////////////////////
