/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2007-04-10 19:18:51 -0500 (Tue, 10 Apr 2007) $
 * Version:       $Rev: 7298 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: MaterialInitializer.cxx 7298 2007-04-11 00:18:51Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/Utilities/MaterialInitializer.h"

// --- OSG Stuff --- //
#include <osg/Node>
#include <osg/Geode>
#include <osg/Group>
#include <osg/Material>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace VE_SceneGraph::Utilities;

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
void MaterialInitializer::apply( osg::Geode& node )
{
   osg::ref_ptr< osg::Material > sa = static_cast< osg::Material* >( node.getOrCreateStateSet()->getAttribute( osg::StateAttribute::MATERIAL ) );

   if( sa.valid() )
   {
      return;
   }

   else
   {
	   for( unsigned int i = 0; i < node.getNumDrawables(); i++ )
	   {
         node.getDrawable( i )->getOrCreateStateSet()->setAttribute( new osg::Material, osg::StateAttribute::ON ) ;
	   }
   }
}
////////////////////////////////////////////////////////////////////////////////    
void MaterialInitializer::apply( osg::Group& node )
{
   osg::ref_ptr< osg::Material > sa = static_cast< osg::Material* >( node.getOrCreateStateSet()->getAttribute( osg::StateAttribute::MATERIAL ) );

   if( sa.valid() )
   {
      return;
   }

   else
   {
      osg::NodeVisitor::apply( (osg::Node&)node );
   }
}
////////////////////////////////////////////////////////////////////////////////
