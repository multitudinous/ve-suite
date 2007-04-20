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
 * Id:            $Id: OpacityVisitor.cxx 7298 2007-04-11 00:18:51Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/Utilities/OpacityVisitor.h"

// --- OSG Stuff --- //
#include <osg/Geode>
#include <osg/Group>
#include <osg/Geometry>
#include <osg/Material>
#include <osg/Array>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace VE_SceneGraph::Utilities;

////////////////////////////////////////////////////////////////////////////////
OpacityVisitor::OpacityVisitor( osg::Node* osg_node, bool state )
:
NodeVisitor( TRAVERSE_ALL_CHILDREN )
{
   transparent = state;

	osg_node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
OpacityVisitor::~OpacityVisitor()
{
   ;
}

////////////////////////////////////////////////////////////////////////////////
void OpacityVisitor::apply( osg::Geode& node )
{
   osg::ref_ptr< osg::Material > material = static_cast< osg::Material* >( node.getOrCreateStateSet()->getAttribute( osg::StateAttribute::MATERIAL ) );
	
   if( material.valid() )
   {
      if( transparent == true )
      {
         material->setAlpha( osg::Material::FRONT_AND_BACK, 0.3f );
      }

      else
      {
         material->setAlpha( osg::Material::FRONT_AND_BACK, 1.0f );
      }

      node.getStateSet()->setAttribute( material.get(), osg::StateAttribute::ON );
   }

   for( size_t i = 0; i < node.getNumDrawables(); i++ )
	{
      
      osg::ref_ptr< osg::Vec4Array > color_array = static_cast< osg::Vec4Array* >( node.getDrawable( i )->asGeometry()->getColorArray() );
      

      if( color_array.valid() )
      {
         for( size_t j = 0; j < color_array->size(); j++ )
         {
            if( transparent == true )
            {
               color_array->at( j ).a() = 0.3f;
            }

            else
            {
               color_array->at( j ).a() = 1.0f;
            }

            node.getDrawable( i )->asGeometry()->setColorArray( color_array.get() );
         }
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void OpacityVisitor::apply( osg::Group& node )
{
   osg::ref_ptr< osg::Material > material = static_cast< osg::Material* >( node.getOrCreateStateSet()->getAttribute( osg::StateAttribute::MATERIAL ) );

   if( material.valid() )
   {
      if( transparent == true )
      {
         material->setAlpha( osg::Material::FRONT_AND_BACK, 0.3f );
      }

      else
      {
         material->setAlpha( osg::Material::FRONT_AND_BACK, 1.0f );
      }

      node.getStateSet()->setAttribute( material.get(), osg::StateAttribute::ON ) ;
   }

   osg::NodeVisitor::apply( (osg::Node&)node );
}
////////////////////////////////////////////////////////////////////////////////
