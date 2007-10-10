/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include "VE_Xplorer/SceneGraph/Clone.h"
#include "VE_Xplorer/SceneGraph/Group.h"

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Geode>
#include <osg/CopyOp>
#include <osg/MatrixTransform>
#elif _OPENSG
#endif

// --- C/C++ Libraries --- //
#include <typeinfo>
#include <iostream>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
Clone::Clone( void )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
Clone::Clone( osg::Node* original )
{
   CloneNode( original );
}
////////////////////////////////////////////////////////////////////////////////
Clone::~Clone( void )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void Clone::CloneNode( osg::Node* original )
{
   if( !m_cloneTransform.valid() )
   {
	   m_cloneTransform = new VE_SceneGraph::DCS();
   }
   
   //Deep copy nodes so that picking is accurate and so that physics will work properly in the future
   if( dynamic_cast< VE_SceneGraph::DCS* >( original ) )
   {
      m_cloneTransform = new VE_SceneGraph::DCS( *static_cast< VE_SceneGraph::DCS* >( original ), osg::CopyOp::DEEP_COPY_NODES );
   }
   else if( dynamic_cast< osg::Geode* >( original ) )
   {
      m_cloneTransform->addChild( new osg::Geode( *static_cast< osg::Geode* >( original ), osg::CopyOp::DEEP_COPY_NODES ) );
   }
   else
   {
      std::cout << "ERROR : Cast not present " << std::endl;
      std::cout << typeid( *original ).name() << std::endl;
   }
}
////////////////////////////////////////////////////////////////////////////////
void Clone::SetTranslationArray( double* translation )
{
   if( m_cloneTransform.valid() )
   {
      m_cloneTransform->SetTranslationArray( translation );
   }
}
////////////////////////////////////////////////////////////////////////////////
void Clone::SetRotationArray( double* rotation )
{
   if( m_cloneTransform.valid() )
   {
      m_cloneTransform->SetRotationArray( rotation );
   }
}
////////////////////////////////////////////////////////////////////////////////
void Clone::SetScaleArray( double* scale )
{
   if( m_cloneTransform.valid() )
   {
      m_cloneTransform->SetScaleArray( scale );
   }
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* Clone::GetClonedGraph( void )
{
   if( m_cloneTransform.valid() )
   {
      return m_cloneTransform.get();
   }

   return 0;
}
////////////////////////////////////////////////////////////////////////////////
