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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/Clone.h"

#include "VE_Xplorer/SceneGraph/Group.h"
#ifdef _OSG
#include <osg/MatrixTransform>
#include <osg/CopyOp>
#include <osg/Geode>
//#include "VE_SceneGraph/cfdMaterial.h"
#elif _PERFORMER
#elif _OPENSG
#endif

#include <typeinfo>
#include <iostream>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
Clone::Clone()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
/*Clone::Clone( SceneNode* original )
{
   CloneNode( original );
}*/
////////////////////////////////////////////////////////////////////////////////
Clone::Clone( osg::Node* original )
{
   CloneNode( original );
}
////////////////////////////////////////////////////////////////////////////////
Clone::~Clone()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void Clone::CloneNode( osg::Node* original )
{
   if ( !cloneTransform.valid() )
   {
		cloneTransform = new VE_SceneGraph::DCS();
   }
   
   if ( dynamic_cast< VE_SceneGraph::DCS* >( original ) )
   {
      cloneTransform = new VE_SceneGraph::DCS( *static_cast< VE_SceneGraph::DCS* >( original ) );
      /*osg::PositionAttitudeTransform* assemblyNode = dynamic_cast< osg::PositionAttitudeTransform* >( original );
      for ( unsigned int i =0; i < assemblyNode->getNumChildren(); i++ )
      {
         cloneTransform->addChild( assemblyNode->getChild( i ) );
      }*/
   }
   if ( dynamic_cast< VE_SceneGraph::Group* >( original ) )
   {
      cloneTransform->addChild( new VE_SceneGraph::Group( *static_cast< VE_SceneGraph::Group* >( original ) ) );
   }
   else if (  dynamic_cast< osg::Geode* >( original ) )
   {
      cloneTransform->addChild( new osg::Geode( *static_cast< osg::Geode* >( original ) ) );
   }
   else if (  dynamic_cast< osg::Group* >( original ) )
   {
      cloneTransform->addChild( new osg::Group( *static_cast< osg::Group* >( original ) ) );
   }
   else
   {
      std::cout << "ERROR : Cast not present " << std::endl;
      std::cout << typeid( *original ).name() << std::endl;
      //cloneTransform->addChild( original );
   }
}
///////////////////////////////////////////////////////////////////
void Clone::SetTranslationArray( float* translation )
{
   if ( cloneTransform.valid() )
   {
      cloneTransform->SetTranslationArray( translation );
   }
}
////////////////////////////////////////////////////////////////////////////////
void Clone::SetRotationArray( float* rotation )
{
   if ( cloneTransform.valid() )
   {
      cloneTransform->SetRotationArray(rotation);
   }
}
////////////////////////////////////////////////////////////////////////////////
void Clone::SetScaleArray(float* scale)
{
   if ( cloneTransform.valid() )
   {
      cloneTransform->SetScaleArray( scale );
   }
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* Clone::GetClonedGraph()
{
   if ( cloneTransform.valid() )
   {
      return cloneTransform.get();
   }
   return 0;
}
////////////////////////////////////////////////////////////////////////////////
