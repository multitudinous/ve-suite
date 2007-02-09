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
#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/Clone.h"
#ifdef _OSG
#include <osg/MatrixTransform>
#include <osg/CopyOp>
//#include "VE_SceneGraph/cfdMaterial.h"
#elif _PERFORMER
#elif _OPENSG
#endif
using namespace VE_SceneGraph;
////////////////////
Clone::Clone()
{
   ;
}
/////////////////////////////////////
Clone::Clone( SceneNode* original )
{
   CloneNode( original );
}
/////////////////////
Clone::~Clone()
{
   ;
}
///////////////////////////////////////////
void Clone::CloneNode( SceneNode* original )
{
   if ( !cloneTransform.valid() )
   {
      cloneTransform = new DCS();
   }
   
   DCS* assemblyNode = dynamic_cast< DCS* >( original );
   if ( assemblyNode )
   {
      for ( int i =0; i < assemblyNode->GetNumChildren(); i++ )
      {
         cloneTransform->addChild( assemblyNode->GetChild( i ) );
      }

   }
   else
   {
      cloneTransform->addChild( dynamic_cast< osg::Node* >( original ) );
   }
}
/////////////////////////////////////////////////
void Clone::SetTranslationArray( float* translation )
{
   if ( cloneTransform.valid() )
   {
      cloneTransform->SetTranslationArray( translation );
   }
}
/////////////////////////////////////////////////
void Clone::SetRotationArray( float* rotation )
{
   if ( cloneTransform.valid() )
   {
      cloneTransform->SetRotationArray(rotation);
   }
}
/////////////////////////////////////////////////
void Clone::SetScaleArray(float* scale)
{
   if ( cloneTransform.valid() )
   {
      cloneTransform->SetScaleArray( scale );
   }
}
//////////////////////////////////
DCS* Clone::GetClonedGraph()
{
   if ( cloneTransform.valid() )
   {
      return cloneTransform.get();
   }
   return 0;
}
