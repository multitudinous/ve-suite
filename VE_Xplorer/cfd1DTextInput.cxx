/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfd1DTextInput.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfd1DTextInput.h"
#include "vtkActorToPF.h"

#include <Performer/pf/pfDCS.h>

#include <vtkGeometryFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkVectorText.h>
#include <vtkProperty.h>

#include <vpr/Util/Debug.h>

#include <string>

cfd1DTextInput::cfd1DTextInput( void )
{
   //DCS = new pfDCS();
   geode = new pfGeode();
   ((pfDCS*)this->GetPfDCS())->addChild( geode );
}
/*
cfd1DTextInput::cfd1DTextInput( cfd1DTextInput* x )
{
   this->DCS = x->DCS;
   this->text = x->text;

   for( int i = 0; i < 3; i++ )
   {
      this->scale[ i ] = x->scale[ i ];
      this->trans[ i ] = x->trans[ i ];
      this->rot[ i ]   = x->rot[ i ];
   }
}
*/

cfd1DTextInput::~cfd1DTextInput( void )
{
   vprDEBUG(vprDBG_ALL,2) << "cfd1DTextInput Destructor" 
                          << std::endl << vprDEBUG_FLUSH;
/*   if ( this->DCS != NULL )
   {
      pfDelete( this->DCS );
   }*/
}

pfDCS* cfd1DTextInput::getpfDCS( void )
{
   //((pfDCS*)this->GetPfDCS())->addChild( geode );
   //DCS->setScale( scale[0], scale[1], scale[2] );
   //DCS->setTrans( trans[0], trans[1], trans[2] );
   //DCS->setRot( rot[0], rot[1], rot[2] );
   return (pfDCS*)this->GetPfDCS();
}

void cfd1DTextInput::SetFilename( std::string text )
{
   this->text = text;
   vprDEBUG(vprDBG_ALL,1) << "\tcfd1DTextInput : " << this->text
                          << std::endl << vprDEBUG_FLUSH;
   //cout << "\tcfd1DTextInput : " << this->text
   //                       << std::endl;
}

void cfd1DTextInput::SetTransforms( float scale[ 3 ], float trans[ 3 ], float rot[ 3 ] )
{
/*   for( int i = 0; i < 3; i++ )
   {
      this->scale[ i ] = scale[ i ];
      this->trans[ i ] = trans[ i ];
      this->rot[ i ]   = rot[ i ];
   }*/
   this->SetTranslationArray( trans );
   this->SetRotationArray( rot );
   this->SetScaleArray( scale );
}

void cfd1DTextInput::Update( void )
{
   vtkVectorText* labelScalar       = vtkVectorText::New();   
   vtkPolyDataMapper *labelMapper   = vtkPolyDataMapper::New();
   actor                            = vtkActor::New();

   labelScalar->SetText( text.c_str() );
   labelMapper->SetInput( labelScalar->GetOutput() );
   actor->SetMapper( labelMapper );
   this->actor->GetProperty()->SetSpecularPower( 20.0f );
   double color[ 3 ];
   color[ 0 ] = color[ 2 ] = 0;
   color[ 1 ] = 1;
   this->actor->GetProperty()->SetColor( color );

   vtkActorToPF( actor, geode, 0 );

   actor->Delete();
   labelMapper->Delete();
   labelScalar->Delete();
}

