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
#include "cfdGeode.h"

#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkVectorText.h>
#include <vtkProperty.h>

#include <vpr/Util/Debug.h>

cfd1DTextInput::cfd1DTextInput( void ):cfdDCS()
{
   this->geode = new cfdGeode();
   ((cfdDCS*)this)->AddChild( this->geode );

   // Do we need to initialize this->text here?
   // If this->text is required, it should probably be a constructor argument
}

cfd1DTextInput::~cfd1DTextInput( void )
{
   vprDEBUG(vprDBG_ALL,2) << "cfd1DTextInput Destructor: doing nothing" 
                          << std::endl << vprDEBUG_FLUSH;
}

cfdDCS* cfd1DTextInput::getpfDCS( void )
{
   return (cfdDCS*)this;
}

void cfd1DTextInput::SetFilename( std::string text )
{
   this->text = text;
   vprDEBUG(vprDBG_ALL,1) << "\tcfd1DTextInput : " << this->text
                          << std::endl << vprDEBUG_FLUSH;
}

void cfd1DTextInput::SetTransforms( float scale[ 3 ],
                                    float trans[ 3 ],
                                    float rot[ 3 ] )
{
   this->SetScaleArray( scale );
   this->SetTranslationArray( trans );
   this->SetRotationArray( rot );
}

void cfd1DTextInput::Update( void )
{
   vtkVectorText* labelScalar = vtkVectorText::New();   
   labelScalar->SetText( this->text.c_str() );

   vtkPolyDataMapper *labelMapper = vtkPolyDataMapper::New();
   labelMapper->SetInput( labelScalar->GetOutput() );

   vtkActor * actor = vtkActor::New();
   actor->SetMapper( labelMapper );
   actor->GetProperty()->SetSpecularPower( 20.0f );

   // set text color to green
   double color[ 3 ];
   color[ 0 ] = color[ 2 ] = 0;
   color[ 1 ] = 1;
   actor->GetProperty()->SetColor( color );

   this->geode->TranslateTocfdGeode( actor );

   actor->Delete();
   labelMapper->Delete();
   labelScalar->Delete();
}

